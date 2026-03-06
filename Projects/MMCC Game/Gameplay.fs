namespace MMCCGame
open System.Numerics
open Prime
open Nu
open ImGuiNET

type WaypointName = string

// this represents the state of gameplay simulation.
type [<SymbolicExpansion>] GameplayState =
    { Zone : Zone
      Player : AnimatedProp option }

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState =
            { Zone = PlayerApartment
              Player = None } }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial player =
        { Gameplay.empty with Gameplay.GameplayState.Player = Some player }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | Nil
    | UpdatePlayer
    | UpdateCamera
    | StartPlaying
    | FinishQuitting
    | TimeUpdate
    | UpdatePlayerTransform of BodyTransformData
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    | UpdatePlayerPhysics of Velocity: Vector3 * AngularVelocity: Vector3
    interface Command

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    let animationRate = 30f
    let blendRate = 0.05f
    let playerWalkSpeed = 0.1f
    let playerTurnSpeed = 0.05f

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.empty 
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.UpdateEvent => UpdatePlayer
         Screen.UpdateEvent => UpdateCamera
         Screen.SelectEvent => StartPlaying
         Screen.DeselectingEvent => FinishQuitting
         Screen.TimeUpdateEvent => TimeUpdate
         Simulants.GameplayPlayer.BodyTransformEvent =|> (fun x -> UpdatePlayerTransform x.Data)]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | Nil -> just gameplay
        | UpdateCamera ->
            match gameplay.GameplayState.Player with
            | None -> just gameplay
            | Some _ ->

            let player = Simulants.GameplayPlayer
            let entityRotation = player.GetRotation world
            let cameraRotation = entityRotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
            let position = player.GetPosition world
            do World.setEye3dCenter (position + v3Up * 1.40f - cameraRotation.Forward) world
            do World.setEye3dRotation cameraRotation world
            just gameplay

        | UpdatePlayerTransform data ->
            match gameplay.GameplayState.Player with
            | None -> just gameplay
            | Some prop ->

            let player = { prop with Position = data.BodyCenter; Rotation = data.BodyRotation }
            let gameplay = { gameplay with Gameplay.GameplayState.Player = Some player }

            just gameplay

        | UpdatePlayer ->
            match gameplay.GameplayState.Player with
            | None -> just gameplay
            | Some prop ->

            let cameraRotation = prop.Rotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)

            // current forward (from camera-adjusted rotation)
            let forward = cameraRotation.Forward

            // movement input
            let walkDirection =
                (if World.isKeyboardKeyDown KeyboardKey.W world then forward else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world then -forward else v3Zero)

            let turnInput =
                (if World.isKeyboardKeyDown KeyboardKey.D world then -1.0f else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then  1.0f else 0.0f)

            let turnVelocity = turnInput * playerTurnSpeed
            let turnVelocity = v3 0.0f turnVelocity 0.0f
            let walkVelocity = walkDirection * playerWalkSpeed

            // animation blending between Idle and Jog (character props only)
            let isMoving = walkDirection.LengthSquared() > 1e-6f || abs turnInput > 0.0f
            let animations =
                match prop.PropType with
                | CharacterProp character ->
                    Character.locomotionAnimations isMoving blendRate animationRate world.GameTime prop.Animations character

            // store computed values in model; := bindings will push them to the entity
            let prop = { prop with Animations = animations }
            let gameplayState = { gameplay.GameplayState with Player = Some prop }
            let gameplay = { gameplay with GameplayState = gameplayState }

            withSignal (UpdatePlayerPhysics (walkVelocity, turnVelocity)) gameplay

        | StartPlaying ->
            let startWaypoint = Simulants.GameplayScene / "Start"
            let position = startWaypoint.GetPosition world
            do World.setEye3dCenter position world
            let rotation = startWaypoint.GetRotation world
            let player = { PropType = CharacterProp Player; Position = position; Rotation = rotation; Animations = Array.empty; Morphs = Array.empty }
            let gameplay = Gameplay.initial player
            //do World.setBodyCenter position (Simulants.GameplayPlayer.GetBodyId world) world
            //do World.setBodyRotation rotation (Simulants.GameplayPlayer.GetBodyId world) world
            just gameplay

        | FinishQuitting ->
            let gameplay = Gameplay.empty
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (_, command, screen, world) =
        match command with
        | StartQuitting ->
            World.publish () screen.QuitEvent screen world
        | UpdatePlayerPhysics (walkVelocity, turnVelocity) ->
            do World.setBodyLinearVelocity walkVelocity (Simulants.GameplayPlayer.GetBodyId world) world
            do World.setBodyAngularVelocity turnVelocity (Simulants.GameplayPlayer.GetBodyId world) world

    // here we allow syncing entity state back into the model from the editor
    override this.Edit (gameplay, op, _, world) =
        match gameplay.GameplayState.Player, op with
        | Some player, AppendProperties _ ->
            let entity = Simulants.GameplayPlayer
            if not (ImGui.Button "Sync Player from Entity") then just gameplay else
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let player = { player with Position = position; Rotation = rotation }
            let gameplayState = { gameplay.GameplayState with Player = Some player }
            just { gameplay with GameplayState = gameplayState }
        | _ -> just gameplay

    // here we describe the content of the game including the scene and the hud
    override this.Content (gameplay, _) =
        let zonePath = Zone.toPath gameplay.GameplayState.Zone

        [ // the scene group while playing
          Content.groupFromFile Simulants.GameplayScene.Name zonePath []

              [// the player character
               match gameplay.GameplayState.Player with
               | None -> ()
               | Some prop ->
                    let propPath = PropType.toPath prop.PropType
                    Content.entityFromFile Simulants.GameplayPlayer.Name propPath
                        [Entity.Position := prop.Position
                         Entity.Rotation := prop.Rotation
                         Entity.PhysicsMotion == ManualMotion
                         Entity.Animations := prop.Animations
                         Entity.Morphs := prop.Morphs]

               // quit
               Content.button Simulants.GameplayQuit.Name
                  [Entity.Position == v3 232.0f -144.0f 0.0f
                   Entity.Text == "Quit"
                   Entity.ClickEvent => StartQuitting]]]
