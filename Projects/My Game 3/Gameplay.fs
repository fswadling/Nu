namespace MyGame3
open System
open System.Numerics
open Prime
open Nu
open MyGame3

// this represents the state of gameplay simulation.
type GameplayState =
    { Zone: Zone
      Avatar : CharacterProp option }

module GameplayState =
    let empty =
        { Zone = NoZone
          Avatar = None
               }

    let initial =
        { Zone = PlayerApartment
          Avatar = None }

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type Gameplay =
    { GameplayTime : int64
      GameplayState : GameplayState }

    // this represents the gameplay model in an unutilized state, such as when the gameplay screen is not selected.
    static member empty =
        { GameplayTime = 0L
          GameplayState = GameplayState.empty }

    // this represents the gameplay model in its initial state, such as when gameplay starts.
    static member initial =
        { Gameplay.empty with
            GameplayState = GameplayState.initial }

// this is our gameplay MMCC message type.
type GameplayMessage =
    | Nil
    | StartPlaying
    | TimeUpdate
    | AvatarPhysicsUpdate of BodyTransformData
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    | WarpAvatar of Position:Vector3 * Rotation:Quaternion
    | ProcessAvatarInput
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

    let avatarWalkSpeed = 5.0f
    let avatarTurnSpeed = 3.0f
    let animationRate = 30f
    let blendRate = 0.05f

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (_, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.TimeUpdateEvent => TimeUpdate
         Screen.UpdateEvent => ProcessAvatarInput
         Simulants.GameplayAvatar.BodyTransformEvent =|> (fun x -> AvatarPhysicsUpdate x.Data)]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | Nil -> just gameplay
        | StartPlaying ->
            let startWaypoint = Simulants.GameplayScene / "Start"
            let position = startWaypoint.GetPosition world
            let rotation = startWaypoint.GetRotation world
            let player =
                { Character = Player
                  Position = position
                  Rotation = rotation
                  Animations = Array.empty
                  Morphs = Array.empty }
            
            let gameplay = { gameplay with Gameplay.GameplayState.Avatar = Some player }
            withSignal (WarpAvatar (position, rotation)) gameplay

        | AvatarPhysicsUpdate data ->
            match gameplay.GameplayState.Avatar with
            | None -> just gameplay
            | Some avatar ->
            let avatar =
                { avatar with
                    Position = data.BodyCenter
                    Rotation = data.BodyRotation }
            let gameplay = { gameplay with Gameplay.GameplayState.Avatar = Some avatar }
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

    // here we handle the above commands
    override this.Command (gameplay, command, screen, world) =
        match command with
        | StartQuitting ->
            do World.publish () screen.QuitEvent screen world

        | WarpAvatar (position, rotation) ->
            let bodyId = Simulants.GameplayAvatar.GetBodyId world
            do World.setBodyCenter position bodyId world
            do World.setBodyRotation rotation bodyId world

        | ProcessAvatarInput ->
            match gameplay.GameplayState.Avatar with
            | None -> ()
            | Some avatar ->

            let bodyId = Simulants.GameplayAvatar.GetBodyId world
            let rotation = avatar.Rotation
            let cameraRotation = rotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
            let forward = cameraRotation.Forward

            // tank controls: W/S for forward/back, A/D for turning
            let walkDirection =
                (if World.isKeyboardKeyDown KeyboardKey.W world then forward else v3Zero) +
                (if World.isKeyboardKeyDown KeyboardKey.S world then -forward else v3Zero)

            let turnInput =
                (if World.isKeyboardKeyDown KeyboardKey.D world then -1.0f else 0.0f) +
                (if World.isKeyboardKeyDown KeyboardKey.A world then  1.0f else 0.0f)

            let walkVelocity = walkDirection * avatarWalkSpeed

            // compute rotation directly (angular velocity doesn't work for KinematicCharacter)
            let turnDelta = turnInput * avatarTurnSpeed * world.GameDelta.SecondsF
            let newRotation =
                if turnDelta <> 0.0f
                then Quaternion.Normalize (rotation * Quaternion.CreateFromAxisAngle (v3Up, turnDelta))
                else rotation

            // set velocities on the physics body, preserving Y velocity (gravity)
            let currentLinearVelocity = World.getBodyLinearVelocity bodyId world
            do World.setBodyLinearVelocity (walkVelocity.WithY 0.0f + currentLinearVelocity * v3Up) bodyId world
            do World.setBodyRotation newRotation bodyId world

            // update animations in the model
            let isMoving = walkDirection.LengthSquared() > 1e-6f || abs turnInput > 0.0f
            let animations = Character.locomotionAnimations isMoving blendRate animationRate world.GameTime avatar.Animations avatar.Character
            let avatar = { avatar with Animations = animations }
            let gameplayState = { gameplay.GameplayState with Avatar = Some avatar }
            do screen.SetGameplay { gameplay with GameplayState = gameplayState } world

            // camera follow
            do World.setEye3dCenter (avatar.Position + v3Up * 1.40f - cameraRotation.Forward) world
            do World.setEye3dRotation cameraRotation world


    // here we describe the content of the game including the scene and the hud
    override this.Content (gameplay, _) =
        let zonePath = Zone.toPath gameplay.GameplayState.Zone
        [// the zone group (non interactive environment - asset, lights etc)
         match zonePath with
         | None -> ()
         | Some zonePath ->
            Content.groupFromFile Simulants.GameplayScene.Name zonePath []
                [match gameplay.GameplayState.Avatar with
                 | None -> ()
                 | Some playerProp ->
                    let path = Character.toPath playerProp.Character
                    Content.entityFromFile Simulants.GameplayAvatar.Name path
                        [Entity.PhysicsMotion == PhysicsMotion.ManualMotion
                         Entity.Position := playerProp.Position
                         Entity.Rotation := playerProp.Rotation
                         Entity.Animations := playerProp.Animations
                         Entity.Morphs := playerProp.Morphs]]

         Content.group Simulants.GameplayGui.Name []
            // quit
            [Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]
