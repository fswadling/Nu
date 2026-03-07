namespace MyGame3
open System
open System.Numerics
open Prime
open Nu
open MyGame3

// this represents the state of gameplay simulation.
type GameplayState =
    { Zone: Zone
      Player : CharacterProp option }

module GameplayState =
    let empty =
        { Zone = NoZone
          Player = None }

    let initial =
        { Zone = PlayerApartment
          Player = None }

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
    | PlayerPhysicsUpdate of BodyTransformData
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    | SetEyeCenter of Vector3
    | MovePlayerForward
    | StopPlayerMovement
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
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (model, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.TimeUpdateEvent => TimeUpdate
         Game.KeyboardKeyChangeEvent =|> fun data ->
            match model.GameplayState.Player, data.Data with
            | Some _, { KeyboardKey = KeyboardKey.W; Down = true } -> MovePlayerForward
            | Some _, { KeyboardKey = KeyboardKey.W; Down = false } -> StopPlayerMovement
            | _ -> Nil
         Simulants.GameplayPlayer.BodyTransformEvent =|> (fun x -> PlayerPhysicsUpdate x.Data)]

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
            let gameplay = { gameplay with Gameplay.GameplayState.Player = Some player }
            withSignal (SetEyeCenter position) gameplay

        | PlayerPhysicsUpdate data ->
            let position = data.BodyCenter
            let rotation = data.BodyRotation
            let playerProp =
                { Character = Player
                  Position = position
                  Rotation = rotation
                  Animations = Array.empty
                  Morphs = Array.empty }
            let gameplay = { gameplay with Gameplay.GameplayState.Player = Some playerProp }
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
        | SetEyeCenter center ->
            World.setEye3dCenter center world
        | MovePlayerForward ->
            let player = Simulants.GameplayPlayer
            let bodyId = player.GetBodyId world
            let rotation = player.GetRotation world
            let forward = rotation.Forward
            let velocity = forward * 0.1f
            System.Console.WriteLine ("MovePlayerForward: " + velocity.ToString ())
            ()
            //World.setBodyLinearVelocity velocity bodyId world
        | StopPlayerMovement ->
            let player = Simulants.GameplayPlayer
            let bodyId = player.GetBodyId world
            System.Console.WriteLine ("StopPlayerMovement")
            ()
           // World.setBodyLinearVelocity v3Zero bodyId world


    // here we describe the content of the game including the scene and the hud
    override this.Content (gameplay, _) =
        let zonePath = Zone.toPath gameplay.GameplayState.Zone
        [// the zone group (non interactive environment - asset, lights etc)
         match zonePath with
         | None -> ()
         | Some zonePath ->
            Content.groupFromFile Simulants.GameplayScene.Name zonePath []
                [match gameplay.GameplayState.Player with
                 | None -> ()
                 | Some playerProp ->
                    let path = Character.toPath playerProp.Character
                    Content.entityFromFile Simulants.GameplayPlayer.Name path
                        [Entity.PhysicsMotion == PhysicsMotion.SynchronizedMotion
                         Entity.Position == playerProp.Position
                         Entity.Rotation == playerProp.Rotation
                         Entity.Animations := playerProp.Animations
                         Entity.Morphs := playerProp.Morphs]]

         Content.group Simulants.GameplayGui.Name []
            // quit
            [Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]]]
