namespace MyGame3
open System
open System.Numerics
open Prime
open Nu
open MyGame3

// this represents the state of gameplay simulation.
type [<SymbolicExpansion>] GameplayState =
    { Zone: Zone
      Avatar : CharacterProp option
      Actors :CharacterProp array
      Exposition: Exposition option
      Cue : Cue }

module GameplayState =
    let empty =
        { Zone = NoZone
          Avatar = None
          Actors = Array.empty
          Exposition = None
          Cue = Fin }

    let initial =
        { Zone = PlayerApartment
          Avatar = None
          Actors = Array.empty
          Exposition = None
          Cue = Fin }

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
    | ActorPhysicsUpdate of Character * BodyTransformData
    | AddActor of Character * Position:Vector3 * Rotation:Quaternion
    | AddActorAtStart of Character
    | RemoveActor of Character
    | ShowExposition of Text:string * ExpositVariant
    | AdvanceExposition
    | UpdateExposition
    | RunCue of Cue
    | StepCues
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
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.initial)

    let avatarWalkSpeed = 5.0f
    let avatarTurnSpeed = 3.0f
    let animationRate = 30f
    let blendRate = 0.05f

    // recursively process a cue until it blocks or finishes, following OmniBlade's pattern.
    // returns the updated cue and (signals, gameplay).
    let rec updateCue (cue: Cue) (gameplay: Gameplay) (world: World) : Cue * (Signal list * Gameplay) =
        match cue with
        | Fin ->
            (Fin, just gameplay)

        | Print text ->
            printfn "[Cue] %s" text
            (Fin, just gameplay)

        | Cue.AddActor (character, spawnPoint) ->
            let waypoint = Simulants.GameplayScene / spawnPoint
            let position = waypoint.GetPosition world
            let rotation = waypoint.GetRotation world
            let idle = Character.idle character
            let idle = Animation.make world.GameTime None idle Playback.Loop 30f 1.0f None
            let actor =
                { Character = character
                  Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Array.append gameplay.GameplayState.Actors [|actor|]
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            (Fin, just gameplay)

        | Cue.RemoveActor character ->
            let actors = Array.filter (fun a -> a.Character <> character) gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            (Fin, just gameplay)

        | Cue.Exposit (text, variant) ->
            let exposition = Exposition.make text variant
            let gameplay = { gameplay with Gameplay.GameplayState.Exposition = Some exposition }
            (Fin, just gameplay)

        | Wait duration ->
            let endTime = world.GameTime + GameTime.ofSeconds (double duration)
            (WaitState endTime, just gameplay)

        | WaitState endTime ->
            if world.GameTime >= endTime
            then (Fin, just gameplay)
            else (cue, just gameplay)

        | Sequence cues ->
            let stepSequence (halted, haltedCues, (signals: Signal FDeque, gameplay)) cue =
                if halted then (halted, FDeque.conj cue haltedCues, (signals, gameplay)) else
                let (cue, (signals2, gameplay)) = updateCue cue gameplay world
                let signals = List.fold (fun acc s -> FDeque.conj s acc) signals signals2
                if Cue.isFin cue
                then (false, FDeque.empty, (signals, gameplay))
                else (true, FDeque.conj cue FDeque.empty, (signals, gameplay))

            let (_, haltedCues, (signals, gameplay)) =
                FDeque.fold stepSequence (false, FDeque.empty, (FDeque.empty, gameplay)) cues

            let signals = Seq.toList signals
            if FDeque.isEmpty haltedCues
            then (Fin, (signals, gameplay))
            else (Sequence haltedCues, (signals, gameplay))

        | Parallel cues ->
            let stepParallel (remaining, (signals: Signal FDeque, gameplay)) cue =
                let (cue, (signals2, gameplay)) = updateCue cue gameplay world
                let signals = List.fold (fun acc s -> FDeque.conj s acc) signals signals2
                if Cue.isFin cue
                then (remaining, (signals, gameplay))
                else (FDeque.conj cue remaining, (signals, gameplay))

            let (remaining, (signals, gameplay)) =
                FDeque.fold stepParallel (FDeque.empty, (FDeque.empty, gameplay)) cues

            let signals = Seq.toList signals
            if FDeque.isEmpty remaining
            then (Fin, (signals, gameplay))
            else (Parallel remaining, (signals, gameplay))

    // here we define the screen's fallback model depending on whether screen is selected
    override this.GetFallbackModel (_, screen, world) =
        if screen.GetSelected world
        then Gameplay.initial
        else Gameplay.empty

    // here we define the screen's property values and event handling
    override this.Definitions (gameplay, _) =
        [Screen.SelectEvent => StartPlaying
         Screen.TimeUpdateEvent => TimeUpdate
         Screen.UpdateEvent => ProcessAvatarInput
         Screen.UpdateEvent => UpdateExposition
         Screen.UpdateEvent => StepCues
         Game.KeyboardKeyDownEvent =|> fun data ->
            if data.Data.KeyboardKey = KeyboardKey.E then AdvanceExposition else Nil
         Simulants.GameplayAvatar.BodyTransformEvent =|> (fun x -> AvatarPhysicsUpdate x.Data)
         for actor in gameplay.GameplayState.Actors do
            let name = Character.toName actor.Character
            let entity = Simulants.GameplayScene / name
            entity.BodyTransformEvent =|> fun evt -> ActorPhysicsUpdate (actor.Character, evt.Data)]

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

        | ActorPhysicsUpdate (character, data) ->
            let actors =
                gameplay.GameplayState.Actors |> Array.map (fun a ->
                    if a.Character = character
                    then { a with Position = data.BodyCenter; Rotation = data.BodyRotation }
                    else a)
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | TimeUpdate ->
            let gameDelta = world.GameDelta
            let gameplay = { gameplay with GameplayTime = gameplay.GameplayTime + gameDelta.Updates }
            just gameplay

        | ShowExposition (text, variant) ->
            let exposition = Exposition.make text variant
            let gameplay = { gameplay with Gameplay.GameplayState.Exposition = Some exposition }
            just gameplay

        | AddActorAtStart character ->
            let startWaypoint = Simulants.GameplayScene / "Start"
            let position = startWaypoint.GetPosition world
            let rotation = startWaypoint.GetRotation world
            let idle = Character.idle character
            let idle = Animation.make world.GameTime None idle Playback.Loop animationRate 1.0f None
            let actor =
                { Character = character
                  Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Array.append gameplay.GameplayState.Actors [|actor|]
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | AddActor (character, position, rotation) ->
            let idle = Character.idle character
            let idle = Animation.make world.GameTime None idle Playback.Loop animationRate 1.0f None
            let actor =
                { Character = character
                  Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Array.append gameplay.GameplayState.Actors [|actor|]
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | RemoveActor character ->
            let actors = Array.filter (fun a -> a.Character <> character) gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | AdvanceExposition ->
            match gameplay.GameplayState.Exposition with
            | None -> just gameplay
            | Some exposition ->
            let exposition = Exposition.advance exposition
            let gameplay = { gameplay with Gameplay.GameplayState.Exposition = Some exposition }
            just gameplay

        | UpdateExposition ->
            match gameplay.GameplayState.Exposition with
            | None -> just gameplay
            | Some exposition ->
            let exposition = Exposition.update world.GameDelta.SecondsF exposition
            let gameplay = { gameplay with Gameplay.GameplayState.Exposition = exposition }
            just gameplay

        | RunCue cue ->
            let gameplay = { gameplay with Gameplay.GameplayState.Cue = cue }
            just gameplay

        | StepCues ->
            let cue = gameplay.GameplayState.Cue
            if Cue.isFin cue then just gameplay
            else
                let (cue, (signals, gameplay)) = updateCue cue gameplay world
                let gameplay = { gameplay with Gameplay.GameplayState.Cue = cue }
                withSignals signals gameplay

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
             [// avatar
              match gameplay.GameplayState.Avatar with
              | None -> ()
              | Some avatarProp ->
              let path = Character.toPath avatarProp.Character
              Content.entityFromFile Simulants.GameplayAvatar.Name path
                  [Entity.PhysicsMotion == PhysicsMotion.ManualMotion
                   Entity.Position := avatarProp.Position
                   Entity.Rotation := avatarProp.Rotation
                   Entity.Animations := avatarProp.Animations
                   Entity.Morphs := avatarProp.Morphs]

              // actors
              for actor in gameplay.GameplayState.Actors do
                  let name = Character.toName actor.Character
                  let path = Character.toPath actor.Character
                  let character = actor.Character
                  Content.entityFromFile name path
                      [Entity.Position := actor.Position
                       Entity.Rotation := actor.Rotation
                       Entity.Animations := actor.Animations
                       Entity.PhysicsMotion == PhysicsMotion.ManualMotion
                       Entity.Morphs := actor.Morphs
                       Entity.BodyTransformEvent =|> fun evt -> ActorPhysicsUpdate (character, evt.Data)]]

         Content.group Simulants.GameplayGui.Name []
            [// exposition
             match gameplay.GameplayState.Exposition with
             | None -> ()
             | Some exposition ->
                 let pos = Exposition.position exposition
                 let baseSize = Exposition.size exposition
                 let width = baseSize.X * exposition.AppearProgress
                 let height = baseSize.Y * exposition.AppearProgress
                 let text =
                     match exposition.Phase with
                     | DisplayPhase  -> exposition.Text
                     | _ -> String.empty

                 match exposition.Variant with
                 | Thought ->
                     Content.text "Exposition"
                         [Entity.Position == pos.V3
                          Entity.Size := v3 width height 0f
                          Entity.Color == Color.DarkGray
                          Entity.TextColor == Color.White
                          Entity.Text := text]

                 | Dialogue ->
                     Content.text "Exposition"
                         [Entity.Position == pos.V3
                          Entity.Size := v3 width height 0f
                          Entity.TextColor == Color.Black
                          Entity.Text := text]

                 | PortraitDialogue portrait ->
                     Content.panel "Exposition"
                         [Entity.Position == pos.V3
                          Entity.Layout == Manual
                          Entity.Size := v3 width height 0f]
                         [match exposition.Phase with
                          | AppearPhase | DisappearPhase -> ()
                          | DisplayPhase ->
                             let portraitPath = ExpositionPortrait.toPath portrait
                             Content.staticSprite "Portrait"
                                 [Entity.PositionLocal == v3 -137f 3f 1f
                                  Entity.Size == v3 80f 64f 0f
                                  Entity.StaticImage == asset "Gameplay" portraitPath
                                  Entity.Elevation == 1f]
                          Content.text "Dialogue"
                             [Entity.BackdropImageOpt == None
                              Entity.Justification == Unjustified true
                              Entity.TextColor == Color.Black
                              Entity.PositionLocal == v3 40f 3f 0f
                              Entity.Size == v3 256f 64f 0f
                              Entity.Text := text]]

             // quit
             Content.button Simulants.GameplayQuit.Name
                [Entity.Position == v3 232.0f -144.0f 0.0f
                 Entity.Text == "Quit"
                 Entity.ClickEvent => StartQuitting]

             // test exposition
             Content.button "TestExposition"
                [Entity.Position == v3 232.0f -104.0f 0.0f
                 Entity.Text == "Test"
                 Entity.ClickEvent => ShowExposition ("Hello, world!", Thought)]

             // test actor
             Content.button "TestActor"
                [Entity.Position == v3 232.0f -64.0f 0.0f
                 Entity.Text == "Add Akane"
                 Entity.ClickEvent => AddActorAtStart Akane]

             // test cue
             Content.button "TestCue"
                [Entity.Position == v3 232.0f -24.0f 0.0f
                 Entity.Text == "Test Cue"
                 Entity.ClickEvent => RunCue (Sequence (FDeque.ofList
                    [Print "Cue started!"
                     Exposit ("A cue is running...", Thought)
                     Wait 3.0f
                     Cue.AddActor (Akane, "Start")
                     Print "Cue finished!"]))]]]
