namespace MyGame3
open System.Numerics
open Prime
open Nu
open MyGame3

type [<SymbolicExpansion>] AnimatedProp =
    { Position: Vector3
      Rotation: Quaternion
      Animations: Animation array
      Morphs : (int * single) array }

// this represents the state of gameplay simulation.
type [<SymbolicExpansion>] GameplayState =
    { Zone: Zone
      Avatar : (Character * AnimatedProp) option
      Actors : Map<Character, AnimatedProp>
      Exposition: Exposition option
      Cue : Cue
      Advents : Advent Set
      Fade : single
      AvatarMovementEnabled : bool
      CameraFollowEnabled : bool }

module GameplayState =
    let empty =
        { Zone = NoZone
          Avatar = None
          Actors = Map.empty
          Exposition = None
          Cue = Fin
          Advents = Set.empty
          Fade = 0.0f
          AvatarMovementEnabled = false
          CameraFollowEnabled = false }

    let initial =
        { Zone = PlayerApartment
          Avatar = None
          Actors = Map.empty
          Exposition = None
          Cue = Fin
          Advents = Set.empty
          Fade = 0.0f
          AvatarMovementEnabled = true
          CameraFollowEnabled = true }

// this is our MMCC model type representing gameplay.
// this model representation uses update time, that is, time based on number of engine updates.
type [<SymbolicExpansion>] Gameplay =
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
    | CameraFollow
    interface Message

// this is our gameplay MMCC command type.
type GameplayCommand =
    | StartQuitting
    | WarpAvatar of Position:Vector3 * Rotation:Quaternion
    | ProcessAvatarInput
    | SetCamera of Position:Vector3 * Rotation:Quaternion
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

    let updateAnimation (animationName: string) (newWeight: single) (rate: single) (gameTime: GameTime) (animations: Animation array) : Animation array =
        let animation = Array.tryFind (fun a -> a.Name = animationName) animations
        match animation, newWeight with
        | Some _, 0.0f ->
            Array.filter (fun a -> a.Name <> animationName) animations
        | Some animation, _ ->
            let animation = { animation with Weight = newWeight }
            animations |> Array.map (fun a -> if a.Name = animationName then animation else a)
        | None, 0.0f ->
            animations
        | None, _ ->
            let animation = Animation.make gameTime None animationName Playback.Loop rate newWeight None
            Array.add animation animations

    let updateMorph (newWeight: single) (morphIndex: int) (morphs: (int * single) array) =
        let hasMorph = Array.exists (fun (i, _) -> i = morphIndex) morphs
        match hasMorph, newWeight with
        | true, 0.0f ->
            Array.filter (fun (i, _) -> i <> morphIndex) morphs
        | true, _ ->
            Array.map (fun (i, weight) -> if i = morphIndex then (i, newWeight) else (i, weight)) morphs
        | false, 0.0f ->
            morphs
        | false, _ ->
            Array.add (morphIndex, newWeight) morphs

    let updateActorProp (character: Character) (f: AnimatedProp -> AnimatedProp) (gameplay: Gameplay) =
        let actors = gameplay.GameplayState.Actors |> Map.change character (Option.map f)
        { gameplay with Gameplay.GameplayState.Actors = actors }

    let getActorProp (character: Character) (gameplay: Gameplay) =
        Map.tryFind character gameplay.GameplayState.Actors

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
                { Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Map.add character actor gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            (Fin, just gameplay)

        | Cue.RemoveActor character ->
            let actors = Map.remove character gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            (Fin, just gameplay)

        | Cue.Exposit (text, variant) ->
            let exposition = Exposition.make text variant
            let gameplay = { gameplay with Gameplay.GameplayState.Exposition = Some exposition }
            (ExpositState, just gameplay)

        | ExpositState ->
            match gameplay.GameplayState.Exposition with
            | None -> (Fin, just gameplay)
            | Some _ -> (cue, just gameplay)

        | Cue.Animate (character, animationName, targetWeight, duration) ->
            let initialWeight =
                match getActorProp character gameplay with
                | Some actor ->
                    actor.Animations |>
                    Array.tryFind (fun a -> a.Name = animationName) |>
                    Option.map (fun a -> a.Weight) |>
                    Option.defaultValue 0.0f
                | None -> 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (AnimateState (character, animationName, initialWeight, targetWeight, startTime, endTime), just gameplay)

        | AnimateState (character, animationName, initialWeight, targetWeight, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = updateActorProp character (fun a -> { a with Animations = updateAnimation animationName targetWeight animationRate world.GameTime a.Animations }) gameplay
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentWeight = initialWeight + t * (targetWeight - initialWeight)
            let gameplay = updateActorProp character (fun a -> { a with Animations = updateAnimation animationName currentWeight animationRate world.GameTime a.Animations }) gameplay
            (cue, just gameplay)

        | Cue.CrossFade (character, fromAnimation, toAnimation, targetWeight, duration) ->
            let actor = getActorProp character gameplay
            let initialFromWeight =
                actor |>
                Option.map _.Animations |>
                Option.bind (Array.tryFind (fun (anim: Animation) -> anim.Name = fromAnimation)) |>
                Option.map _.Weight |> 
                Option.defaultValue 0.0f
            let initialToWeight =
                actor |>
                Option.map _.Animations |>
                Option.bind (Array.tryFind (fun (anim: Animation) -> anim.Name = toAnimation)) |>
                Option.map _.Weight |>
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (CrossFadeState (character, fromAnimation, toAnimation, initialFromWeight, initialToWeight, targetWeight, startTime, endTime), just gameplay)

        | CrossFadeState (character, fromAnimation, toAnimation, initialFromWeight, initialToWeight, targetWeight, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = updateActorProp character (fun a ->
                    let anims = updateAnimation fromAnimation 0.0f animationRate world.GameTime a.Animations
                    let anims = updateAnimation toAnimation targetWeight animationRate world.GameTime anims
                    { a with Animations = anims }) gameplay
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFromWeight = initialFromWeight + t * (0.0f - initialFromWeight)
            let currentToWeight = initialToWeight + t * (targetWeight - initialToWeight)
            let gameplay = updateActorProp character (fun a ->
                let anims = updateAnimation fromAnimation currentFromWeight animationRate world.GameTime a.Animations
                let anims = updateAnimation toAnimation currentToWeight animationRate world.GameTime anims
                { a with Animations = anims }) gameplay
            (cue, just gameplay)

        | Cue.Morph (character, morphIndex, targetWeight, duration) ->
            let initialWeight =
                match getActorProp character gameplay with
                | Some actor ->
                    actor.Morphs |>
                    Array.tryPick (fun (i, w) -> if i = morphIndex then Some w else None) |>
                    Option.defaultValue 0.0f
                | None -> 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (MorphState (character, morphIndex, initialWeight, targetWeight, startTime, endTime), just gameplay)

        | MorphState (character, morphIndex, initialWeight, targetWeight, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = updateActorProp character (fun a -> { a with Morphs = updateMorph targetWeight morphIndex a.Morphs }) gameplay
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentWeight = initialWeight + t * (targetWeight - initialWeight)
            let gameplay = updateActorProp character (fun a -> { a with Morphs = updateMorph currentWeight morphIndex a.Morphs }) gameplay
            (cue, just gameplay)

        | Cue.CrossMorph (character, fromMorphIndex, toMorphIndex, targetWeight, duration) ->
            let actor = getActorProp character gameplay
            let initialFromWeight =
                actor |> Option.bind (fun a -> Array.tryPick (fun (i, w) -> if i = fromMorphIndex then Some w else None) a.Morphs) |>
                Option.defaultValue 0.0f
            let initialToWeight =
                actor |> Option.bind (fun a -> Array.tryPick (fun (i, w) -> if i = toMorphIndex then Some w else None) a.Morphs) |>
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (CrossMorphState (character, fromMorphIndex, toMorphIndex, initialFromWeight, initialToWeight, targetWeight, startTime, endTime), just gameplay)

        | CrossMorphState (character, fromMorphIndex, toMorphIndex, initialFromWeight, initialToWeight, targetWeight, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = updateActorProp character (fun a ->
                    let morphs = updateMorph 0.0f fromMorphIndex a.Morphs
                    let morphs = updateMorph targetWeight toMorphIndex morphs
                    { a with Morphs = morphs }) gameplay
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFromWeight = initialFromWeight + t * (0.0f - initialFromWeight)
            let currentToWeight = initialToWeight + t * (targetWeight - initialToWeight)
            let gameplay = updateActorProp character (fun a ->
                let morphs = updateMorph currentFromWeight fromMorphIndex a.Morphs
                let morphs = updateMorph currentToWeight toMorphIndex morphs
                { a with Morphs = morphs }) gameplay
            (cue, just gameplay)

        | Cue.AddAdvent advent ->
            let advents = Set.add advent gameplay.GameplayState.Advents
            let gameplay = { gameplay with Gameplay.GameplayState.Advents = advents }
            (Fin, just gameplay)

        | EnableCameraFollow ->
            let gameplay = { gameplay with Gameplay.GameplayState.CameraFollowEnabled = true }
            (Fin, just gameplay)

        | DisableCameraFollow ->
            let gameplay = { gameplay with Gameplay.GameplayState.CameraFollowEnabled = false }
            (Fin, just gameplay)

        | EnableAvatarMovement ->
            let gameplay = { gameplay with Gameplay.GameplayState.AvatarMovementEnabled = true }
            (Fin, just gameplay)

        | DisableAvatarMovement ->
            let gameplay = { gameplay with Gameplay.GameplayState.AvatarMovementEnabled = false }
            (Fin, just gameplay)

        | Await advent ->
            if Set.contains advent gameplay.GameplayState.Advents
            then (Fin, just gameplay)
            else (cue, just gameplay)

        | Wait duration ->
            let endTime = world.GameTime + GameTime.ofSeconds (double duration)
            (WaitState endTime, just gameplay)

        | WaitState endTime ->
            if world.GameTime >= endTime
            then (Fin, just gameplay)
            else (cue, just gameplay)

        | Cue.FadeOut duration ->
            let initialFade = gameplay.GameplayState.Fade
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (FadeOutState (initialFade, startTime, endTime), just gameplay)

        | FadeOutState (initialFade, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = { gameplay with Gameplay.GameplayState.Fade = 1.0f }
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let fade = initialFade + t * (1.0f - initialFade)
            let gameplay = { gameplay with Gameplay.GameplayState.Fade = fade }
            (cue, just gameplay)

        | Cue.FadeIn duration ->
            let initialFade = gameplay.GameplayState.Fade
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            (FadeInState (initialFade, startTime, endTime), just gameplay)

        | FadeInState (initialFade, startTime, endTime) ->
            if world.GameTime >= endTime then
                let gameplay = { gameplay with Gameplay.GameplayState.Fade = 0.0f }
                (Fin, just gameplay)
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let fade = initialFade + t * (0.0f - initialFade)
            let gameplay = { gameplay with Gameplay.GameplayState.Fade = fade }
            (cue, just gameplay)

        | Cue.FlyCamera (path, speed) ->
            let pathEntity = Simulants.GameplayScene / path
            let nodeData = pathEntity.GetNodePositionsAndRotations world
            let points = nodeData |> List.map fst |> Array.ofList
            let rotations = nodeData |> List.map snd |> Array.ofList
            let initialPos = pathEntity.GetPosition world
            let initialRot = pathEntity.GetRotation world
            if points.Length < 2 || speed <= 0.0f then
                (Fin, withSignal (SetCamera (initialPos, initialRot)) gameplay)
            else
            let totalLength = Maths.approxSplineLength points 10
            let durationSeconds = totalLength / speed
            (FlyCameraState (points, rotations, durationSeconds, world.GameTime), withSignal (SetCamera (initialPos, initialRot)) gameplay)

        | FlyCameraState (points, rotations, durationSeconds, startTime) ->
            let elapsed = single (world.GameTime - startTime).Seconds
            let t = elapsed / durationSeconds |> max 0.0f |> min 1.0f
            if t >= 1.0f then
                let finalPos = Maths.evalSpline points 1.0f
                let finalRot = Maths.slerpAlongPath rotations 1.0f
                (Fin, withSignal (SetCamera (finalPos, finalRot)) gameplay)
            else
            let pos = Maths.evalSpline points t
            let rot = Maths.slerpAlongPath rotations t
            (cue, withSignal (SetCamera (pos, rot)) gameplay)

        | Fork cue ->
            updateCue cue gameplay world

        | If (advent, thenCue, elseCue) ->
            if Set.contains advent gameplay.GameplayState.Advents
            then updateCue thenCue gameplay world
            else updateCue elseCue gameplay world

        | Sequence cues ->
            // Fold over each cue in order. Accumulator tracks:
            //   halted     - whether a previous cue blocked (still running), so remaining cues are deferred
            //   haltedCues - the blocked cue + all subsequent unprocessed cues
            //   forkedCues - cues collected from Fork ops, to be run in parallel alongside the main sequence
            //   signals    - accumulated signals from processed cues
            //   gameplay   - threaded gameplay state
            let stepSequence (halted, haltedCues, forkedCues, (signals: Signal FDeque, gameplay)) cue =
                // if a previous cue blocked, just append this cue to the deferred queue
                if halted then (halted, FDeque.conj cue haltedCues, forkedCues, (signals, gameplay)) else
                match cue with
                // Fork: collect the forked cue to run in parallel later, don't block the sequence
                | Fork forkedCue -> (false, FDeque.empty, FDeque.conj forkedCue forkedCues, (signals, gameplay))
                | _ ->
                // process the cue normally
                let (cue, (signals2, gameplay)) = updateCue cue gameplay world
                let signals = List.fold (fun acc s -> FDeque.conj s acc) signals signals2
                if cue.IsFin
                // cue completed instantly — continue to the next cue in the sequence
                then (false, FDeque.empty, forkedCues, (signals, gameplay))
                // cue blocked — mark as halted so remaining cues are deferred
                else (true, FDeque.conj cue FDeque.empty, forkedCues, (signals, gameplay))

            let (_, haltedCues, forkedCues, (signals, gameplay)) =
                FDeque.fold stepSequence (false, FDeque.empty, FDeque.empty, (FDeque.empty, gameplay)) cues

            let signals = Seq.toList signals
            // build the remaining main sequence (Fin if everything completed)
            let mainCue =
                if FDeque.isEmpty haltedCues 
                then Fin
                else Sequence haltedCues
            // if no forks were encountered, return the main sequence as-is
            if FDeque.isEmpty forkedCues then
                (mainCue, (signals, gameplay))
            else
            // forks were encountered — wrap the main sequence + forked cues into a Parallel
            // so the sequence continues running alongside the forked cues
            let allParallel =
                if mainCue.IsFin 
                then forkedCues
                else FDeque.cons mainCue forkedCues
            (Parallel allParallel, (signals, gameplay))

        | Parallel cues ->
            let stepParallel (remaining, (signals: Signal FDeque, gameplay)) cue =
                let (cue, (signals2, gameplay)) = updateCue cue gameplay world
                let signals = List.fold (fun acc s -> FDeque.conj s acc) signals signals2
                if cue.IsFin
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
         if gameplay.GameplayState.AvatarMovementEnabled then
            Screen.UpdateEvent => ProcessAvatarInput
         if gameplay.GameplayState.CameraFollowEnabled then
            Screen.UpdateEvent => CameraFollow
         Screen.UpdateEvent => StepCues
         Screen.UpdateEvent => UpdateExposition
         Game.KeyboardKeyDownEvent =|> fun data ->
            if data.Data.KeyboardKey = KeyboardKey.E then AdvanceExposition else Nil
         Simulants.GameplayAvatar.BodyTransformEvent =|> (fun x -> AvatarPhysicsUpdate x.Data)
         for KeyValue (character, _) in gameplay.GameplayState.Actors do
            let name = Character.toName character
            let entity = Simulants.GameplayScene / name
            entity.BodyTransformEvent =|> fun evt -> ActorPhysicsUpdate (character, evt.Data)]

    // here we handle the above messages
    override this.Message (gameplay, message, _, world) =

        match message with
        | Nil -> just gameplay
        | StartPlaying ->
            let startWaypoint = Simulants.GameplayScene / "Start"
            let position = startWaypoint.GetPosition world
            let rotation = startWaypoint.GetRotation world
            let player =
                { Position = position
                  Rotation = rotation
                  Animations = Array.empty
                  Morphs = Array.empty }

            let gameplay = { gameplay with Gameplay.GameplayState.Avatar = Some (Player, player) }
            withSignal (WarpAvatar (position, rotation)) gameplay

        | AvatarPhysicsUpdate data ->
            match gameplay.GameplayState.Avatar with
            | None -> just gameplay
            | Some (character, avatar) ->
            let avatar =
                { avatar with
                    Position = data.BodyCenter
                    Rotation = data.BodyRotation }
            let gameplay = { gameplay with Gameplay.GameplayState.Avatar = Some (character, avatar) }
            just gameplay

        | ActorPhysicsUpdate (character, data) ->
            let actors =
                gameplay.GameplayState.Actors |> Map.change character (Option.map (fun a ->
                    { a with Position = data.BodyCenter; Rotation = data.BodyRotation }))
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
                { Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Map.add character actor gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | AddActor (character, position, rotation) ->
            let idle = Character.idle character
            let idle = Animation.make world.GameTime None idle Playback.Loop animationRate 1.0f None
            let actor =
                { Position = position
                  Rotation = rotation
                  Animations = Array.singleton idle
                  Morphs = Array.empty }
            let actors = Map.add character actor gameplay.GameplayState.Actors
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay

        | RemoveActor character ->
            let actors = Map.remove character gameplay.GameplayState.Actors
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
            if cue.IsFin then just gameplay else
            let (cue, (signals, gameplay)) = updateCue cue gameplay world
            let gameplay = { gameplay with Gameplay.GameplayState.Cue = cue }
            withSignals signals gameplay

        | CameraFollow ->
            match gameplay.GameplayState.Avatar with
            | None -> just gameplay
            | Some (_, avatar) ->
            let rotation = avatar.Rotation
            let cameraRotation = rotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
            let position = avatar.Position + v3Up * 1.40f - cameraRotation.Forward
            withSignal (SetCamera (position, cameraRotation)) gameplay

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
            | Some (avatarCharacter, avatar) ->

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
            let animations = Character.locomotionAnimations isMoving blendRate animationRate world.GameTime avatar.Animations avatarCharacter
            let avatar = { avatar with Animations = animations }
            let gameplayState = { gameplay.GameplayState with Avatar = Some (avatarCharacter, avatar) }
            do screen.SetGameplay { gameplay with GameplayState = gameplayState } world

        | SetCamera (position, rotation) ->
            do World.setEye3dCenter position world
            do World.setEye3dRotation rotation world

    

    // here we sync the avatar and actor models from their entities when editing in Gaia
    override this.Edit (gameplay, op, _, world) =
        match op with
        | ViewportOverlay _ when not world.Advancing ->
            // sync avatar
            let gameplay =
                match gameplay.GameplayState.Avatar with
                | None -> gameplay
                | Some (character, avatar) ->
                let position = Simulants.GameplayAvatar.GetPosition world
                let rotation = Simulants.GameplayAvatar.GetRotation world
                let bodyId = Simulants.GameplayAvatar.GetBodyId world
                do World.setBodyCenter position bodyId world
                do World.setBodyRotation rotation bodyId world
                let avatar = { avatar with Position = position; Rotation = rotation }
                { gameplay with Gameplay.GameplayState.Avatar = Some (character, avatar) }

            // sync actors
            let actors =
                gameplay.GameplayState.Actors |> Map.map (fun character actor ->
                    let name = Character.toName character
                    let entity = Simulants.GameplayScene / name
                    if not (entity.GetExists world) then actor else
                    let position = entity.GetPosition world
                    let rotation = entity.GetRotation world
                    { actor with Position = position; Rotation = rotation })
            let gameplay = { gameplay with Gameplay.GameplayState.Actors = actors }
            just gameplay
        | _ -> just gameplay


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
              | Some (avatarCharacter, avatarProp) ->
              let path = Character.toPath avatarCharacter
              Content.entityFromFile Simulants.GameplayAvatar.Name path
                  [Entity.PhysicsMotion == PhysicsMotion.ManualMotion
                   Entity.Position := avatarProp.Position
                   Entity.Rotation := avatarProp.Rotation
                   Entity.Animations := avatarProp.Animations
                   Entity.Morphs := avatarProp.Morphs]

              // actors
              for KeyValue (character, actor) in gameplay.GameplayState.Actors do
                  let name = Character.toName character
                  let path = Character.toPath character
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

             // screen fade overlay
             if gameplay.GameplayState.Fade > 0.0f then
                 Content.staticSprite "ScreenFade"
                     [Entity.Position == v3 0.0f 0.0f 0.0f
                      Entity.Size == v3 640.0f 360.0f 0.0f
                      Entity.Elevation == 100.0f
                      Entity.Color := Color(0.0f, 0.0f, 0.0f, gameplay.GameplayState.Fade)]

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
