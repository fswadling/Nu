namespace MyGame2

open Nu
open Prime
open System.Numerics
open ImGuiNET

type Exposition =
    { Text: string
      Position: Vector2
      Variant: ExpositVariant }

type AppearProgress = single

type ScriptProgression =
    | ManualProgression
    | Automatic
    | FastForward

[<AutoOpen>]
module ScenarioExtensions =
    type Game with
        member this.GetScreenTag world : ScreenTag = this.Get (nameof Game.ScreenTag) world
        member this.SetScreenTag (value : ScreenTag) world = this.Set (nameof Game.ScreenTag) value world
        member this.ScreenTag = lens (nameof Game.ScreenTag) this this.GetScreenTag this.SetScreenTag

    type Group with
        member this.GetExpositions world : HMap<Exposition, AppearProgress> = this.Get (nameof Group.Expositions) world
        member this.SetExpositions (value: HMap<Exposition, AppearProgress>) world = this.Set (nameof Group.Expositions) value world
        member this.Expositions = lens (nameof Group.Expositions) this this.GetExpositions this.SetExpositions

        member this.GetAreProgressionOptionsAvailable world : bool = this.Get (nameof Group.AreProgressionOptionsAvailable) world
        member this.SetAreProgressionOptionsAvailable (value : bool) world = this.Set (nameof Group.AreProgressionOptionsAvailable) value world
        member this.AreProgressionOptionsAvailable = lens (nameof Group.AreProgressionOptionsAvailable) this this.GetAreProgressionOptionsAvailable this.SetAreProgressionOptionsAvailable

        member this.GetCueProgression world : ScriptProgression = this.Get (nameof Group.CueProgression) world
        member this.SetCueProgression (value : ScriptProgression) world = this.Set (nameof Group.CueProgression) value world
        member this.CueProgression = lens (nameof Group.CueProgression) this this.GetCueProgression this.SetCueProgression

        member this.GetFade world : single = this.Get (nameof Group.Fade) world
        member this.SetFade (value : single) world = this.Set (nameof Group.Fade) value world
        member this.Fade = lens (nameof Group.Fade) this this.GetFade this.SetFade

        member this.GetPendingSignals world : SignalTag list = this.Get (nameof Group.PendingSignals) world
        member this.SetPendingSignals (value : SignalTag list) world = this.Set (nameof Group.PendingSignals) value world
        member this.PendingSignals = lens (nameof Group.PendingSignals) this this.GetPendingSignals this.SetPendingSignals


type ScenarioDispatcher () =
    inherit GroupDispatcherImSim ()

    let animationRate = 30f
    let emoteDuration = 4f

    let hasAdventHappened (world: World) advent =
        let advents = Game.GetAdvents world
        Seq.contains advent advents

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

    let getFullControlPointArray (startPos: Vector3) (path: Vector3 list) =
        match path with
        | [] -> Array.singleton startPos
        | first :: _ when first = startPos -> Array.ofList path
        | _ -> Array.ofList (startPos :: path)

    let interpolateBetweenAnimations (animationName1: AnimationName) (animationName2: AnimationName) (walkWeight: single) (rate: single) (gameTime: GameTime) (animations: Animation array) =
        let idleWeight = 1.0f - walkWeight
        animations |> 
        updateAnimation animationName1 idleWeight rate gameTime |>
        updateAnimation animationName2 walkWeight rate gameTime

    let normalize (dirRaw: Vector3) =
        if dirRaw.LengthSquared() > 1e-6f
        then Vector3.Normalize dirRaw
        else Vector3.UnitZ

    let getExpositDuration (scriptProgression: ScriptProgression) =
        match scriptProgression with
        | ManualProgression -> None
        | Automatic -> Some (GameTime.ofSeconds (double Scenario.ExpositAutoDuration))
        | FastForward -> Some (GameTime.ofSeconds (double Scenario.ExpositFastForwadDuration))

    let adjustDurationForProgression (scriptProgression: ScriptProgression) (duration: single) =
        match scriptProgression with
        | ManualProgression | Automatic -> duration
        | FastForward -> duration * Scenario.FastForwardFactor

    let opHandler (cueName: string) (group: Group) (world: World) op =
        match op with
        // === Control flow ===
        | IfElse (advent, thenCue, elseCue) ->
            let branchOps = if hasAdventHappened world advent then thenCue else elseCue
            Inject branchOps

        | Op.Loop (Until advent, bodyCue) ->
            if hasAdventHappened world advent then
                let newOps = FDeque.conj op bodyCue
                Inject newOps
            else Continue

        | Op.Loop (Times times, bodyCue) ->
            if times > 0 then
                let newWhile = Op.Loop (Times (times - 1), bodyCue)
                let newOps = FDeque.conj newWhile bodyCue
                Inject newOps
            else Continue

        | Op.Loop (Always, bodyCue) ->
            let newOps = FDeque.conj op bodyCue
            Inject newOps

        | Op.AwaitSignal signal ->
            AwaitSignal signal

        | Fork cue ->
            printfn "Forking cue '%s' from cue '%s'." cue.Name cueName
            ForkCue cue

        | Forq ops ->
            printfn "Forking Forq of %d ops from cue '%s'." (FDeque.length ops) cueName
            let cue = { Name = $"{cueName}_Forq_{world.GameTime}"; Ops = ops; SignalCondition = None }
            ForkCue cue

        // === Instant ops ===
        | Print msg ->
            printfn "%s" msg
            Continue

        | Broadcast signalTag ->
            printfn "Broadcasting signal '%s' from cue '%s'." signalTag cueName
            let pendingSignals = group.GetPendingSignals world
            do group.SetPendingSignals (signalTag :: pendingSignals) world
            Continue

        | PlaySong song ->
            let fadeTime = GameTime.ofSeconds 0.5
            do World.fadeOutSong fadeTime world
            do World.playSong fadeTime fadeTime GameTime.zero None 1.0f song world
            Continue

        | Disable entityName ->
            printfn "Disabling entity %s in cue '%s'." entityName cueName
            do (group / entityName).SetEnabled false world
            Continue

        | Enable entityName ->
            printfn "Enabling entity %s in cue '%s'." entityName cueName
            do (group / entityName).SetEnabled true world
            Continue

        | EnableFastForward ->
            printfn "Enabling fast-forward in cue '%s'." cueName
            do group.SetAreProgressionOptionsAvailable true world
            Continue

        | DisableFastForward ->
            printfn "Disabling fast-forward in cue '%s'." cueName
            do group.SetAreProgressionOptionsAvailable false world
            do group.SetCueProgression ScriptProgression.ManualProgression world
            Continue

        | DisableMovement entity ->
            printfn "Disallowing movement in cue '%s'." cueName
            let entity = group / entity
            let facetNames = entity.GetFacetNames world
            do entity.SetFacetNames (Set.remove (nameof NavigableFacet) facetNames) world
            Continue

        | EnableMovement entity ->
            printfn "Allowing movement in cue '%s'." cueName
            let entity = group / entity
            let facetNames = entity.GetFacetNames world
            if not (Set.contains (nameof NavigableFacet) facetNames) then
                do entity.SetFacetNames (Set.add (nameof NavigableFacet) facetNames) world
            Continue

        | CameraFollow entity ->
            printfn "Making camera follow player in cue '%s'." cueName
            let entity = group / entity
            let facetNames = entity.GetFacetNames world
            if not (Set.contains (nameof CameraFacingFacet) facetNames) then
                do entity.SetFacetNames (Set.add (nameof CameraFacingFacet) facetNames) world
            Continue

        | StopCameraFollow entity ->
            printfn "Making camera stop following player in cue '%s'." cueName
            let entity = group / entity
            let facetNames = entity.GetFacetNames world
            do entity.SetFacetNames (Set.remove (nameof CameraFacingFacet) facetNames) world
            Continue

        | DollyCameraFollow (entityName, pathName) ->
            printfn "Making camera dolly-follow player in cue '%s'." cueName
            let entity = group / entityName
            let facetNames = entity.GetFacetNames world
            if not (Set.contains (nameof CameraFacingFacet) facetNames) then
                do entity.SetFacetNames (Set.add (nameof CameraFacingFacet) facetNames) world
            do entity.SetDolly pathName world
            Continue

        | StopDollyCameraFollow entityName ->
            printfn "Making camera stop dolly-following player in cue '%s'." cueName
            let entity = group / entityName
            let facetNames = entity.GetFacetNames world
            do entity.SetFacetNames (Set.remove (nameof DollyTrackingCameraFacet) facetNames) world
            do entity.SetDolly "" world
            Continue

        | SetCamera (entityName: EntityName) ->
            printfn "Setting camera position in cue to entity '%s'." entityName 
            let entity = group / entityName
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            do Game.SetEye3dCenter position world
            do Game.SetEye3dRotation rotation world
            Continue

        | MoveCharacter (character, location) ->
            let characterName = Character.toName character
            printfn "Moving character '%s' to location '%s' in cue '%s'." characterName location cueName
            let entity = group / characterName
            let locationEntity = group / location
            do entity.SetPosition (locationEntity.GetPosition world) world
            do entity.SetRotation (locationEntity.GetRotation world) world
            Continue

        | AddCharacter (character, spawnPoint) ->
            let characterName = Character.toName character
            printfn "Adding character '%s' in cue '%s'." characterName cueName
            let spawnEntity = group / spawnPoint
            let position = spawnEntity.GetPosition world
            let degrees = spawnEntity.GetDegrees world
            let characterPath = Character.toPath character
            let entity = World.readEntityFromFile false false characterPath (Some characterName) group world
            do entity.SetPosition position world
            do entity.SetDegrees degrees world
            Continue

        | RemoveCharacter character ->
            let characterName = Character.toName character
            printfn "Removing character '%s' in cue '%s'." characterName cueName
            do World.destroyEntity (group / characterName) world
            Continue

        | StopAllAnimations entityName ->
            printfn "Stopping all animations on entity '%s' in cue '%s'." entityName cueName
            do (group / entityName).SetAnimations [||] world
            Continue

        | AddScenario (zone, scenario) ->
            let zoneName = Zone.toName zone
            let scenarioName = Scenario.toName scenario
            printfn "Adding scenario '%s' to zone '%s' in cue '%s'." scenarioName zoneName cueName
            let screen = Game / zoneName
            let scenarios = screen.GetScenarios world
            do screen.SetScenarios (Set.add scenario scenarios) world
            Continue

        | RemoveScenario (zone, scenario) ->
            let zoneName = Zone.toName zone
            let scenarioName = Scenario.toName scenario
            printfn "Removing scenario '%s' from zone '%s' in cue '%s'." scenarioName zoneName cueName
            let screen = Game / zoneName
            let scenarios = screen.GetScenarios world
            do screen.SetScenarios (Set.remove scenario scenarios) world
            Continue

        | SetPlayerScenario scenario ->
            printfn $"Setting player scenario to '{scenario}' in cue '{cueName}'."
            do group.Screen.SetPlayerScenario (Some scenario) world
            Continue

        | Advent advent ->
            printfn "Triggering advent '%A' in cue '%s'." advent cueName
            let advents = Game.GetAdvents world
            let durable = Durable.zip advents Progression.main
            let continuation = Durable.toContinuation durable
            match continuation with
            | None -> failwith "oh oh spagetti ohs"
            | Some continuation ->
            let durable = continuation advent
            let cue = durable |> Durable.toState |> FDeque.ofSeq
            let advents = FDeque.conj advent advents
            do Game.SetAdvents advents world
            Inject cue

        | SwitchToZone zone ->
            printfn $"Switching to zone '{zone}' in cue '{cueName}'."
            do Game.SetScreenTag (Zone zone) world
            Continue

        // === Time-based ops (initial -> state transition) ===
        | Wait duration ->
            printfn "Waiting for %f seconds in cue '%s'." duration cueName
            let cueProgression = group.GetCueProgression world
            let duration = adjustDurationForProgression cueProgression duration
            let endTime = world.GameTime + GameTime.ofSeconds (double duration)
            Executing (WaitState endTime)

        | WaitState endTime ->
            if world.GameTime >= endTime then Continue
            else StillExecuting

        | PlaySound (_, duration, _) ->
            let endTime = world.GameTime + GameTime.ofSeconds (double duration)
            Executing (PlaySoundState endTime)

        | PlaySoundState endTime ->
            if world.GameTime >= endTime then Continue
            else StillExecuting

        | FadeOut duration ->
            printfn "Fading to black over %f seconds in cue '%s'." duration cueName
            let cueProgression = group.GetCueProgression world
            let duration = adjustDurationForProgression cueProgression duration
            let initialFade = group.GetFade world
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (FadeOutState (initialFade, startTime, endTime))

        | FadeOutState (initialFade, startTime, endTime) ->
            if world.GameTime >= endTime then
                do group.SetFade 1.0f world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFade = initialFade + t * (1.0f - initialFade)
            do group.SetFade currentFade world
            StillExecuting

        | FadeIn duration ->
            printfn "Fading in from black over %f seconds in cue '%s'." duration cueName
            let cueProgression = group.GetCueProgression world
            let duration = adjustDurationForProgression cueProgression duration
            let initialFade = group.GetFade world
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (FadeInState (initialFade, startTime, endTime))

        | FadeInState (initialFade, startTime, endTime) ->
            if world.GameTime >= endTime then
                do group.SetFade 0.0f world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFade = initialFade + t * (0.0f - initialFade)
            do group.SetFade currentFade world
            StillExecuting

        | Animation (entityName, animationName, targetWeight, duration) ->
            printfn "Animating '%s' on entity '%s' to weight %f over %f seconds in cue '%s'." animationName entityName targetWeight duration cueName
            let entity = group / entityName
            let initialWeight = 
                entity.GetAnimations world |>
                Array.tryFind (fun a -> a.Name = animationName) |> 
                Option.map _.Weight |> 
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (AnimationState (entityName, animationName, initialWeight, targetWeight, startTime, endTime))

        | AnimationState (entityName, animationName, initialWeight, targetWeight, startTime, endTime) ->
            let entity = group / entityName
            if not (entity.GetExists world) then Continue
            elif world.GameTime >= endTime then
                let animations = entity.GetAnimations world
                let animations = updateAnimation animationName targetWeight animationRate world.GameTime animations
                do entity.SetAnimations animations world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentWeight = initialWeight + t * (targetWeight - initialWeight)
            let animations = entity.GetAnimations world
            let animations = updateAnimation animationName currentWeight animationRate world.GameTime animations
            do entity.SetAnimations animations world
            StillExecuting

        | ChangeAnimation (entityName, fromAnimation, toAnimation, targetWeight, duration) ->
            printfn "Changing animation from '%s' to '%s' on entity '%s' to weight %f over %f seconds in cue '%s'." fromAnimation toAnimation entityName targetWeight duration cueName
            let entity = group / entityName
            let initialFromWeight = 
                entity.GetAnimations world |> 
                Array.tryFind (fun a -> a.Name = fromAnimation) |>
                Option.map _.Weight |> 
                Option.defaultValue 0.0f
            let initialToWeight = 
                entity.GetAnimations world |> 
                Array.tryFind (fun a -> a.Name = toAnimation) |> 
                Option.map _.Weight |> 
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (ChangeAnimationState (entityName, fromAnimation, toAnimation, initialFromWeight, initialToWeight, targetWeight, startTime, endTime))

        | ChangeAnimationState (entityName, fromAnimation, toAnimation, initialFromWeight, initialToWeight, targetWeight, startTime, endTime) ->
            let entity = group / entityName
            if not (entity.GetExists world) then Continue
            elif world.GameTime >= endTime then
                let animations = entity.GetAnimations world
                let animations = updateAnimation fromAnimation 0.0f animationRate world.GameTime animations
                let animations = updateAnimation toAnimation targetWeight animationRate world.GameTime animations
                do entity.SetAnimations animations world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFromWeight = initialFromWeight + t * (0.0f - initialFromWeight)
            let currentToWeight = initialToWeight + t * (targetWeight - initialToWeight)
            let animations = entity.GetAnimations world
            let animations = updateAnimation fromAnimation currentFromWeight animationRate world.GameTime animations
            let animations = updateAnimation toAnimation currentToWeight animationRate world.GameTime animations
            do entity.SetAnimations animations world
            StillExecuting

        | FacialExpression (entityName, morphIndex, targetWeight, duration) ->
            printfn "Changing facial expression (morph %d) on entity '%s' to weight %f over %f seconds in cue '%s'." morphIndex entityName targetWeight duration cueName
            let entity = group / entityName
            let initialWeight =
                entity.GetMorphs world |>
                Array.tryPick (fun (i, w) -> if i = morphIndex then Some w else None) |>
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (FacialExpressionState (entityName, morphIndex, initialWeight, targetWeight, startTime, endTime))

        | FacialExpressionState (entityName, morphIndex, initialWeight, targetWeight, startTime, endTime) ->
            let entity = group / entityName
            if not (entity.GetExists world) then Continue
            elif world.GameTime >= endTime then
                let morphs = entity.GetMorphs world
                let morphs = updateMorph targetWeight morphIndex morphs
                do entity.SetMorphs morphs world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentWeight = initialWeight + t * (targetWeight - initialWeight)
            let morphs = entity.GetMorphs world
            let morphs = updateMorph currentWeight morphIndex morphs
            do entity.SetMorphs morphs world
            StillExecuting

        | ChangeFacialExpression (entityName, fromMorphIndex, toMorphIndex, targetWeight, duration) ->
            printfn "Changing facial expression on entity '%s' from '%i' to '%i' to weight '%f' over %f seconds in cue '%s'." entityName fromMorphIndex toMorphIndex targetWeight duration cueName
            let entity = group / entityName
            let initialFromWeight =
                entity.GetMorphs world |>
                Array.tryPick (fun (i, w) -> if i = fromMorphIndex then Some w else None) |> 
                Option.defaultValue 0.0f
            let initialToWeight =
                entity.GetMorphs world |> 
                Array.tryPick (fun (i, w) -> if i = toMorphIndex then Some w else None) |> 
                Option.defaultValue 0.0f
            let startTime = world.GameTime
            let endTime = startTime + GameTime.ofSeconds (double duration)
            Executing (ChangeFacialExpressionState (entityName, fromMorphIndex, toMorphIndex, initialFromWeight, initialToWeight, targetWeight, startTime, endTime))

        | ChangeFacialExpressionState (entityName, fromMorphIndex, toMorphIndex, initialFromWeight, initialToWeight, targetWeight, startTime, endTime) ->
            let entity = group / entityName
            if not (entity.GetExists world) then Continue
            elif world.GameTime >= endTime then
                let morphs = entity.GetMorphs world
                let morphs = updateMorph 0.0f fromMorphIndex morphs
                let morphs = updateMorph targetWeight toMorphIndex morphs
                do entity.SetMorphs morphs world
                Continue
            else
            let t = single ((world.GameTime - startTime).Seconds / (endTime - startTime).Seconds)
            let currentFromWeight = initialFromWeight + t * (0.0f - initialFromWeight)
            let currentToWeight = initialToWeight + t * (targetWeight - initialToWeight)
            let morphs = entity.GetMorphs world
            let morphs = updateMorph currentFromWeight fromMorphIndex morphs
            let morphs = updateMorph currentToWeight toMorphIndex morphs
            do entity.SetMorphs morphs world
            StillExecuting

        | Exposit (text, variant) ->
            printfn "Setting exposition text in cue '%s'." cueName
            let position =
                match variant with
                | Thought -> Scenario.ExpositDefaultPosition
                | Dialogue | PortraitDialogue _ -> Scenario.DialogueDefaultPosition
            let exposition = { Text = text; Position = position; Variant = variant }
            let expositions = group.GetExpositions world
            let expositions = HMap.add exposition 0.0f expositions
            do group.SetExpositions expositions world
            let startTime = world.GameTime
            Executing (ExpositState (text, variant, AppearPhase, startTime, 0.0f))

        | ExpositState (text, variant, phase, startTime, peakAppearProgress) ->
            let position =
                match variant with
                | Thought -> Scenario.ExpositDefaultPosition
                | Dialogue | PortraitDialogue _ -> Scenario.DialogueDefaultPosition
            let exposition = { Text = text; Position = position; Variant = variant }
            let expositAppearDuration = GameTime.ofSeconds Scenario.ExpositAppearDuration
            let scriptProgression = group.GetCueProgression world

            match phase with
            | AppearPhase ->
                let elapsed = world.GameTime - startTime
                let appearProgress =
                    if elapsed < expositAppearDuration then single (elapsed / expositAppearDuration)
                    else 1.0f
                let expositions = group.GetExpositions world
                let expositions = HMap.add exposition appearProgress expositions
                do group.SetExpositions expositions world

                // Check if we should transition to display phase
                let durationOpt = getExpositDuration scriptProgression
                let shouldAdvance =
                    match durationOpt with
                    | None -> World.isKeyboardKeyPressed KeyboardKey.E world
                    | Some duration -> world.GameTime >= startTime + duration || World.isKeyboardKeyPressed KeyboardKey.E world

                if shouldAdvance then
                    // Transition to fade out
                    let expositions = group.GetExpositions world
                    let currentProgress = HMap.find exposition expositions
                    Executing (ExpositState (text, variant, FadeOutPhase, world.GameTime, currentProgress))
                else
                    StillExecuting

            | DisplayPhase ->
                // This phase waits for user input or auto-duration
                let durationOpt = getExpositDuration scriptProgression
                let shouldAdvance =
                    match durationOpt with
                    | None -> World.isKeyboardKeyPressed KeyboardKey.E world
                    | Some duration -> world.GameTime >= startTime + duration || World.isKeyboardKeyPressed KeyboardKey.E world

                if shouldAdvance then
                    let expositions = group.GetExpositions world
                    let currentProgress = HMap.find exposition expositions
                    Executing (ExpositState (text, variant, FadeOutPhase, world.GameTime, currentProgress))
                else
                    StillExecuting

            | FadeOutPhase ->
                let fadeOutDuration = expositAppearDuration * peakAppearProgress
                let elapsed = world.GameTime - startTime

                if elapsed >= fadeOutDuration then
                    let expositions = group.GetExpositions world
                    let expositions = HMap.remove exposition expositions
                    do group.SetExpositions expositions world
                    Continue
                else
                let fadeOutFraction = 1.0f - single (elapsed / fadeOutDuration)
                let appearProgress = peakAppearProgress * fadeOutFraction
                let expositions = group.GetExpositions world
                let expositions = HMap.add exposition appearProgress expositions
                do group.SetExpositions expositions world
                StillExecuting

        | FlyCamera (path, speed) ->
            printfn "Flying camera along spline in cue '%s'." cueName
            let pathEntity = group / path
            let nodeData = pathEntity.GetNodePositionsAndRotations world
            let pathPositions = Seq.map fst nodeData
            let pathRotations = Seq.map snd nodeData
            let points = Array.ofSeq pathPositions
            let rotations = Array.ofSeq pathRotations
            let initialPos = pathEntity.GetPosition world
            let initialRot = pathEntity.GetRotation world
            do Game.SetEye3dCenter initialPos world
            do Game.SetEye3dRotation initialRot world
            if points.Length < 2 || speed <= 0.0f then Continue else
            let totalLength = Maths.approxSplineLength points 10
            let durationSeconds = totalLength / speed
            Executing (FlyCameraState (points, rotations, durationSeconds, world.GameTime))

        | FlyCameraState (points, rotations, durationSeconds, startTime) ->
            let elapsed = single (world.GameTime - startTime).Seconds
            let t = elapsed / durationSeconds |> max 0.0f |> min 1.0f
            if t >= 1.0f then
                let finalSplinePos = Maths.evalSpline points 1.0f
                let finalRot = Maths.slerpAlongPath rotations 1.0f
                do Game.SetEye3dCenter finalSplinePos world
                do Game.SetEye3dRotation finalRot world
                Continue
            else
            let pos = Maths.evalSpline points t
            let rot = Maths.slerpAlongPath rotations t
            do Game.SetEye3dCenter pos world
            do Game.SetEye3dRotation rot world
            StillExecuting

        | FlyCameraToPosition (position, rotation, speed) ->
            printfn "Flying camera to position in cue '%s'." cueName
            let startPos = World.getEye3dCenter world
            let initialRot = World.getEye3dRotation world
            let dir = position - startPos
            let distance = dir.Length()
            let durationSeconds = if speed > 0.0f then distance / speed else 0.0f
            Executing (FlyCameraToPositionState (startPos, position, initialRot, rotation, durationSeconds, world.GameTime))

        | FlyCameraToPositionState (startPos, targetPos, initialRot, targetRot, durationSeconds, startTime) ->
            if durationSeconds <= 0.0f then
                do Game.SetEye3dCenter targetPos world
                do Game.SetEye3dRotation targetRot world
                Continue
            else
            let elapsed = single (world.GameTime - startTime).Seconds
            let t = elapsed / durationSeconds |> max 0.0f |> min 1.0f
            if t >= 1.0f then
                do Game.SetEye3dCenter targetPos world
                do Game.SetEye3dRotation targetRot world
                Continue
            else
            let pos = Vector3.Lerp(startPos, targetPos, t)
            let rot = Quaternion.Slerp(initialRot, targetRot, t)
            do Game.SetEye3dCenter pos world
            do Game.SetEye3dRotation rot world
            StillExecuting

        | LocomoteCharacter (character, path, speed, idle, moving) ->
            let characterName = Character.toName character
            printfn "Moving character '%s' along spline in cue '%s'." characterName cueName
            let entity = group / characterName
            let startPos = entity.GetPosition world
            let initialRot = entity.GetRotation world
            let pathEntity = group / path
            let nodeData = pathEntity.GetNodePositionsAndRotations world
            let pathPositions = nodeData |> List.map fst
            let finalRot = nodeData |> List.last |> snd
            let points = getFullControlPointArray startPos pathPositions
            if points.Length < 1 || speed <= 0.0f then Continue else
            let totalLength = Maths.approxSplineLength points 10
            let durationSeconds = totalLength / speed
            let walkBlendTime = min 0.25f (durationSeconds * 0.3f)

            Executing (LocomoteCharacterState (character, points, durationSeconds, world.GameTime, initialRot, finalRot, walkBlendTime, idle, moving))

        | LocomoteCharacterState (character, points, durationSeconds, startTime, initialRot, finalRot, walkBlendTime, idle, moving) ->
            let characterName = Character.toName character
            let entity = group / characterName
            if not (entity.GetExists world) then Continue else
            let elapsed = single (world.GameTime - startTime).Seconds
            let t = elapsed / durationSeconds |> max 0.0f |> min 1.0f
            let orientBlendFrac = 0.3f
            let orientBlendStart = durationSeconds * (1.0f - orientBlendFrac)
            let orientBlendEnd = durationSeconds
            let startOrientBlendTime = min 0.25f (durationSeconds * 0.3f)
            if t >= 1.0f then
                // Finished
                let finalPos = Maths.evalSpline points 1.0f
                do entity.SetPosition finalPos world
                do entity.SetRotation finalRot world
                let animations = entity.GetAnimations world
                let animations = interpolateBetweenAnimations idle moving 0.0f animationRate world.GameTime animations
                do entity.SetAnimations animations world
                Continue
            else
            let pos = Maths.evalSpline points t
            let tAhead = min 1.0f (t + 0.01f)
            let posAhead = Maths.evalSpline points tAhead
            let dirRaw = posAhead - pos
            let pathDir = normalize (-dirRaw)
            let pathRot = Maths.lookRotation pathDir

            // Start blend
            let rotAfterStart =
                if startOrientBlendTime > 0.0f && elapsed < startOrientBlendTime then
                    let s = elapsed / startOrientBlendTime |> max 0.0f |> min 1.0f
                    Quaternion.Slerp(initialRot, pathRot, s)
                else pathRot

            // End blend
            let rot =
                let blendParam =
                    if elapsed <= orientBlendStart then 0.0f
                    elif elapsed >= orientBlendEnd then 1.0f
                    else (elapsed - orientBlendStart) / (orientBlendEnd - orientBlendStart)
                let blendParam = blendParam |> max 0.0f |> min 1.0f
                if blendParam <= 0.0f then rotAfterStart
                else Quaternion.Slerp(rotAfterStart, finalRot, blendParam)

            do entity.SetPosition pos world
            do entity.SetRotation rot world

            // Walk weight
            let walkWeight =
                if elapsed < walkBlendTime then elapsed / walkBlendTime
                elif elapsed > durationSeconds - walkBlendTime then (durationSeconds - elapsed) / walkBlendTime
                else 1.0f
                |> max 0.0f |> min 1.0f

            let animations = entity.GetAnimations world
            let animations = interpolateBetweenAnimations idle moving walkWeight animationRate world.GameTime animations
            do entity.SetAnimations animations world
            StillExecuting

        | Emote (character, emoteImage) ->
            let characterName = Character.toName character
            printfn "Adding emote '%s' to character '%s' in cue '%s'." (emoteImage.ToString()) characterName cueName
            let characterEntity = group / characterName
            let emoteId = $"Emote_{world.GameTime}"
            let emote =
                World.createEntity<AnimatedSpriteDispatcher>
                    (Some characterEntity.EntityAddress)
                    OverlayDescriptor.DefaultOverlay
                    (Some [|characterName; emoteId|])
                    group
                    world

            let emoteHeight = Character.getEmoteHeight character
            let mutable position3d = characterEntity.GetPosition world
            position3d.Y <- position3d.Y + emoteHeight
            let position2d = World.position3dToPosition2d position3d world
            do emote.SetAnimationSheet emoteImage world
            do emote.SetAnimationStride 187 world
            do emote.SetCelCount 91 world
            do emote.SetCelRun 91 world
            do emote.SetCelSize (v2 187f 187f) world
            do emote.SetSize (v3 64f 64f 0f) world
            do emote.SetPosition position2d.V3 world
            let scriptProgression = group.GetCueProgression world
            let duration = adjustDurationForProgression scriptProgression emoteDuration
            let endTime = world.GameTime + GameTime.ofSeconds (double duration)

            // Store the emote entity name so we can destroy it later
            Executing (EmoteState (emote.Name, endTime))

        | EmoteState (emoteEntityName, endTime) ->
            if world.GameTime >= endTime then
                // Find and destroy the emote entity
                let entities = World.getEntities group world
                for entity in entities do
                    if entity.Name = emoteEntityName then
                        do World.destroyEntity entity world
                Continue
            else
                StillExecuting

    let runCue (group: Group) (world: World) (cue: Cue) =
        Cue.runUntilBlocked (opHandler cue.Name group world) cue

    let processPendingSignals (group: Group) (world: World) =
        let pendingSignals = group.GetPendingSignals world
        if (List.isEmpty pendingSignals) then () else
        do group.SetPendingSignals [] world
        let cues = group.GetCues world
        let processSignal (cues, forked) signalTag =
            let results = cues |> Seq.map (Cue.signal (runCue group world) signalTag) |> Seq.toList
            let stepped = results |> Seq.choose fst |> FDeque.ofSeq
            let newForked = results |> Seq.map snd |> Seq.fold FDeque.append FDeque.empty
            (stepped, FDeque.append newForked forked)
        let cues, forked = List.fold processSignal (cues, FDeque.empty) pendingSignals
        do group.SetCues (FDeque.append forked cues) world

    let advanceActiveCues (group: Group) (world: World) =
        let cues = group.GetCues world
        let results = cues |> Seq.map (fun cue -> Cue.step (opHandler cue.Name group world) cue) |> Seq.toList
        let stepped = results |> Seq.choose fst |> FDeque.ofSeq
        let forked = results |> Seq.map snd |> Seq.fold FDeque.append FDeque.empty
        do group.SetCues (FDeque.append forked stepped) world

    let exposition (group: Group) (world: World) =
        let expositions = group.GetExpositions world
        let expositions = HMap.toSeq expositions

        for i, (exposition, appearProgress) in Seq.indexed expositions do
            let text = if appearProgress = 1.0f then exposition.Text else String.empty

            match exposition.Variant with
            | Thought ->
                let width = Scenario.ExpositDefaultSize.X * appearProgress
                let height = Scenario.ExpositDefaultSize.Y * appearProgress

                World.doText
                    $"Exposition_{i}_{exposition.GetHashCode()}"
                    [ Entity.Text @= text
                      Entity.Position .= exposition.Position.V3
                      Entity.Color .= Color.DarkGray
                      Entity.TextColor .= Color.White
                      Entity.Size @= v3 width height 0f]
                    world

            | Dialogue ->
                let width = Scenario.ExpositDefaultSize.X * appearProgress
                let height = Scenario.ExpositDefaultSize.Y * appearProgress

                World.doText
                    $"Exposition_{exposition.GetHashCode()}"
                    [ Entity.Text @= text
                      Entity.Position .= exposition.Position.V3
                      Entity.TextColor .= Color.Black
                      Entity.Size @= v3 width height 0f]
                    world

            | PortraitDialogue portrait ->
                let width = Scenario.DialogueDefaultSize.X * appearProgress
                let height = Scenario.DialogueDefaultSize.Y * appearProgress

                World.beginPanel
                    $"Exposition_{exposition.GetHashCode()}" 
                    [ Entity.Position .= exposition.Position.V3
                      Entity.Layout .= Manual
                      Entity.Size @= v3 width height 0f ]
                    world

                if appearProgress = 1f then
                    World.doStaticSprite "Portrait" 
                        [ Entity.PositionLocal .= v3 -137f 3f 1f
                          Entity.Size .= v3 80f 64f 0f
                          Entity.StaticImage .= portrait
                          Entity.Elevation .= 1f ]
                        world

                World.doText
                    "Dialogue"
                    [ Entity.Text @= text
                      Entity.BackdropImageOpt .= None
                      Entity.Justification .= Unjustified true
                      Entity.TextColor .= Color.Black
                      Entity.PositionLocal .= v3 40f 3f 0f
                      Entity.Size .= v3 256f 64f 0f ]
                    world

                World.endPanel world

    let fastForward (group: Group) (world: World) =
        let isFastForwardAvailable = group.GetAreProgressionOptionsAvailable world
        if not isFastForwardAvailable then () else

        let scriptProgression = group.GetCueProgression world
        let text =
            match scriptProgression with
            | FastForward -> "Press Space to progress manually"
            | Automatic -> "Press Space to fast-forward"
            | ManualProgression -> "Press Space to auto-progress"

        World.doText
            "FastForwardPrompt"
            [ Entity.Text @= text
              Entity.Color .= Color.DarkGray
              Entity.Position .= Scenario.PromptDefaultPosition.V3
              Entity.Size .= Scenario.PromptDefaultSize.V3 ]
            world

        if not world.Advancing then () else

        let next =
            match scriptProgression with
            | ManualProgression -> Automatic
            | Automatic -> FastForward
            | FastForward -> ManualProgression

        if World.isKeyboardKeyPressed KeyboardKey.Space world then
            do group.SetCueProgression next world

    let fade (group: Group) (world: World) =
        let fade = group.GetFade world
        if fade <= 0f then () else

        World.doStaticSprite
            "ScreenFade"
            [ Entity.Position .= v3 0f 0f 0f
              Entity.Size .= v3 640f 360f 0f
              Entity.Color @= Color(0f, 0f, 0f, fade) ]
            world

    let interactablePrompt (group: Group) (world: World) =
        let screen = group.Screen

        let isZoneDispatcher =
            match screen.GetDispatcher world with
            | :? ZoneDispatcher -> true
            | _ -> false

        if not isZoneDispatcher then () else

        // find the player entity
        let playerScenarioOpt = screen.GetPlayerScenario world
        match playerScenarioOpt with
        | None -> ()
        | Some playerScenario ->

        let playerScenarioName = Scenario.toName playerScenario
        let playerName = "Player"
        let player = screen / playerScenarioName / playerName
        if not (player.GetExists world) then () else

        let playerPosition = player.GetPosition world

        // find the closest interactable entity within range
        let interactables = World.getEntitiesWith<InteractableFacet> group world
        let runningCues = group.GetCues world

        let isEngaged (entity: Entity) =
            let isEnabled = entity.GetEnabled world
            if not isEnabled then None else 
            let interactCueOpt = entity.GetInteractCue world
            match interactCueOpt with 
            | None -> None 
            | Some interactCue ->
            let isCueRunning = runningCues |> Seq.exists (fun s -> s.Name = interactCue.Name)
            if isCueRunning then None else
            let entityPosition = entity.GetPosition world
            let distance = (entityPosition - playerPosition).Magnitude
            let interactDistance = entity.GetInteractDistance world
            if distance > interactDistance then None else
            Some (entity, distance)

        let closestInteractableOpt =
            interactables |> 
            Seq.choose isEngaged |> 
            Seq.sortBy (fun (_, distance) -> distance) |> 
            Seq.tryHead

        match closestInteractableOpt with
        | None -> ()
        | Some (entity, _) ->

        let promptMessage = entity.GetInteractPromptMessage world
        let interactKey = entity.GetInteractKey world

        // show the prompt using ImSim style
        World.doText
            "InteractPrompt"
            [ Entity.Text @= promptMessage
              Entity.Position .= Scenario.PromptDefaultPosition.V3
              Entity.Color .= Color.DarkGray
              Entity.Size .= Scenario.PromptDefaultSize.V3 ]
            world

        if not world.Advancing then () else

        // handle interaction
        if World.isKeyboardKeyPressed interactKey world then
            match entity.GetInteractCue world with
            | Some interactCue ->
                let cues = group.GetCues world
                let cues = FDeque.cons interactCue cues
                do group.SetCues cues world
            | None -> ()

    override this.Process (group, world) =
        let isScreenSelected = group.GetSelected world

        if not isScreenSelected then () else
        do exposition group world
        do fastForward group world
        do interactablePrompt group world
        do fade group world

        if not world.Advancing then () else
        do processPendingSignals group world
        do advanceActiveCues group world

    override this.Edit (op, group, world) =
        base.Edit (op, group, world)
        match op with
        | AppendProperties appendProperties ->
            // Add buttons/controls to the property panel
            if ImGui.Button "Add path" then
                let position = World.getEye3dCenter world
                let rotation = World.getEye3dRotation world
                let mountOpt = None
                let names = [| "Node" |]
                let waypoint = World.createEntity<PathDispatcher> mountOpt OverlayDescriptor.DefaultOverlay (Some names) group world
                do waypoint.SetPosition position world
                do waypoint.SetRotation rotation world
                do appendProperties.EditContext.Snapshot SnapshotType.CreateEntity world
         | _ -> ()

    static member Properties =
        [ define Group.AreProgressionOptionsAvailable false
          define Group.CueProgression ManualProgression
          define Group.Expositions (HMap.makeEmpty ())
          define Group.Cues FDeque.empty
          define Group.Fade 0f
          define Group.PendingSignals [] ]
