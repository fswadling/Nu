namespace MyGame2
open Nu
open System.Numerics
open Prime

type Advent =
    | OpeningComplete
    | ApartmentOpeningComplete
    | SakuraFirstMeeting
    | OpeningGoToClass

 [<AutoOpen>]
module CueExtensions =
    type Game with
        member this.GetAdvents world : Advent FDeque = this.Get (nameof Game.Advents) world
        member this.SetAdvents (value : Advent FDeque) world = this.Set (nameof Game.Advents) value world
        member this.Advents = lens (nameof Game.Advents) this this.GetAdvents this.SetAdvents

[<AutoOpen>]
module CueModule =
    type EntityName = string
    type SignalTag = string
    type CueName = string
    type AnimationName = string
    type GroupName = string

    type LoopCondition =
        | Always
        | Until of Advent
        | Times of int

    type SignalCondition =
        | All of SignalCondition list
        | Any of SignalCondition list
        | On of SignalTag

    module SignalCondition =
        let rec apply signal = function
            | On signal' when signal = signal' -> None
            | On signal -> Some (On signal)
            | All conditions -> 
                let conditions = List.choose (apply signal) conditions 
                match conditions with
                | [] -> None
                | conditions -> Some (All conditions)

            | Any conditions ->
                let signaledConditions = List.choose (apply signal) conditions

                if (signaledConditions.Length < conditions.Length) 
                then None
                else Some (Any signaledConditions)

    type ExpositVariant =
        | Thought
        | Dialogue
        | PortraitDialogue of Image AssetTag

    type ExpositPhase =
        | AppearPhase
        | DisplayPhase
        | FadeOutPhase

    type Op =
        // === Instant ops ===
        | Print of string
        | PlaySong of Song: Song AssetTag
        | DisableMovement of Entity:EntityName
        | EnableMovement of Entity:EntityName
        | CameraFollow of Entity:EntityName
        | StopCameraFollow of Entity:EntityName
        | Disable of Entity:EntityName
        | Enable of Entity:EntityName
        | EnableFastForward
        | DisableFastForward
        | DollyCameraFollow of Entity:EntityName * Path:EntityName
        | StopDollyCameraFollow of Entity:EntityName
        | SetCamera of Entity:EntityName
        | MoveCharacter of Character:Character * Location:EntityName
        | AddCharacter of Character:Character * SpawnPoint:EntityName
        | RemoveCharacter of Character:Character
        | StopAllAnimations of Entity:EntityName
        | AddScenario of Zone:Zone * Scenario:Scenario
        | Advent of Advent:Advent
        | SwitchToZone of Zone:Zone
        | Broadcast of SignalTag:SignalTag

        // === Control flow ===
        | AwaitSignal of Signal: SignalCondition
        | Fork of Cue
        | Forq of Op FDeque
        | IfElse of Advent:Advent * ThenCue:Op FDeque * ElseCue:Op FDeque
        | Loop of Condition:LoopCondition * BodyCue:Op FDeque

        // === Time-based ops (initial) ===
        | Wait of Duration:single
        | PlaySound of Volume:single * Duration:single * SoundTag: Sound AssetTag
        | FadeOut of Duration:single
        | FadeIn of Duration:single
        | Animation of Entity:EntityName * Name:AnimationName * TargetWeight:single * Duration:single
        | ChangeAnimation of Entity:EntityName * From:AnimationName * To:AnimationName * TargetWeight:single * Duration:single
        | FacialExpression of Entity:EntityName * MorphIndex:int * TargetWeight:single * Duration:single
        | ChangeFacialExpression of Entity:EntityName * From:int * To:int * TargetWeight:single * Duration:single
        | Exposit of Text:string * Variant:ExpositVariant
        | FlyCamera of Path:EntityName * Speed:single
        | FlyCameraToPosition of Position:Vector3 * Rotation:Quaternion * Speed:single
        | LocomoteCharacter of Character:Character * Path:EntityName * Speed:single * Idle:AnimationName * Moving:AnimationName
        | Emote of Character:Character * Sprite: Image AssetTag

        // === Time-based ops (in-progress state) ===
        | WaitState of EndTime:GameTime
        | PlaySoundState of EndTime:GameTime
        | FadeOutState of InitialFade:single * StartTime:GameTime * EndTime:GameTime
        | FadeInState of InitialFade:single * StartTime:GameTime * EndTime:GameTime
        | AnimationState of Entity:EntityName * Name:AnimationName * InitialWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
        | ChangeAnimationState of Entity:EntityName * From:AnimationName * To:AnimationName * InitialFromWeight:single * InitialToWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
        | FacialExpressionState of Entity:EntityName * MorphIndex:int * InitialWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
        | ChangeFacialExpressionState of Entity:EntityName * From:int * To:int * InitialFromWeight:single * InitialToWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
        | ExpositState of Text:string * Variant:ExpositVariant * Phase:ExpositPhase * StartTime:GameTime * PeakAppearProgress:single
        | FlyCameraState of Points:Vector3 array * Rotations:Quaternion array * DurationSeconds:single * StartTime:GameTime
        | FlyCameraToPositionState of StartPos:Vector3 * TargetPos:Vector3 * InitialRot:Quaternion * TargetRot:Quaternion * DurationSeconds:single * StartTime:GameTime
        | LocomoteCharacterState of Character:Character * Points:Vector3 array * DurationSeconds:single * StartTime:GameTime * InitialRot:Quaternion * FinalRotOpt:Quaternion * WalkBlendTime:single * Idle:AnimationName * Moving:AnimationName
        | EmoteState of EmoteEntityName:string * EndTime:GameTime

    and Cue =
        { Name : CueName
          Ops : Op FDeque
          SignalCondition: SignalCondition option }

    type OpResult =
        | Continue
        | StillExecuting
        | Executing of Op
        | Inject of Op FDeque
        | AwaitSignal of SignalCondition
        | ForkCue of Cue

    type OpHandler = Op -> OpResult

    [<RequireQualifiedAccess>]
    module Cue =
        /// Run cue from beginning until it blocks (state op) or ends
        let rec runUntilBlocked (handle:OpHandler) (cue:Cue) : Cue option * Cue FDeque =
            let rec loop (ops:Op FDeque) (forked: Cue FDeque) =
                match ops.TryUncons with
                | None -> (None, forked)
                | Some (op, rest) ->

                match handle op with
                | Continue ->
                    loop rest forked

                | StillExecuting ->
                    // Keep current op at front, cue is still running
                    (Some { cue with Ops = ops }, forked)

                | Executing stateOp ->
                    // Replace current op with state op
                    let newOps = FDeque.cons stateOp rest
                    (Some { cue with Ops = newOps }, forked)

                | Inject injectedOps ->
                    let newOps = FDeque.append injectedOps rest
                    loop newOps forked

                | AwaitSignal signalCondition ->
                    (Some { cue with Ops = rest; SignalCondition = Some signalCondition }, forked)

                | ForkCue forkedCue ->
                    loop rest (FDeque.conj forkedCue forked)

            loop cue.Ops FDeque.empty

        /// Step a running cue (process current op)
        let step (handle:OpHandler) (cue:Cue) : Cue option * Cue FDeque =
            // If waiting for signal, don't step
            if cue.SignalCondition.IsSome then (Some cue, FDeque.empty)
            else runUntilBlocked handle cue

        let signal (runCue:Cue -> Cue option * Cue FDeque) (signalTag:SignalTag) (cue:Cue) : Cue option * Cue FDeque =
            match cue.SignalCondition with
            | None -> (Some cue, FDeque.empty)
            | Some signalCondition ->

            let signalCondition = SignalCondition.apply signalTag signalCondition

            match signalCondition with
            | None -> runCue { cue with SignalCondition = None }
            | Some signalCondition -> (Some { cue with SignalCondition = Some signalCondition }, FDeque.empty)

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
module ZoneExtensions =
    type Game with
        member this.GetScreenTag world : ScreenTag = this.Get (nameof Game.ScreenTag) world
        member this.SetScreenTag (value : ScreenTag) world = this.Set (nameof Game.ScreenTag) value world
        member this.ScreenTag = lens (nameof Game.ScreenTag) this this.GetScreenTag this.SetScreenTag

    type Screen with
        member this.GetZone world : Zone = this.Get (nameof Screen.Zone) world
        member this.SetZone (value: Zone) world = this.Set (nameof Screen.Zone) value world
        member this.Zone = lens (nameof Screen.Zone) this this.GetZone this.SetZone

        member this.GetCues world : Cue FDeque = this.Get (nameof Screen.Cues) world
        member this.SetCues (value: Cue FDeque) world = this.Set (nameof Screen.Cues) value world
        member this.Cues = lens (nameof Screen.Cues) this this.GetCues this.SetCues

        member this.GetPendingSignals world : SignalTag list = this.Get (nameof Screen.PendingSignals) world
        member this.SetPendingSignals (value : SignalTag list) world = this.Set (nameof Screen.PendingSignals) value world
        member this.PendingSignals = lens (nameof Screen.PendingSignals) this this.GetPendingSignals this.SetPendingSignals

        member this.GetExpositions world : HMap<Exposition, AppearProgress> = this.Get (nameof Screen.Expositions) world
        member this.SetExpositions (value: HMap<Exposition, AppearProgress>) world = this.Set (nameof Screen.Expositions) value world
        member this.Expositions = lens (nameof Screen.Expositions) this this.GetExpositions this.SetExpositions

        member this.GetAreProgressionOptionsAvailable world : bool = this.Get (nameof Screen.AreProgressionOptionsAvailable) world
        member this.SetAreProgressionOptionsAvailable (value : bool) world = this.Set (nameof Screen.AreProgressionOptionsAvailable) value world
        member this.AreProgressionOptionsAvailable = lens (nameof Screen.AreProgressionOptionsAvailable) this this.GetAreProgressionOptionsAvailable this.SetAreProgressionOptionsAvailable

        member this.GetCueProgression world : ScriptProgression = this.Get (nameof Screen.CueProgression) world
        member this.SetCueProgression (value : ScriptProgression) world = this.Set (nameof Screen.CueProgression) value world
        member this.CueProgression = lens (nameof Screen.CueProgression) this this.GetCueProgression this.SetCueProgression

        member this.GetFade world : single = this.Get (nameof Screen.Fade) world
        member this.SetFade (value : single) world = this.Set (nameof Screen.Fade) value world
        member this.Fade = lens (nameof Screen.Fade) this this.GetFade this.SetFade

    type Group with
        member this.GetInitialCues world : Cue FDeque = this.Get (nameof Group.InitialCues) world
        member this.SetInitialCues (value : Cue FDeque) world = this.Set (nameof Group.InitialCues) value world
        member this.InitialCues = lens (nameof Group.InitialCues) this this.GetInitialCues this.SetInitialCues
