namespace MyGame3

open System.Numerics
open Prime
open Nu

// a cue is a single scripted action, following the OmniBlade pattern.
// instant ops complete immediately; time-based ops have a corresponding *State variant
// that holds in-progress data. Sequence and Parallel provide control flow.
type Cue =
    | Fin
    // === Instant ops ===
    | Print of string
    | AddActor of Character:Character * Zone:Zone * SpawnPoint:string
    | RemoveActor of Character:Character
    | AddAdvent of Advent
    | EnableCameraFollow
    | DisableCameraFollow
    | EnableAvatarMovement
    | DisableAvatarMovement
    // === Time-based ops ===
    | Wait of Duration:single
    | WaitState of EndTime:GameTime
    | FadeOut of Duration:single
    | FadeOutState of InitialFade:single * StartTime:GameTime * EndTime:GameTime
    | FadeIn of Duration:single
    | FadeInState of InitialFade:single * StartTime:GameTime * EndTime:GameTime
    | FlyCamera of Path:string * Speed:single
    | FlyCameraState of Points:Vector3 array * Rotations:Quaternion array * DurationSeconds:single * StartTime:GameTime
    | Exposit of Text:string * Variant:ExpositVariant
    | ExpositState
    | Animate of Character:Character * Name:string * TargetWeight:single * Duration:single
    | AnimateState of Character:Character * Name:string * InitialWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
    | CrossFade of Character:Character * From:string * To:string * TargetWeight:single * Duration:single
    | CrossFadeState of Character:Character * From:string * To:string * InitialFromWeight:single * InitialToWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
    | Morph of Character:Character * MorphIndex:int * TargetWeight:single * Duration:single
    | MorphState of Character:Character * MorphIndex:int * InitialWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
    | CrossMorph of Character:Character * From:int * To:int * TargetWeight:single * Duration:single
    | CrossMorphState of Character:Character * From:int * To:int * InitialFromWeight:single * InitialToWeight:single * TargetWeight:single * StartTime:GameTime * EndTime:GameTime
    // === Control flow ===
    | Sequence of Cue FDeque
    | Parallel of Cue FDeque
    | Fork of Cue
    | If of Advent * Then:Cue * Else:Cue
    | Await of Advent