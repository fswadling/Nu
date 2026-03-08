namespace MyGame3

open Prime
open Nu

// a cue is a single scripted action, following the OmniBlade pattern.
// instant ops complete immediately; time-based ops have a corresponding *State variant
// that holds in-progress data. Sequence and Parallel provide control flow.
type Cue =
    | Fin
    // === Instant ops ===
    | Print of string
    | AddActor of Character:Character * SpawnPoint:string
    | RemoveActor of Character:Character
    | Exposit of Text:string * Variant:ExpositVariant
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
    // === Control flow ===
    | Sequence of Cue FDeque
    | Parallel of Cue FDeque
    | Fork of Cue
    | If of Advent * Then:Cue * Else:Cue
    | Await of Advent