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
    // === Time-based ops ===
    | Wait of Duration:single
    | WaitState of EndTime:GameTime
    // === Control flow ===
    | Sequence of Cue FDeque
    | Parallel of Cue FDeque

[<RequireQualifiedAccess>]
module Cue =
    let isFin = function Fin -> true | _ -> false
    let notFin = function Fin -> false | _ -> true
