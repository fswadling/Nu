namespace MyGame2

open System.Numerics

[<AutoOpen>]
module ScenarioModule =
    type Scenario =
        | Placeholder

    [<RequireQualifiedAccess>]
    module Scenario =
        let FastForwardFactor = 0.1f
        let ExpositAppearDuration = 0.3
        let ExpositDefaultSize = v2 400.0f 60.0f
        let ExpositDefaultPosition = v2 0f 140f
        let ExpositAutoDuration = 3f
        let ExpositFastForwadDuration = ExpositAutoDuration * FastForwardFactor
        let DialogueDefaultSize = v2 400.0f 120.0f
        let DialogueDefaultPosition = v2 0f 136f
        let PromptDefaultSize = v2 300.0f 40.0f
        let PromptDefaultPosition = v2 0f -140f

        let toName (x: 'a) =
            let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>)
            case.Name

        let toPath = function
            | Placeholder -> ""

[<AutoOpen>]
module ZoneModule =
    type Zone =
        | NoZone

    [<RequireQualifiedAccess>]
    module Zone =
        let main = "Main"

        let all = 
            [ NoZone ]

        let toName (x: 'a) =
            let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>)
            case.Name

        let toPath = function
            | NoZone -> None

        let toSong = function
            | NoZone -> None
