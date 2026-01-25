namespace MyGame2

[<AutoOpen>]
module CharacterModule =
    type Character =
        | Placeholder

    [<RequireQualifiedAccess>]
    module Character =
        let toName (x: 'a) =
            let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(x, typeof<'a>)
            case.Name

        let toPath = function
            | Placeholder -> ""

        let getEmoteHeight = function
            | Placeholder -> 1.664f
