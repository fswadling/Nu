namespace MyGame2

open Nu

[<AutoOpen>]
module InteractableFacetExtensions =
    type Entity with
        member this.GetInteractPromptMessage world : string = this.Get (nameof this.InteractPromptMessage) world
        member this.SetInteractPromptMessage (value : string) world = this.Set (nameof this.InteractPromptMessage) value world
        member this.InteractPromptMessage = lens (nameof this.InteractPromptMessage) this this.GetInteractPromptMessage this.SetInteractPromptMessage

        member this.GetInteractDistance world : single = this.Get (nameof this.InteractDistance) world
        member this.SetInteractDistance (value : single) world = this.Set (nameof this.InteractDistance) value world
        member this.InteractDistance = lens (nameof this.InteractDistance) this this.GetInteractDistance this.SetInteractDistance

        member this.GetInteractCue world : Cue option = this.Get (nameof this.InteractCue) world
        member this.SetInteractCue (value : Cue option) world = this.Set (nameof this.InteractCue) value world
        member this.InteractCue = lens (nameof this.InteractCue) this this.GetInteractCue this.SetInteractCue

        member this.GetInteractKey world : KeyboardKey = this.Get (nameof this.InteractKey) world
        member this.SetInteractKey (value : KeyboardKey) world = this.Set (nameof this.InteractKey) value world
        member this.InteractKey = lens (nameof this.InteractKey) this this.GetInteractKey this.SetInteractKey

type InteractableFacet () =
    inherit Facet (false, false, false)

    static member Properties =
        [define Entity.InteractPromptMessage "Press E to interact"
         define Entity.InteractDistance 3.0f
         define Entity.InteractCue None
         define Entity.InteractKey KeyboardKey.E ]