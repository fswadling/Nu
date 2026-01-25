namespace MyGame2

open Nu
open MyGame2

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGame2Plugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetScreenTag Splash world)
             ("Title", fun world -> Game.SetScreenTag Title world)
             ("Credits", fun world -> Game.SetScreenTag Credits world)
             ("NoZone", fun world -> Game.SetScreenTag (Zone NoZone) world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName]
