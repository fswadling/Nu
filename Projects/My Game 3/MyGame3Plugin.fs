namespace MyGame3
open System
open Nu
open MyGame3

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type MyGame3Plugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetMyGame3 Splash world)
             ("Title", fun world -> Game.SetMyGame3 Title world)
             ("Credits", fun world -> Game.SetMyGame3 Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.initial world
                Game.SetMyGame3 Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]