namespace MMCCGame
open System
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type MMCCGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Splash", fun world -> Game.SetMMCCGame Splash world)
             ("Title", fun world -> Game.SetMMCCGame Title world)
             ("Credits", fun world -> Game.SetMMCCGame Credits world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay Gameplay.empty world
                Game.SetMMCCGame Gameplay world)]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [MMCCGame.Assets.Gui.PackageName
         MMCCGame.Assets.Gameplay.PackageName]