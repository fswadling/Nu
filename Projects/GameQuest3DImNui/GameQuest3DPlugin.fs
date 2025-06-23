namespace GameQuest3DImNui

open Nu
open GameQuest3DImNui

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofList
            [("Start",
                fun world -> 
                    do Game.SetIsTextCrawlScreen1InUse true world
                    do Game.SetProgression Progression.initial world
                    ())]

    // this specifies which packages are automatically loaded at game start-up.
    override this.InitialPackages =
        [Assets.Gui.PackageName
         Assets.Gameplay.PackageName]