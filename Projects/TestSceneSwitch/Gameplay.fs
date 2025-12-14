namespace TestSceneSwitch
open System
open System.Numerics
open Prime
open Nu
open TestSceneSwitch

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the Gameplay model as well as the Quit event.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

        member this.GetSceneName world : string = this.Get (nameof Screen.SceneName) world
        member this.SetSceneName (value : string) world = this.Set (nameof Screen.SceneName) value world
        member this.SceneName = lens (nameof Screen.SceneName) this this.GetSceneName this.SetSceneName

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [define Screen.SceneName "Scene"
         define Screen.GameplayState Quit]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =
        let sceneName = screen.GetSceneName world
        // begin scene declaration
        World.beginGroupFromFile sceneName "Assets/Gameplay/Scene.nugroup" [] world

        //// declare quit button
        //if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
        //    screen.SetGameplayState Quit world
        //
        //// ensure game is unpaused when quitting
        //if screen.GetGameplayState world = Quit then
        //    World.setAdvancing true world

        // end scene declaration
        World.endGroup world