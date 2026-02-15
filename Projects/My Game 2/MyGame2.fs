namespace MyGame2

open Prime
open Nu
open MyGame2

// this is the dispatcher that customizes the top-level behavior of our game.
type MyGame2Dispatcher () =
    inherit GameDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [ define Game.ScreenTag Splash
          define Game.Advents FDeque.empty ]

    // here we define the game's top-level behavior
    override this.Process (game, world) =

        // declare splash screen
        let behavior = Slide (Constants.Dissolve.Default, Constants.Slide.Default, None, Simulants.Title)
        let results = World.beginScreen Simulants.Splash.Name (game.GetScreenTag world = Splash) behavior [] world
        if FQueue.contains Deselecting results && game.GetScreenTag world = Splash then game.SetScreenTag Title world
        World.endScreen world

        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreenWithGroupFromFile
            Simulants.Title.Name
            (game.GetScreenTag world = Title)
            behavior
            "Assets/Gui/Title.nugroup"
            []
            world |> ignore

        World.beginGroup "Gui" [] world

        if World.doButton "Play" [] world then
            let noZoneScreen = Game / (Zone.toName NoZone)
            noZoneScreen.SetCues (FDeque.singleton Progression.initialCue) world
            game.SetScreenTag (Zone NoZone) world

        if World.doButton "Credits" [] world then game.SetScreenTag Credits world
        if World.doButton "Exit" [] world && world.Unaccompanied then World.exit world
        World.endGroup world
        World.endScreen world

        let screenTag = game.GetScreenTag world
        let zones = Zone.all

        for zone in zones do
            let song = Zone.toSong zone
            let zoneName = Zone.toName zone
            let songDescriptor =
                song |> Option.map (fun song ->
                { FadeInTime = GameTime.zero
                  FadeOutTime = Constants.Audio.FadeOutTimeDefault
                  StartTime = 0L
                  RepeatLimitOpt = None
                  Volume = 1.0f
                  Song = song })

            let isCurrentZone = ScreenTag.isZone zone screenTag
            let behavior = Dissolve (Constants.Dissolve.Default, songDescriptor)
            do World.doScreen<ZoneDispatcher> zoneName isCurrentZone behavior [ Screen.Zone .= zone ] world |> ignore

        // declare credits screen
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        World.beginScreenWithGroupFromFile Simulants.Credits.Name (game.GetScreenTag world = Credits) behavior "Assets/Gui/Credits.nugroup" [] world |> ignore
        World.beginGroup "Gui" [] world
        if World.doButton "Back" [] world then game.SetScreenTag Title world
        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if World.isKeyboardAltDown world &&
           World.isKeyboardKeyDown KeyboardKey.F4 world &&
           world.Unaccompanied then
           World.exit world
