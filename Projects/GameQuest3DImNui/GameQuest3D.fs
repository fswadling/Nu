namespace GameQuest3DImNui

open Nu
open Prime
open GameQuest3DImNui

type MainMenu =
    | Title
    | Load

[<AutoOpen>]
module Extensions =
    type Screen with
        member this.GetTextCrawl world : (string * LabelSize) = this.Get (nameof Screen.TextCrawl) world
        member this.SetTextCrawl (value : (string * LabelSize)) world = this.Set (nameof Screen.TextCrawl) value world
        member this.TextCrawl = lens (nameof Screen.TextCrawl) this this.GetTextCrawl this.SetTextCrawl

        member this.SetSelectTime (value : GameTime) world = this.Set (nameof Screen.SelectTime) value world
        member this.GetSelectTime world : GameTime = this.Get (nameof Screen.SelectTime) world
        member this.SelectTime = lens (nameof Screen.SelectTime) this this.GetSelectTime this.SetSelectTime

        member this.GetAwaitingProgression world : bool = this.Get (nameof Screen.AwaitingProgression) world
        member this.SetAwaitingProgression (value : bool) world = this.Set (nameof Screen.AwaitingProgression) value world
        member this.AwaitingProgression = lens (nameof Screen.AwaitingProgression) this this.GetAwaitingProgression this.SetAwaitingProgression

        member this.GetMainMenu world : MainMenu = this.Get (nameof Screen.MainMenu) world
        member this.SetMainMenu (value : MainMenu) world = this.Set (nameof Screen.MainMenu) value world
        member this.MainMenu = lens (nameof Screen.MainMenu) this this.GetMainMenu this.SetMainMenu

    type Game with
        member this.GetIsTextCrawlScreen1InUse world : bool = this.Get (nameof Game.IsTextCrawlScreen1InUse) world
        member this.SetIsTextCrawlScreen1InUse (value : bool) world = this.Set (nameof Game.IsTextCrawlScreen1InUse) value world
        member this.IsTextCrawlScreen1InUse = lens (nameof Game.IsTextCrawlScreen1InUse) this this.GetIsTextCrawlScreen1InUse this.SetIsTextCrawlScreen1InUse

type TextCrawlScreenDispatcher () =
    inherit ScreenDispatcherImSim ()

    static member Properties =
        [ define Screen.TextCrawl ("", v3 200f 32f 0f)
          define Screen.AwaitingProgression false
          define Screen.SelectTime GameTime.zero ]

    override this.Process (results, screen, world) =
        let text, labelSize = screen.GetTextCrawl world
        do World.beginGroup "TextCrawlGroup" [] world
        do World.doLabel 
              "Text" 
              [ Entity.Position .= v3 0.0f 0.0f 0.0f
                Entity.Text @= text
                Entity.Size @= labelSize ]
              world

        do World.endGroup world

        if FQueue.contains Select results then
            let gameTime = world.GameTime
            do screen.SetSelectTime gameTime world
            do screen.SetAwaitingProgression true world

        let isSelected = screen.GetSelected world
        let isAwaitingProgression = screen.GetAwaitingProgression world

        if (not isSelected) || (not isAwaitingProgression) then
            ()
        else

        let selectTime = screen.GetSelectTime world
        let time = world.GameTime
        
        let isTimerElapsed = time - selectTime > GameTime.ofSeconds 3.0f
        let keyPressed =
            World.isKeyboardKeyPressed KeyboardKey.Space world
            || World.isKeyboardKeyPressed KeyboardKey.Escape world
            || World.isKeyboardKeyPressed KeyboardKey.Enter world

        if (not isTimerElapsed && not keyPressed) then
            ()
        else

        do screen.SetAwaitingProgression false world
        do Game.DoProgressionEvent (TextCrawlDone text) world
        let isTextCrawlScreen1InUse = Game.GetIsTextCrawlScreen1InUse world
        do Game.SetIsTextCrawlScreen1InUse (not isTextCrawlScreen1InUse) world

type MainMenuDispatcher () =
    inherit ScreenDispatcherImSim ()

    static member Properties =
        [define Screen.MainMenu Title]

    override this.Process (_, screen, world) =
        let menuState = screen.GetMainMenu world
        do World.beginGroup "MainMenuGroup" [] world

        match menuState with
        | Title -> 
            do World.doLabel 
                   "Title" 
                   [ Entity.Position .= v3 0.0f 64.0f 0.0f
                     Entity.Text .= "Echoes of Elaria"
                     Entity.Size .= v3 140f 32f 0f ]
                   world
            do World.doLabel 
                   "Subtitle" 
                   [ Entity.Position .= v3 0.0f 40.0f 0.0f
                     Entity.Text .= "The Crystals of Destiny"
                     Entity.Size .= v3 200f 32f 0f ]
                   world
            let clicked = 
                World.doButton 
                    "StartGameButton"
                    [ Entity.Text .= "Start Game" ]
                    world

            if clicked then
                do Game.DoProgressionEvent Progression.StartGame world

            let clicked =
                World.doButton
                    "LoadGameButton"
                    [ Entity.Text .= "Load Game"
                      Entity.Position .= v3 0.0f -40.0f 0.0f ]
                    world

            if clicked 
            then do screen.SetMainMenu Load world

        | Load ->
            let slotsState = Persistence.getSlotsState ()
            let getSlotText = Persistence.getSlotText slotsState
            do World.doLabel "LoadTitle" [ Entity.Text .= "Load Game"; Entity.Position .= v3 0f 140f 0f ] world

            let clicked = World.doButton "MainMenuBack" [ Entity.Text .= "Back"; Entity.Position .= v3 0f 100f 0f ] world
            if clicked then do screen.SetMainMenu Title world

            let loadGame slot world =
               let loadedState = Persistence.load slot
               let progressionEvents, stateMachine = Persistence.toProgression loadedState
               let state = StateMachine.toState stateMachine
               do Game.SetProgressionEvents progressionEvents world
               do Game.SetProgressionState state world
               let zone, position, rotation = loadedState.Location
               do Simulants.Explore.SetZone zone world
               do Simulants.PlayerCharacter.SetPosition position world
               do Simulants.PlayerCharacter.SetRotation rotation world

            let clicked = World.doButton "MainMenuSlot1" [ Entity.Text .= getSlotText Persistence.Slot1; Entity.Position .= v3 0f 60f 0f ] world

            if clicked 
            then do loadGame Persistence.Slot1 world

            let clicked = World.doButton "MainMenuSlot2" [ Entity.Text .= getSlotText Persistence.Slot2; Entity.Position .= v3 0f 20f 0f ] world

            if clicked 
            then do loadGame Persistence.Slot2 world

            let clicked = World.doButton "MainMenuSlot3" [ Entity.Text .= getSlotText Persistence.Slot3; Entity.Position .= v3 0f -20f 0f ] world

            if clicked 
            then do loadGame Persistence.Slot3 world

            let clicked = World.doButton "MainMenuSlot4" [ Entity.Text .= getSlotText Persistence.Slot4; Entity.Position .= v3 0f -60f 0f ] world

            if clicked 
            then do loadGame Persistence.Slot4 world

        World.endGroup world

type GameQuest3DDispatcher () =
    inherit GameDispatcherImSim ()

    // This works by alternating between two text crawl screens.
    // That way you get a nice fade in and fade out effect. 
    // Feels a bit hacky but it works.
    let doTextCrawlScreens (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let state = game.GetProgressionState world
        let text = state.TextCrawl
        let isText = state.State.IsTextCrawl
        let isScreen1InUse = game.GetIsTextCrawlScreen1InUse world

        let _ = 
            World.beginScreen<TextCrawlScreenDispatcher>
                Simulants.TextCrawl1.Name
                (isText && isScreen1InUse)
                behavior
                [ if isScreen1InUse && isText then
                    Screen.TextCrawl @= text ]
                world

        do World.endScreen world

        let _ = 
            World.beginScreen<TextCrawlScreenDispatcher>
                Simulants.TextCrawl2.Name
                (isText && not isScreen1InUse)
                behavior
                [ if not isScreen1InUse && isText then
                    Screen.TextCrawl @= text ]
                world

        do World.endScreen world

    let doMainMenu (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let state = game.GetProgressionState world
        let isMainMenu = state.State.IsMainMenu
        let _ = World.beginScreen<MainMenuDispatcher> Simulants.MainMenu.Name isMainMenu behavior [] world
        do World.endScreen world

    let doExplore (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.FieldSong)
        let state = game.GetProgressionState world
        let isExplore = state.State.IsExplore
        let _ = World.beginScreen<ExploreScreenDispatcher> Simulants.Explore.Name isExplore behavior [] world
        do World.endScreen world

    let doBattle (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.FightSong)
        let state = game.GetProgressionState world
        let isBattle = state.State.IsBattle
        let battle = state.Battle
        let events = World.beginScreen<BattleScreenDispatcher> Simulants.Battle.Name isBattle behavior [] world
        do World.endScreen world
        if FQueue.contains Select events 
        then do Simulants.Battle.SetBattleState (BattleState.init battle) world

    let doGameOverScreen (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let state = game.GetProgressionState world
        let isGameOver = state.State.IsGameOver
        let _ = World.beginScreen Simulants.GameOver.Name isGameOver behavior [] world
        do World.beginGroup "GameOverGroup" [] world
        do World.doLabel 
               "GameOverText" 
               [ Entity.Position .= v3 0.0f 0.0f 0.0f
                 Entity.Text .= "Game Over"
                 Entity.Size .= v3 200f 32f 0f ]
               world
        do World.endGroup world
        do World.endScreen world

    static member Properties =
        let state = StateMachine.toState Progression.initial
        [ define Game.ProgressionEvents FQueue.empty
          define Game.ProgressionState state
          define Game.IsTextCrawlScreen1InUse true ]

    override this.Process (myGame, world) =
        do doTextCrawlScreens myGame world
        do doMainMenu myGame world
        do doExplore myGame world
        do doBattle myGame world
        do doGameOverScreen myGame world