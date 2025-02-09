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
        let world = World.beginGroup "TextCrawlGroup" [] world
        let world = 
            World.doLabel 
                "Text" 
                [ Entity.Position .= v3 0.0f 0.0f 0.0f
                  Entity.Text @= text
                  Entity.Size @= labelSize ]
                world

        let world = World.endGroup world

        let world =
            if FQueue.contains Select results then
                let gameTime = World.getGameTime world
                let world = screen.SetSelectTime gameTime world
                let world = screen.SetAwaitingProgression true world
                world
            else
                world

        let isSelected = screen.GetSelected world
        let isAwaitingProgression = screen.GetAwaitingProgression world

        if (not isSelected) || (not isAwaitingProgression) then
            world
        else

        let selectTime = screen.GetSelectTime world
        let time = World.getGameTime world
        
        let isTimerElapsed = time - selectTime > GameTime.ofSeconds 3.0f
        let keyPressed =
            World.isKeyboardKeyPressed KeyboardKey.Space world
            || World.isKeyboardKeyPressed KeyboardKey.Escape world
            || World.isKeyboardKeyPressed KeyboardKey.Enter world

        if (not isTimerElapsed && not keyPressed) then
            world
        else

        let world = screen.SetAwaitingProgression false world
        let progression = Game.GetProgression world
        let coreGameState = Progression.doTextCrawl progression
        let world = Game.SetProgression coreGameState world
        let isTextCrawlScreen1InUse = Game.GetIsTextCrawlScreen1InUse world
        let world = Game.SetIsTextCrawlScreen1InUse (not isTextCrawlScreen1InUse) world
        world

type MainMenuDispatcher () =
    inherit ScreenDispatcherImSim ()

    static member Properties =
        [define Screen.MainMenu Title]

    override this.Process (_, screen, world) =
        let menuState = screen.GetMainMenu world
        let world = World.beginGroup "MainMenuGroup" [] world
        let world = 
            match menuState with
            | Title -> 
                let world =
                    World.doLabel 
                        "Title" 
                        [ Entity.Position .= v3 0.0f 64.0f 0.0f
                          Entity.Text .= "Echoes of Elaria"
                          Entity.Size .= v3 140f 32f 0f ]
                        world
                let world = 
                    World.doLabel 
                        "Subtitle" 
                        [ Entity.Position .= v3 0.0f 40.0f 0.0f
                          Entity.Text .= "The Crystals of Destiny"
                          Entity.Size .= v3 200f 32f 0f ]
                        world
                let clicked, world = 
                    World.doButton 
                        "StartGameButton"
                        [ Entity.Text .= "Start Game" ]
                        world

                let world =
                    if clicked then
                        let progressionState = Game.GetProgression world
                        let nextState = Progression.doEvent progressionState Progression.StartGame
                        let world = Game.SetProgression nextState world
                        world
                    else world

                let clicked, world =
                    World.doButton
                        "LoadGameButton"
                        [ Entity.Text .= "Load Game"
                          Entity.Position .= v3 0.0f -40.0f 0.0f ]
                        world

                let world =
                    if clicked 
                    then screen.SetMainMenu Load world
                    else world

                world
            | Load ->
                 let slotsState = Persistence.getSlotsState ()
                 let getSlotText = Persistence.getSlotText slotsState
                 let world = World.doLabel "LoadTitle" [ Entity.Text .= "Load Game"; Entity.Position .= v3 0f 140f 0f ] world
                 let clicked, world = World.doButton "MainMenuBack" [ Entity.Text .= "Back"; Entity.Position .= v3 0f 100f 0f ] world
                 let world =
                    if clicked then screen.SetMainMenu Title world
                    else world

                 let loadGame slot world =
                    let loadedState = Persistence.load slot
                    let progression = Persistence.toProgression loadedState
                    let world = Game.SetProgression progression world
                    let zone, position, rotation = loadedState.Location
                    let world = Simulants.Explore.SetZone zone world
                    let world = Simulants.PlayerCharacter.SetPosition position world
                    let world = Simulants.PlayerCharacter.SetRotation rotation world
                    world

                 let clicked, world = World.doButton "MainMenuSlot1" [ Entity.Text .= getSlotText Persistence.Slot1; Entity.Position .= v3 0f 60f 0f ] world

                 let world =
                     if clicked 
                     then loadGame Persistence.Slot1 world
                     else world

                 let clicked, world = World.doButton "MainMenuSlot2" [ Entity.Text .= getSlotText Persistence.Slot2; Entity.Position .= v3 0f 20f 0f ] world

                 let world =
                     if clicked 
                     then loadGame Persistence.Slot2 world
                     else world

                 let clicked, world = World.doButton "MainMenuSlot3" [ Entity.Text .= getSlotText Persistence.Slot3; Entity.Position .= v3 0f -20f 0f ] world

                 let world =
                     if clicked 
                     then loadGame Persistence.Slot3 world
                     else world

                 let clicked, world = World.doButton "MainMenuSlot4" [ Entity.Text .= getSlotText Persistence.Slot4; Entity.Position .= v3 0f -60f 0f ] world

                 let world =
                     if clicked 
                     then loadGame Persistence.Slot4 world
                     else world

                 world

        World.endGroup world

type GameQuest3DDispatcher () =
    inherit GameDispatcherImSim ()

    // This works by alternating between two text crawl screens.
    // That way you get a nice fade in and fade out effect. 
    // Feels a bit hacky but it works.
    let doTextCrawlScreens (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let progressionState = game.GetProgression world
        let text = Progression.toTextCrawl progressionState
        let isText = Progression.isTextCrawl progressionState
        let isScreen1InUse = game.GetIsTextCrawlScreen1InUse world

        let _, world = 
            World.beginScreen<TextCrawlScreenDispatcher>
                Simulants.TextCrawl1.Name
                (isText && isScreen1InUse)
                behavior
                [ if isScreen1InUse && isText then
                    Screen.TextCrawl @= text ]
                world

        let world = World.endScreen world

        let _,world = 
            World.beginScreen<TextCrawlScreenDispatcher>
                Simulants.TextCrawl2.Name
                (isText && not isScreen1InUse)
                behavior
                [ if not isScreen1InUse && isText then
                    Screen.TextCrawl @= text ]
                world

        let world = World.endScreen world

        world

    let doMainMenu (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let progressionState = game.GetProgression world
        let isMainMenu = Progression.isMainMenu progressionState
        let _, world = World.beginScreen<MainMenuDispatcher> Simulants.MainMenu.Name isMainMenu behavior [] world
        let world = World.endScreen world
        world

    let doExplore (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.FieldSong)
        let progressionState = game.GetProgression world
        let isExplore = Progression.isExplore progressionState
        let _, world = World.beginScreen<ExploreScreenDispatcher> Simulants.Explore.Name isExplore behavior [] world
        let world = World.endScreen world
        world

    let doBattle (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, Some Assets.Gameplay.FightSong)
        let progression = game.GetProgression world
        let isBattle = Progression.isBattle progression
        let battle = Progression.toBattle progression
        let events, world = World.beginScreen<BattleScreenDispatcher> Simulants.Battle.Name isBattle behavior [] world
        let world = World.endScreen world
        let world =
            if FQueue.contains Select events 
            then Simulants.Battle.SetBattleState (BattleState.init battle) world
            else world
        world

    let doGameOverScreen (game: Game) world =
        let behavior = Dissolve (Constants.Dissolve.Default, None)
        let gameState = game.GetProgression world
        let isGameOver = Progression.isGameOver gameState
        let _, world = World.beginScreen Simulants.GameOver.Name isGameOver behavior [] world
        let world = World.beginGroup "GameOverGroup" [] world
        let world =
            World.doLabel 
                "GameOverText" 
                [ Entity.Position .= v3 0.0f 0.0f 0.0f
                  Entity.Text @= "Game Over"
                  Entity.Size @= v3 200f 32f 0f ]
                world

        let world = World.endGroup world
        let world = World.endScreen world
        world

    static member Properties =
        [ nonPersistent Game.Progression Progression.initial
          define Game.IsTextCrawlScreen1InUse true ]

    override this.Process (myGame, world) =
        let world = doTextCrawlScreens myGame world
        let world = doMainMenu myGame world
        let world = doExplore myGame world
        let world = doBattle myGame world
        let world = doGameOverScreen myGame world
        world