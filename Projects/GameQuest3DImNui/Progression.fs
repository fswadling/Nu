namespace GameQuest3DImNui
open StateMachine
open Nu
open Prime

[<AutoOpen>]
module Progression =
    type ProgressionEvent =
        | Nil
        | TextCrawlDone of TextCrawl
        | StartGame
        | LoadGame
        | CompanionRecruited of Companion
        | CrystalObtained of Crystal
        | BattleDone of Tag

    type InteractionEvent =
        | Respond of Response
        | ProgressionDone

    type InteractionState =
        | Prompt of Prompt * Response array
        | Progress of ProgressionEvent

    type Interaction = StateMachine<InteractionEvent, InteractionState, unit>

    module private Interaction =
        let prompt (text: Prompt,options: Response array) =
            (Prompt (text,options))
            |> StateMachine.untilChoose
                (function
                | Respond ans when Array.contains ans options -> Some ans
                | _ -> None)

        let progression (progressionEvent: ProgressionEvent) =
            (Progress progressionEvent)
            |> StateMachine.untilChoose
                (function
                | ProgressionDone -> Some ()
                | _ -> None)

        let recruitConversation (companion: Companion):  Interaction = 
            stateMachine {
                let! answer = prompt ("Hi there! Want to team up?", [| "Sure do!"; "Nah" |])

                let! answer =
                    if answer = "Sure do!" then
                        prompt ("Glad to here it!", [| "Let's go!" |])
                    else
                        prompt ("You'll be back!", [| "Goodbye" |])

                do!
                    if answer = "Let's go!"
                    then progression (CompanionRecruited companion)
                    else Return ()
            }

        let crystalInteraction (crystal: Crystal) = 
            stateMachine {
                let! answer = prompt ($"You found the {crystal} crystal", [| "Take it"; "Leave" |])

                let! _ =
                    if answer = "Take it" then
                        prompt ($"You have taken the {crystal} crystal", [| "Leave" |])
                    else
                        prompt ("You leave the crystal alone", [| "Leave" |])

                do!
                    if answer = "Take it"
                    then progression (CrystalObtained crystal)
                    else Return ()
            }

    type PositionedActor =
        { Actor: Actor
          Location: Location
          Interaction: Interaction option }

    type ExploreState =
        { PositionedActors: PositionedActor array
          InvisibleWalls: (Tag * Zone * Position * Radius) array }

    [<AutoOpen>]
    module private ExploreState =
        let empty =
            { PositionedActors = Array.empty
              InvisibleWalls = Array.empty }

        let withActors positionedActors =
            { empty with PositionedActors = positionedActors }

    type Battle =
        { Companions: Companion array
          Enemies: Enemy array
          BattleTag: Tag }

    module Battle =
        let empty =
            { Companions = Array.empty; 
              Enemies = Array.empty;
              BattleTag = "" }

    type ScreenState =
        | TextCrawl of TextCrawl * LabelSize
        | MainMenu
        | Explore of ExploreState
        | Battle of Battle
        | GameOver

    type ProgressionState =
        { State: ScreenState
          TextCrawl: TextCrawl * LabelSize
          Explore: ExploreState
          Battle: Battle }

    module ProgressionState =
        let empty = 
            { State = GameOver
              TextCrawl = "", v3Zero
              Explore = ExploreState.empty
              Battle = Battle.empty }

        let fold folded state =
            match state with
            | TextCrawl (prompt, answers) ->
                { folded with TextCrawl = (prompt, answers); State = state }
            | Explore explore ->
                { folded with Explore = explore; State = state }
            | Battle battle ->
                { folded with Battle = battle; State = state }
            | _ ->
                { folded with State = state}

    type private StatefulEvent = 
        { Event: ProgressionEvent
          CompanionsRecruited: Companion Set
          CrystalsCollected: Crystal Set }

    [<AutoOpen>]
    module private StatefulEvent =
        let empty =
            { Event = Nil
              CompanionsRecruited = Set.empty
              CrystalsCollected = Set.empty }

        let fold
            (coreEvent: StatefulEvent)
            (event: ProgressionEvent) : StatefulEvent =
            match event with
            | CompanionRecruited companion -> 
                { coreEvent with
                    Event = event;
                    CompanionsRecruited =
                        Set.add
                            companion
                            coreEvent.CompanionsRecruited }
            | CrystalObtained crystal ->
                { coreEvent with 
                    Event = event
                    CrystalsCollected =
                        Set.add
                            crystal
                            coreEvent.CrystalsCollected }
            | _ ->
                { coreEvent with Event = event }

        let isRecruited companion foldedEvent =
            match foldedEvent.Event with
            | CompanionRecruited c when c = companion -> true
            | _ -> false

        let crystalIsObtained crystal foldedEvent =
            match foldedEvent.Event with
            | CrystalObtained c when c = crystal -> true
            | _ -> false

    let private textCrawl text size =
        TextCrawl (text, size)
        |> until
            (function
            | { Event = TextCrawlDone txt }
                when txt = text -> true
            | _ -> false)

    let private textCrawlWithState text size =
        TextCrawl (text, size)
        |> untilChoose
            (function
            | { Event = TextCrawlDone txt
                CompanionsRecruited = companions
                CrystalsCollected = crystals }
                when txt = text -> Some (companions, crystals)
            | _ -> None)

    let private battle battleState =
        Battle battleState
        |> until
            (function 
            | { Event = BattleDone tag } when tag = battleState.BattleTag -> true
            | _ -> false)

    let private combineExplore
        (left: ScreenState)
        (right: ScreenState): ScreenState =
        match left, right with
        | Explore accumulated, Explore exploreState -> 
            let positionedActors = 
                Array.append
                    accumulated.PositionedActors
                    exploreState.PositionedActors

            let invisibleWalls = 
                Array.append
                    accumulated.InvisibleWalls
                    exploreState.InvisibleWalls

            let exploreState = 
                { PositionedActors = positionedActors;
                  InvisibleWalls = invisibleWalls }

            Explore exploreState
        | _, Explore _ -> left
        | Explore _, _ -> right
        | _ -> left

    let private exploreStateMachine =
        ParallelStateMachineBuilder<ScreenState>(combineExplore)

    let private crystalFoundText
        (crystal: Crystal)
        (crystalsSoFar: Crystal Set)
        (companionsSoFar: Companion Set) =
        match Seq.length crystalsSoFar with
        | 1 ->
            if (Set.contains BlueWitch companionsSoFar)
            then Some $"""Blue witch: "We found the {crystal.ToString()} crystal!" """
            else None
        | 2 ->
            if (Set.contains Knight companionsSoFar)
            then Some $"""Knight: "We found the {crystal.ToString()} crystal!" """
            else None
        | 3 ->
            if (Set.contains Thief companionsSoFar)
            then Some $"""Thief: "We found the {crystal.ToString()} crystal!" """
            else None
        | _ -> None

    let private crystalFoundStateMachine
        (crystal: Crystal)
        (crystalsSoFar: Crystal Set)
        (companionsSoFar: Companion Set) = 
        stateMachine {
            let crystalFoundText =
                crystalFoundText
                    crystal
                    crystalsSoFar
                    companionsSoFar

            do!
                match crystalFoundText with
                | Some crystalFoundText -> 
                    textCrawl crystalFoundText (v3 400f 32f 0f)
                | None ->
                    Return ()
        }

    let withInvisibleWalls invisibleWalls = function 
        | Explore e -> Explore { e with InvisibleWalls = invisibleWalls }
        | e -> e

    let initial =
        stateMachine {
            do! textCrawl "SwadTech Games Present" (v3 200f 32f 0f)

            do! textCrawl "Some more text" (v3 130f 32f 0f)

            do! MainMenu |> until (fun { Event = e } -> e.IsStartGame)

            do! textCrawl "A long time ago" (v3 130f 32f 0f)

            do! textCrawl "There was a guy" (v3 130f 32f 0f)

            do! battle
                    { Companions = [| Hero |]
                      Enemies = [| Horns |]
                      BattleTag = "FirstBattle" }

            let blueWitch =
                { Actor = Companion BlueWitch
                  Location = SideZone.witchLocation
                  Interaction =
                    Some (Interaction.recruitConversation BlueWitch) }

            let knight =
                { Actor = Companion Knight
                  Location = SideZone2.knightLocation
                  Interaction =
                    Some (Interaction.recruitConversation Knight) }

            let thief =
                { Actor = Companion Thief 
                  Location = SideZone3.thiefLocation
                  Interaction =
                    Some (Interaction.recruitConversation Thief) }

            do! exploreStateMachine {
                    let! _ = 
                        stateMachine {
                            do! Explore (withActors [| blueWitch |])
                                |> until (BlueWitch |> isRecruited)

                            do! textCrawl "Blue witch was recruited!" (v3 200f 32f 0f)
                        }

                    and! _ =
                        stateMachine {
                            do! Explore (withActors [| knight |])
                                |> until (Knight |> isRecruited)

                            do! textCrawl "Knight was recruited!" (v3 200f 32f 0f)
                        }

                    and! _ =
                        stateMachine {
                            do! Explore (withActors [| thief |])
                                |> until (Thief |> isRecruited)

                            do! textCrawl "Thief was recruited!" (v3 200f 32f 0f)
                        }

                    return ()
                }
                |> StateMachine.mapState
                    (withInvisibleWalls StartingZone.crystalsInvisibleWalls)

            let fireCrystal =
                { Actor = Crystal Fire
                  Location = CrystalsHub.fireCrystalLocation
                  Interaction = Some (Interaction.crystalInteraction Fire) }

            let waterCrystal =
                { Actor = Crystal Water
                  Location = CrystalZone1.waterCrystalLocation
                  Interaction = Some (Interaction.crystalInteraction Water) }

            let airCrystal =
                { Actor = Crystal Air
                  Location = CrystalZone1.airCrystalLocation
                  Interaction = Some (Interaction.crystalInteraction Air) }

            let earthCrystal =
                { Actor = Crystal Earth 
                  Location = CrystalZone2.earthCrystalLocation
                  Interaction = Some (Interaction.crystalInteraction Earth)  }

            do! exploreStateMachine {
                    let! _ = 
                        stateMachine {
                            do! Explore (withActors [| fireCrystal |])
                                |> until (Fire |> crystalIsObtained)

                            let! companionsSoFar, crystalsSoFar =
                                textCrawlWithState
                                    "Fire crystal found!"
                                    (v3 200f 32f 0f)

                            do! crystalFoundStateMachine
                                    Fire
                                    crystalsSoFar
                                    companionsSoFar
                        }

                    and! _ = 
                        stateMachine {
                            do! Explore (withActors [| waterCrystal |])
                                |> until (Water |> crystalIsObtained)

                            let! companionsSoFar, crystalsSoFar =
                                textCrawlWithState
                                    "Water crystal found!"
                                    (v3 200f 32f 0f)

                            do! crystalFoundStateMachine
                                    Water
                                    crystalsSoFar
                                    companionsSoFar
                        }

                    and! _ = 
                        stateMachine {
                            do! Explore (withActors [| airCrystal |])
                                |> until (Air |> crystalIsObtained)

                            let! companionsSoFar, crystalsSoFar =
                                textCrawlWithState
                                    "Air crystal found!"
                                    (v3 200f 32f 0f)

                            do! crystalFoundStateMachine
                                    Air
                                    crystalsSoFar
                                    companionsSoFar
                        }

                    and! _ = 
                        stateMachine {
                            do! Explore (withActors [| earthCrystal |])
                                |> until (Earth |> crystalIsObtained)

                            let! companionsSoFar, crystalsSoFar =
                                textCrawlWithState
                                    "Earth crystal found!"
                                    (v3 200f 32f 0f)

                            do! crystalFoundStateMachine
                                    Earth
                                    crystalsSoFar
                                    companionsSoFar
                        }

                    return ()
                }
                |> StateMachine.mapState
                       (withInvisibleWalls CrystalsHub.finalAreaInvisibleWalls)

            do! battle
                    { Companions = 
                        [| Hero
                           BlueWitch
                           Knight
                           Thief |]
                      Enemies = [| Horns |]
                      BattleTag = "LastBattle" }

            do! GameOver |> forever
        }
        |> StateMachine.unfoldEvent StatefulEvent.fold StatefulEvent.empty
        |> StateMachine.foldState ProgressionState.empty ProgressionState.fold

    

[<AutoOpen>]
module MyGameExtensions =
    let rec private doInteraction (interaction: Interaction) state =
        match state, interaction with
        | (events, Yield (_, progression)),
          (Yield (Progress event, interactionContinuation)) ->
            let state = (FQueue.conj event events), progression event
            let interaction = interactionContinuation ProgressionDone
            doInteraction interaction state
        | _ -> interaction, state

    type Game with
        member this.GetProgressionEvents world : ProgressionEvent FQueue = this.Get (nameof Game.ProgressionEvents) world
        member this.SetProgressionEvents (value : ProgressionEvent FQueue) (world: World) = this.Set (nameof Game.ProgressionEvents) value world
        member this.ProgressionEvents = lens (nameof Game.ProgressionEvents) this this.GetProgressionEvents this.SetProgressionEvents

        member this.GetProgressionState world : ProgressionState = this.Get (nameof Game.ProgressionState) world
        member this.SetProgressionState (value : ProgressionState) (world: World) = this.Set (nameof Game.ProgressionState) value world
        member this.ProgressionState = lens (nameof Game.ProgressionState) this this.GetProgressionState this.SetProgressionState

        member this.DoProgressionEvent event world : unit =
            let events = this.GetProgressionEvents world
            let newEvents = FQueue.conj event events
            let newStateMachine = StateMachine.zip newEvents Progression.initial
            let state = StateMachine.toState newStateMachine
            do this.SetProgressionEvents newEvents world
            do this.SetProgressionState state world

        member this.DoProgressionWithInteraction (interaction: Interaction) world: Interaction =
            let events = this.GetProgressionEvents world
            let newStateMachine = StateMachine.zip events Progression.initial
            let newInteraction, (newEvents, newStateMachine) = doInteraction interaction (events, newStateMachine)
            let newState = StateMachine.toState newStateMachine
            do this.SetProgressionEvents newEvents world
            do this.SetProgressionState newState world
            newInteraction
