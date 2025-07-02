namespace GameQuest3DImNui
open Nu
open Prime
open System.Numerics
open StateMachine

type InteractionState =
    | NoInteraction
    | Inactive of PositionedActor * Interaction
    | Active of PositionedActor * Interaction

type MenuState =
    | SaveOrLoad
    | Save
    | Load

[<AutoOpen>]
module ExploreScreenExtensions =
    type Screen with
        member this.GetZone world : Zone = this.Get (nameof Screen.Zone) world
        member this.SetZone (value : Zone) world = this.Set (nameof Screen.Zone) value world
        member this.Zone = lens (nameof Screen.Zone) this this.GetZone this.SetZone

        member this.GetInteraction world : InteractionState = this.Get (nameof Screen.Interaction) world
        member this.SetInteraction (value : InteractionState) world = this.Set (nameof Screen.Interaction) value world
        member this.Interaction = lens (nameof Screen.Interaction) this this.GetInteraction this.SetInteraction

        member this.GetMenuState world : MenuState option = this.Get (nameof Screen.MenuState) world
        member this.SetMenuState (value : MenuState option) world = this.Set (nameof Screen.MenuState) value world
        member this.MenuState = lens (nameof Screen.MenuState) this this.GetMenuState this.SetMenuState

    type Entity with
        member this.GetIsMoving world : bool = this.Get (nameof Entity.IsMoving) world
        member this.SetIsMoving (value : bool) world = this.Set (nameof Entity.IsMoving) value world
        member this.IsMoving = lens (nameof Entity.IsMoving) this this.GetIsMoving this.SetIsMoving

type NPCDispatcher () =
    inherit Entity3dDispatcherImSim (true, false, false)

    static member Facets =
        [ typeof<RigidBodyFacet> ]

type TriggerVolumeDispatcher () = 
    inherit Entity3dDispatcherImSim (false, false, false)

    static member Radius = 1.0f

    static member Facets =
        [ typeof<RigidBodyFacet> ]

    static member Properties =
        [ define Entity.BodyShape (SphereShape { Radius = TriggerVolumeDispatcher.Radius; TransformOpt = None; PropertiesOpt = None })
          define Entity.Sensor true ]

type InvisibleWallDispatcher () =
    inherit Entity3dDispatcherImSim (true, false, false)

    static member Facets =
        [ typeof<RigidBodyFacet> ]

type PlayerDispatcher () =
    inherit Entity3dDispatcherImSim (true, false, false)

    let getAnimations (entity: Entity) world =
        let isMoving = entity.GetIsMoving world
        if isMoving
        then [| Animation.make 0L None "Armature|Running" Loop 1.0f 1f None |]
        else [| Animation.make 0L None "Armature|Idle" Loop 1.0f 1f None |]

    static member Facets =
        [ typeof<RigidBodyFacet> ]

    static member Properties =
        [ define Entity.IsMoving false
          define Entity.BodyType KinematicCharacter
          define Entity.BodyShape
            (CapsuleShape 
                { Height = 1.0f
                  Radius = 0.35f
                  TransformOpt = Some (Affine.makeTranslation (v3 0.0f 0.85f 0.0f))
                  PropertiesOpt = None }) ]

    override this.Process (entity, world) =
        World.doAnimatedModel
            "AnimatedModel"
            [ Entity.AnimatedModel .= Assets.Gameplay.MainCharacter
              Entity.Animations @= getAnimations entity world
              Entity.Size .= v3Dup 2.0f
              Entity.Offset .= v3 0.0f 1.0f 0.0f
              Entity.MaterialProperties .= { MaterialProperties.defaultProperties with IsToon = true } ]
            world

type ExploreScreenDispatcher () =
    inherit ScreenDispatcherImSim ()

    let playerWalkSpeed = 0.1f
    let playerTurnSpeed = 0.05f
    let _, initialPosition, initialRotation = Zones.initial

    let doCamera (world: World) =
        if not world.Advancing then
            ()
        else

        let playerRotation = Simulants.PlayerCharacter.GetRotation world
        let playerRotation = Quaternion.Concatenate(playerRotation, (Quaternion.CreateFromAxisAngle(v3Up, float32 Math.PI_MINUS_EPSILON)))
        let playerPostion = Simulants.PlayerCharacter.GetPosition world
        do World.setEye3dCenter (playerPostion + v3Up * 1.75f - playerRotation.Forward * 3.0f) world
        do World.setEye3dRotation playerRotation world

    let doPlayerMovement (world: World) =
        if not world.Advancing then
            ()
        else

        let rotation = Simulants.PlayerCharacter.GetRotation world
        let forward = rotation.Forward
        let walkDirection =
            (if World.isKeyboardKeyDown KeyboardKey.W world then -forward else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.S world then forward else v3Zero)
        let turnVelocity =
            (if World.isKeyboardKeyDown KeyboardKey.D world then -playerTurnSpeed else 0.0f) +
            (if World.isKeyboardKeyDown KeyboardKey.A world then playerTurnSpeed else 0.0f)
        let walkVelocity = walkDirection * playerWalkSpeed
        let position = (Simulants.PlayerCharacter.GetPosition world) + walkVelocity
        let rotation = Simulants.PlayerCharacter.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)
        do Simulants.PlayerCharacter.SetPosition position world
        do Simulants.PlayerCharacter.SetRotation rotation world
        do Simulants.PlayerCharacter.SetIsMoving (walkVelocity <> v3Zero) world

    let doTriggers (screen: Screen) world =
        let doTrigger (name, position, radius) =
            World.doEntityPlus<TriggerVolumeDispatcher,_>
                FQueue.empty
                World.initBodyResult
                name
                [ Entity.Position .= position; 
                  Entity.BodyShape .= SphereShape { Radius = radius; TransformOpt = None; PropertiesOpt = None } ]
                world

        let enqueuePenetration name queue result =
            match result with
            | BodyPenetrationData _ -> FQueue.conj name queue
            | _ -> queue

        let foldPenetrations results (name, position, radius) =
            let bodyResults = doTrigger (name, position, radius)
            let penetrations = FQueue.fold (enqueuePenetration name) results bodyResults
            penetrations

        let currentZone = screen.GetZone world
        let triggers = Zones.getTriggers currentZone
        let penetrations = Array.fold foldPenetrations FQueue.empty triggers

        match FQueue.tryHead penetrations with
        | None -> ()
        | Some triggerName ->

        let zone, position, rotation = Zones.getNextZone currentZone triggerName
        do screen.SetZone zone world
        do Simulants.PlayerCharacter.SetPosition position world
        do Simulants.PlayerCharacter.SetRotation rotation world

    let doActors (exploreState: ExploreState) (screen: Screen) world =
        let doActor (positionedActor: PositionedActor) =
            let _, position, rotation = positionedActor.Location
            let actorName = positionedActor.Actor.ToString()
            let asset = Actor.idleAsset positionedActor.Actor

            do World.beginEntity<NPCDispatcher>
                   actorName
                   // Quick hack because im bored and want to get this done
                   [ if asset.IsChoice1Of2 then
                         Entity.Scale .= v3Dup 5.0f
                         Entity.Size .= v3Dup 0.2f
                     Entity.Position .= position 
                     Entity.Rotation .= rotation ]
                   world

            do match asset with
               | Choice2Of2 (asset, animations) ->
                   do World.doAnimatedModel
                         "AnimatedModel" 
                         [ Entity.Size .= v3Dup 2.0f
                           Entity.Offset .= v3 0.0f 1.0f 0.0f
                           Entity.MaterialProperties .= { MaterialProperties.defaultProperties with IsToon = true }
                           Entity.Animations .= [| animations |]
                           Entity.AnimatedModel .= asset ]
                         world
               | Choice1Of2 asset ->
                   do World.doStaticModel
                         "StaticModel"
                         [ Entity.Size .= v3Dup 2.0f
                           Entity.Offset .= v3 0.0f 1.0f 0.0f
                           Entity.MaterialProperties .= MaterialProperties.defaultProperties
                           Entity.StaticModel .= asset ]
                         world

            do match positionedActor.Interaction with
               | Some interaction ->
                   let result =
                        World.doEntityPlus<TriggerVolumeDispatcher,_>
                           FQueue.empty
                           World.initBodyResult
                           "Trigger"
                           [ Entity.BodyShape .= 
                               SphereShape
                                   { Radius = TriggerVolumeDispatcher.Radius;
                                     TransformOpt = None; PropertiesOpt = None } ]
                           world

                   match FQueue.tryHead result with
                   | Some (BodyPenetrationData _) ->
                       do screen.SetInteraction (Inactive (positionedActor, interaction)) world
                   | Some (BodySeparationExplicitData _) -> 
                       do screen.SetInteraction NoInteraction world
                   | _ -> ()

               | None -> ()

            do World.endEntity world

        for actor in exploreState.PositionedActors do
            let actorZone = Location.getZone actor.Location
            let screenZone = screen.GetZone world
            if actorZone = screenZone then
                do doActor actor

    let doInvisibleWalls (exploreState: ExploreState) (screen: Screen) world =
        let currentZone = screen.GetZone world
        for (name, zone, position, radius) in exploreState.InvisibleWalls do
            if zone <> currentZone then
                ()
            else

            do World.doEntity<InvisibleWallDispatcher>
                  name
                  [ Entity.Position .= position
                    Entity.BodyShape .=
                      SphereShape
                          { Radius = radius
                            TransformOpt = None
                            PropertiesOpt = None } ]
                  world

    let doInteraction (screen: Screen) world =
        let interaction = screen.GetInteraction world
        match interaction with
        | Inactive (positionedActor, interaction) ->
            do World.doLabel
                  "InteractionPrompt"
                  [ Entity.Text .= "!";
                    Entity.PositionLocal .= v3 0f 120f 0f;
                    Entity.Size .= v3 10f 32f 0f ]
                  world

            if World.isKeyboardKeyDown KeyboardKey.Space world then 
                do screen.SetInteraction (Active (positionedActor, interaction)) world

        | Active (positionedActor, (Yield (Prompt (text, options), conversation))) ->
            let actorName = positionedActor.Actor.ToString()
            do World.beginPanel
                   "InteractionPanel"
                   [ Entity.BackdropImageOpt .= Some (Assets.Default.Image)
                     Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                     Entity.Position .= v3 0f 120f 0f
                     Entity.Size .= v3 500f 100f 0f
                     Entity.Color .= Color.Blue
                     Entity.LayoutMargin .= v2 3f 3f ] 
                   world

            do World.doText
                   "NPCName"
                   [ Entity.Text .= actorName
                     Entity.Size .= v3 500f 20f 0f ]
                   world

            do World.doText 
                   "Text" 
                   [ Entity.Text @= text
                     Entity.Size .= v3 500f 20f 0f
                     Entity.FontSizing .= Some 8 ]
                   world

            do World.beginPanel
                   "AnswersPanel"
                   [ Entity.Layout .= Layout.Flow (FlowDirection.FlowRightward, FlowLimit.FlowParent); 
                     Entity.Size .= v3 500f 40f 0f
                     Entity.BackdropImageOpt .= None ]
                   world

            for option in options do
                let clicked = 
                    World.doButton
                        option
                        [ Entity.Text .= option
                          Entity.Size .= v3 100f 20f 0f
                          Entity.FontSizing .= Some 8 ]
                        world

                if not clicked then
                    ()
                else

                let conversation = conversation (Respond option)
                do screen.SetInteraction (Active (positionedActor, conversation)) world

            do World.endPanel world
            do World.endPanel world

        | Active (_, (Return _)) ->
            do screen.SetInteraction NoInteraction world
        | Active (positionedActor, interaction) ->
            // Upon encountering a progression event in the interaction,
            // apply and move on.
            let progression = Game.GetProgression world

            let interaction, progressionState =
                Progression.doInteraction
                    interaction
                    progression

            let interactionState = Active (positionedActor, interaction)
            do Game.SetProgression progressionState world
            do screen.SetInteraction interactionState world
        | _ ->
            ()

    let doMenu (screen: Screen) world =
        let menuState = screen.GetMenuState world
        match menuState with
        | Some menu ->
            if (World.isKeyboardKeyPressed KeyboardKey.Escape world) then
                do screen.SetMenuState None world
            else

            match menu with
            | SaveOrLoad ->
                do World.beginPanel
                       "SaveOrLoadPanel"
                       [ Entity.Layout .= 
                           Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                         Entity.Position .= v3 0f 0f 0f
                         Entity.Size .= v3 210f 50f 0f
                         Entity.Color .= Color.Blue
                         Entity.LayoutMargin .= v2 3f 3f ] 
                       world

                let clicked =
                    World.doButton
                        "Save"
                        [ Entity.Text .= "Save"
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then
                    do screen.SetMenuState (Some Save) world

                let clicked =
                    World.doButton
                        "Load"
                        [ Entity.Text .= "Load"
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then
                    do screen.SetMenuState (Some Load) world

                do World.endPanel world

            | Save ->
                // I probably shouldn't do this in the render loop. Sort out later
                let saveSlots = Persistence.getSlotsState ()
                let getSlotText = Persistence.getSlotText saveSlots

                do World.beginPanel
                       "SavePanel"
                       [ Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                         Entity.Position .= v3 0f 0f 0f
                         Entity.Size .= v3 210f 120f 0f
                         Entity.Color .= Color.Blue
                         Entity.LayoutMargin .= v2 3f 3f ] 
                       world

                let clicked =
                    World.doButton
                        "Back"
                        [ Entity.Text .= "Back"
                          Entity.Position .= v3 0f 80f 0f
                          Entity.Size .= v3 200f 20f 0f ] 
                        world

                if clicked then
                    do screen.SetMenuState (Some SaveOrLoad) world

                let saveGame slot =
                    let events,_ = Game.GetProgression world
                    let zone = screen.GetZone world
                    let position = Simulants.PlayerCharacter.GetPosition world
                    let rotation = Simulants.PlayerCharacter.GetRotation world
                    let persistence: Persistence.PersistenceState = 
                        { Events = events;
                          Location = (zone, position, rotation) }
                    Persistence.save slot persistence

                let clicked =
                    World.doButton
                        "MainMenuSlot1"
                        [ Entity.Text @= getSlotText Persistence.Slot1
                          Entity.Position .= v3 0f 60f 0f
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then
                    do saveGame Persistence.Slot1

                let clicked =
                    World.doButton
                        "MainMenuSlot2"
                        [ Entity.Text @= getSlotText Persistence.Slot2
                          Entity.Position .= v3 0f 20f 0f
                          Entity.Size .= v3 200f 20f 0f  ]
                        world

                if clicked then
                    do saveGame Persistence.Slot2

                let clicked = 
                    World.doButton 
                        "MainMenuSlot3" 
                        [ Entity.Text @= getSlotText Persistence.Slot3
                          Entity.Position .= v3 0f -20f 0f
                          Entity.Size .= v3 200f 20f 0f ] 
                        world

                if clicked then
                    do saveGame Persistence.Slot3

                let clicked =
                    World.doButton
                        "MainMenuSlot4"
                        [ Entity.Text @= getSlotText Persistence.Slot4
                          Entity.Position .= v3 0f -60f 0f
                          Entity.Size .= v3 200f 20f 0f  ] 
                        world

                if clicked then
                    do saveGame Persistence.Slot4

                World.endPanel world
            | Load ->
                let saveSlots = Persistence.getSlotsState ()
                let getSlotText = Persistence.getSlotText saveSlots

                do World.beginPanel
                       "SavePanel"
                       [ Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                         Entity.Position .= v3 0f 0f 0f
                         Entity.Size .= v3 210f 120f 0f
                         Entity.Color .= Color.Blue
                         Entity.LayoutMargin .= v2 3f 3f ] 
                       world

                let clicked =
                    World.doButton
                        "Back"
                        [ Entity.Text .= "Back"
                          Entity.Position .= v3 0f 80f 0f
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then 
                    do screen.SetMenuState (Some SaveOrLoad) world

                let clicked = 
                    World.doButton 
                        "MainMenuSlot1"
                        [ Entity.Text @= getSlotText Persistence.Slot1
                          Entity.Position .= v3 0f 60f 0f
                          Entity.Size .= v3 200f 20f 0f ] 
                        world

                let loadGame slot =
                    let loadedState = Persistence.load slot
                    let progression = Persistence.toProgression loadedState
                    do Game.SetProgression progression world
                    let zone, position, rotation = loadedState.Location
                    do screen.SetMenuState None world
                    do screen.SetZone zone world
                    do Simulants.PlayerCharacter.SetPosition position world
                    do Simulants.PlayerCharacter.SetRotation rotation world

                if clicked then 
                    do loadGame Persistence.Slot1

                let clicked =
                    World.doButton
                        "MainMenuSlot2"
                        [ Entity.Text @= getSlotText Persistence.Slot2
                          Entity.Position .= v3 0f 20f 0f
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then 
                    do loadGame Persistence.Slot2

                let clicked =
                    World.doButton
                        "MainMenuSlot3"
                        [ Entity.Text @= getSlotText Persistence.Slot3
                          Entity.Position .= v3 0f -20f 0f
                          Entity.Size .= v3 200f 20f 0f ] 
                        world

                if clicked then
                    do loadGame Persistence.Slot3

                let clicked = 
                    World.doButton
                        "MainMenuSlot4"
                        [ Entity.Text @= getSlotText Persistence.Slot4
                          Entity.Position .= v3 0f -60f 0f
                          Entity.Size .= v3 200f 20f 0f ]
                        world

                if clicked then
                    do loadGame Persistence.Slot4

                do World.endPanel world

        | None ->
            if (World.isKeyboardKeyPressed KeyboardKey.Escape world) then 
                do screen.SetMenuState (Some SaveOrLoad) world

    static member Properties =
        [ define Screen.Zone StartingZone
          define Screen.Interaction NoInteraction 
          define Screen.MenuState None ]

    override this.Process (_, screen, world) =
        let explore = Game.GetProgression world |> Progression.toExplore
        let zone = screen.GetZone world
        do World.beginGroup Simulants.ExploreGroup.Name [] world
        do World.doSkyBox "SkyBox" [] world
        do World.doLight3d "Sun" [ Entity.Position .= v3 1.158f 2.990f -36.396f ] world
        do World.doRigidModelHierarchy Simulants.Zone.Name [ Entity.StaticModel @= Zones.toAsset zone ] world
        do World.doEntity<PlayerDispatcher> Simulants.PlayerCharacter.Name [ Entity.Position .= initialPosition; Entity.Rotation .= initialRotation ] world
        do doPlayerMovement world
        do doCamera world
        do doTriggers screen world
        do doActors explore screen world
        do doInteraction screen world
        do doInvisibleWalls explore screen world
        do doMenu screen world
        do World.endGroup world