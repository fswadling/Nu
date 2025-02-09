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
              Entity.MaterialProperties .= MaterialProperties.defaultProperties ]
            world

type ExploreScreenDispatcher () =
    inherit ScreenDispatcherImSim ()

    let playerWalkSpeed = 0.1f
    let playerTurnSpeed = 0.05f
    let _, initialPosition, initialRotation = Zones.initial

    let doCamera (world: World) =
        if not world.Advancing then
            world
        else

        let playerRotation = Simulants.PlayerCharacter.GetRotation world
        let playerRotation = Quaternion.Concatenate(playerRotation, (Quaternion.CreateFromAxisAngle(v3Up, float32 Math.PI_MINUS_EPSILON)))
        let playerPostion = Simulants.PlayerCharacter.GetPosition world
        let world = World.setEye3dCenter (playerPostion + v3Up * 1.75f - playerRotation.Forward * 3.0f) world
        let world = World.setEye3dRotation playerRotation world
        world

    let doPlayerMovement (world: World) =
        if not world.Advancing then
            world
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
        let world = Simulants.PlayerCharacter.SetPosition position world
        let world = Simulants.PlayerCharacter.SetRotation rotation world
        let world = Simulants.PlayerCharacter.SetIsMoving (walkVelocity <> v3Zero) world
        world

    let doTriggers (screen: Screen) world =
        let doTrigger world (name, position, radius) =
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

        let foldPenetrations (results, world) (name, position, radius) =
            let bodyResults, world = doTrigger world (name, position, radius)
            let penetrations = FQueue.fold (enqueuePenetration name) results bodyResults
            penetrations, world

        let currentZone = screen.GetZone world
        let triggers = Zones.getTriggers currentZone
        let penetrations, world = Array.fold foldPenetrations (FQueue.empty, world) triggers

        match FQueue.tryHead penetrations with
        | Some triggerName ->
            let zone, position, rotation = Zones.getNextZone currentZone triggerName
            let world = screen.SetZone zone world
            let world = Simulants.PlayerCharacter.SetPosition position world
            let world = Simulants.PlayerCharacter.SetRotation rotation world
            world
        | None -> world

    let doActors (exploreState: ExploreState) (screen: Screen) world =
        let doActor world (positionedActor: PositionedActor) =
            let _, position, rotation = positionedActor.Location
            let actorName = positionedActor.Actor.ToString()
            let asset = Actor.idleAsset positionedActor.Actor

            let world =
                World.beginEntity<NPCDispatcher>
                    actorName
                    // Quick hack because im bored and want to get this done
                    [ if asset.IsChoice1Of2 then
                          Entity.Scale .= v3Dup 5.0f
                          Entity.Size .= v3Dup 0.2f
                      Entity.Position .= position 
                      Entity.Rotation .= rotation ]
                    world

            let world =
                match asset with
                | Choice2Of2 (asset, animations) ->
                    World.doAnimatedModel
                        "AnimatedModel" 
                        [ Entity.Size .= v3Dup 2.0f
                          Entity.Offset .= v3 0.0f 1.0f 0.0f
                          Entity.MaterialProperties .= MaterialProperties.defaultProperties
                          Entity.Animations .= [| animations |]
                          Entity.AnimatedModel .= asset ]
                        world
                | Choice1Of2 asset ->
                    World.doStaticModel
                        "StaticModel"
                        [ Entity.Size .= v3Dup 2.0f
                          Entity.Offset .= v3 0.0f 1.0f 0.0f
                          Entity.MaterialProperties .= MaterialProperties.defaultProperties
                          Entity.StaticModel .= asset ]
                        world

            let world =
                match positionedActor.Interaction with
                | Some interaction ->
                    let result, world = 
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
                        screen.SetInteraction (Inactive (positionedActor, interaction)) world
                    | Some (BodySeparationExplicitData _) -> 
                        screen.SetInteraction NoInteraction world
                    | _ -> world

                | None -> world

            World.endEntity world

        let world =
            exploreState.PositionedActors
            |> Seq.filter (fun actor -> Location.getZone actor.Location = screen.GetZone world)
            |> Seq.fold doActor world

        world

    let doInvisibleWalls (exploreState: ExploreState) (screen: Screen) world =
        let currentZone = screen.GetZone world
        exploreState.InvisibleWalls
        |> Array.fold (fun world (name, zone, position, radius) ->
            if zone = currentZone
            then
                World.doEntity<InvisibleWallDispatcher>
                    name
                    [ Entity.Position .= position
                      Entity.BodyShape .=
                        SphereShape
                            { Radius = radius
                              TransformOpt = None
                              PropertiesOpt = None } ]
                    world
            else world) world

    let doInteraction (screen: Screen) world =
        let interaction = screen.GetInteraction world
        match interaction with
        | Inactive (positionedActor, interaction) ->
            let world =
                World.doLabel
                    "InteractionPrompt"
                    [ Entity.Text .= "!";
                      Entity.PositionLocal .= v3 0f 120f 0f;
                      Entity.Size .= v3 10f 32f 0f ]
                    world

            if World.isKeyboardKeyDown KeyboardKey.Space world
            then screen.SetInteraction (Active (positionedActor, interaction)) world
            else world

        | Active (positionedActor, (Yield (Prompt (text, options), conversation))) ->
            let actorName = positionedActor.Actor.ToString()
            let world =
                World.beginPanel
                    "InteractionPanel"
                    [ Entity.BackdropImageOpt .= Some (Assets.Default.Image)
                      Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                      Entity.Position .= v3 0f 120f 0f
                      Entity.Size .= v3 500f 100f 0f
                      Entity.Color .= Color.Blue
                      Entity.LayoutMargin .= v2 3f 3f ] 
                    world

            let world =
                World.doText
                    "NPCName"
                    [ Entity.Text .= actorName
                      Entity.Size .= v3 500f 20f 0f ]
                    world

            let world =
                World.doText 
                    "Text" 
                    [ Entity.Text @= text
                      Entity.Size .= v3 500f 20f 0f
                      Entity.FontSizing .= Some 8 ]
                    world

            let world =
                World.beginPanel
                    "AnswersPanel"
                    [ Entity.Layout .= Layout.Flow (FlowDirection.FlowRightward, FlowLimit.FlowParent); 
                      Entity.Size .= v3 500f 40f 0f
                      Entity.BackdropImageOpt .= None ]
                    world

            let world =
                options
                |> Array.fold (fun world option ->
                    let clicked, world = 
                        World.doButton
                            option
                            [ Entity.Text .= option
                              Entity.Size .= v3 100f 20f 0f
                              Entity.FontSizing .= Some 8 ]
                            world

                    if clicked
                    then
                        let conversation = conversation (Respond option)

                        let world = screen.SetInteraction (Active (positionedActor, conversation)) world
                        world
                    else world)
                    world

            let world = World.endPanel world
            let world = World.endPanel world

            world

        | Active (_, (Return _)) ->
            let world = screen.SetInteraction NoInteraction world
            world
        | Active (positionedActor, interaction) ->
            // Upon encountering a progression event in the interaction,
            // apply and move on.
            let progression = Game.GetProgression world

            let interaction, progressionState =
                Progression.doInteraction
                    interaction
                    progression

            let interactionState = Active (positionedActor, interaction)
            let world = Game.SetProgression progressionState world
            let world = screen.SetInteraction interactionState world
            world
        | _ ->
            world

    let doMenu (screen: Screen) world =
        let menuState = screen.GetMenuState world
        match menuState with
        | Some menu ->
            let world =
                match menu with
                | SaveOrLoad ->
                    let world =
                        World.beginPanel
                            "SaveOrLoadPanel"
                            [ Entity.Layout .= 
                                Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                              Entity.Position .= v3 0f 0f 0f
                              Entity.Size .= v3 210f 50f 0f
                              Entity.Color .= Color.Blue
                              Entity.LayoutMargin .= v2 3f 3f ] 
                            world

                    let clicked, world =
                        World.doButton
                            "Save"
                            [ Entity.Text .= "Save"
                              Entity.Size .= v3 200f 20f 0f ]
                            world

                    let world = 
                        if clicked
                        then screen.SetMenuState (Some Save) world
                        else world

                    let clicked, world =
                        World.doButton
                            "Load"
                            [ Entity.Text .= "Load"
                              Entity.Size .= v3 200f 20f 0f ]
                            world

                    let world = 
                        if clicked
                        then screen.SetMenuState (Some Load) world
                        else world

                    let world = World.endPanel world
                    world
                | Save ->
                    // I probably shouldnt do this in the render loop. Sort out later
                    let saveSlots = Persistence.getSlotsState ()
                    let getSlotText = Persistence.getSlotText saveSlots

                    let world =
                        World.beginPanel
                            "SavePanel"
                            [ Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                              Entity.Position .= v3 0f 0f 0f
                              Entity.Size .= v3 210f 120f 0f
                              Entity.Color .= Color.Blue
                              Entity.LayoutMargin .= v2 3f 3f ] 
                            world

                    let clicked, world = World.doButton "Back" [ Entity.Text .= "Back"; Entity.Position .= v3 0f 80f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let world =
                        if clicked
                        then screen.SetMenuState (Some SaveOrLoad) world
                        else world

                    let saveGame slot world =
                        let events,_ = Game.GetProgression world
                        let zone = screen.GetZone world
                        let position = Simulants.PlayerCharacter.GetPosition world
                        let rotation = Simulants.PlayerCharacter.GetRotation world
                        let persistence: Persistence.PersistenceState = 
                            { Events = events;
                              Location = (zone, position, rotation) }
                        Persistence.save slot persistence

                    let clicked, world = World.doButton "MainMenuSlot1" [ Entity.Text @= getSlotText Persistence.Slot1; Entity.Position .= v3 0f 60f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    if clicked then
                        do saveGame Persistence.Slot1 world

                    let clicked, world = World.doButton "MainMenuSlot2" [ Entity.Text @= getSlotText Persistence.Slot2; Entity.Position .= v3 0f 20f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    if clicked then
                        do saveGame Persistence.Slot2 world

                    let clicked, world = World.doButton "MainMenuSlot3" [ Entity.Text @= getSlotText Persistence.Slot3; Entity.Position .= v3 0f -20f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    if clicked then
                        do saveGame Persistence.Slot3 world

                    let clicked, world = World.doButton "MainMenuSlot4" [ Entity.Text @= getSlotText Persistence.Slot4; Entity.Position .= v3 0f -60f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    if clicked then
                        do saveGame Persistence.Slot4 world

                    let world = World.endPanel world
                    world
                | Load ->
                    let saveSlots = Persistence.getSlotsState ()
                    let getSlotText = Persistence.getSlotText saveSlots

                    let world =
                        World.beginPanel
                            "SavePanel"
                            [ Entity.Layout .= Layout.Flow (FlowDirection.FlowDownward, FlowLimit.FlowParent)
                              Entity.Position .= v3 0f 0f 0f
                              Entity.Size .= v3 210f 120f 0f
                              Entity.Color .= Color.Blue
                              Entity.LayoutMargin .= v2 3f 3f ] 
                            world

                    let clicked, world = World.doButton "Back" [ Entity.Text .= "Back"; Entity.Position .= v3 0f 80f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let world =
                        if clicked
                        then screen.SetMenuState (Some SaveOrLoad) world
                        else world

                    let clicked, world = World.doButton "MainMenuSlot1" [ Entity.Text @= getSlotText Persistence.Slot1; Entity.Position .= v3 0f 60f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let loadGame slot world =
                        let loadedState = Persistence.load slot
                        let progression = Persistence.toProgression loadedState
                        let world = Game.SetProgression progression world
                        let zone, position, rotation = loadedState.Location
                        let world = screen.SetMenuState None world
                        let world = screen.SetZone zone world
                        let world = Simulants.PlayerCharacter.SetPosition position world
                        let world = Simulants.PlayerCharacter.SetRotation rotation world
                        world

                    let world =
                        if clicked
                        then loadGame Persistence.Slot1 world
                        else world

                    let clicked, world = World.doButton "MainMenuSlot2" [ Entity.Text @= getSlotText Persistence.Slot2; Entity.Position .= v3 0f 20f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let world =
                        if clicked
                        then loadGame Persistence.Slot2 world
                        else world

                    let clicked, world = World.doButton "MainMenuSlot3" [ Entity.Text @= getSlotText Persistence.Slot3; Entity.Position .= v3 0f -20f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let world =
                        if clicked
                        then loadGame Persistence.Slot3 world
                        else world

                    let clicked, world = World.doButton "MainMenuSlot4" [ Entity.Text @= getSlotText Persistence.Slot4; Entity.Position .= v3 0f -60f 0f; Entity.Size .= v3 200f 20f 0f  ] world

                    let world =
                        if clicked
                        then loadGame Persistence.Slot4 world
                        else world

                    let world = World.endPanel world
                    world

            if (World.isKeyboardKeyPressed KeyboardKey.Escape world)
            then screen.SetMenuState None world
            else world
        | None ->
            if (World.isKeyboardKeyPressed KeyboardKey.Escape world)
            then screen.SetMenuState (Some SaveOrLoad) world
            else world

    static member Properties =
        [ define Screen.Zone StartingZone
          define Screen.Interaction NoInteraction 
          define Screen.MenuState None ]

    override this.Process (_, screen, world) =
        let explore = Game.GetProgression world |> Progression.toExplore
        let zone = screen.GetZone world
        let world = World.beginGroup Simulants.ExploreGroup.Name [] world
        let world = World.doSkyBox "SkyBox" [] world
        let world = World.doRigidModelHierarchy Simulants.Zone.Name [ Entity.StaticModel @= Zones.toAsset zone ] world
        let world = World.doEntity<PlayerDispatcher> Simulants.PlayerCharacter.Name [ Entity.Position .= initialPosition; Entity.Rotation .= initialRotation ] world
        let world = doPlayerMovement world
        let world = doCamera world
        let world = doTriggers screen world
        let world = doActors explore screen world
        let world = doInteraction screen world
        let world = doInvisibleWalls explore screen world
        let world = doMenu screen world
        let world = World.endGroup world
        world