namespace ImSimWithReflecetedState
open System.Numerics
open Nu
open Prime

// this represents the state of gameplay simulation.
type GameplayState =
    | Playing
    | Quit

// this extends the Screen API to expose the GameplayState property.
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplayState world : GameplayState = this.Get (nameof Screen.GameplayState) world
        member this.SetGameplayState (value : GameplayState) world = this.Set (nameof Screen.GameplayState) value world
        member this.GameplayState = lens (nameof Screen.GameplayState) this this.GetGameplayState this.SetGameplayState

        member this.GetZone world : Zone = this.Get (nameof Screen.Zone) world
        member this.SetZone (value : Zone) world = this.Set (nameof Screen.Zone) value world
        member this.Zone = lens (nameof Screen.Zone) this this.GetZone this.SetZone

        member this.GetPlayer world : CharacterProp option = this.Get (nameof Screen.Player) world
        member this.SetPlayer (value : CharacterProp option) world = this.Set (nameof Screen.Player) value world
        member this.Player = lens (nameof Screen.Player) this this.GetPlayer this.SetPlayer

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    let animationRate = 30f
    let blendRate = 0.05f
    let playerWalkSpeed = 0.1f
    let playerTurnSpeed = 0.05f

    let onSelect (screen: Screen) world =
        let startWaypoint = Simulants.GameplayScene / "Start"
        let position = startWaypoint.GetPosition world
        let rotation = startWaypoint.GetRotation world
        do World.setEye3dCenter position world
        let player: CharacterProp =
            { Position = position
              Rotation = rotation
              Character = Player
              Animations = [||]
              Morphs = FMap.empty }
        do screen.SetPlayer (Some player) world

    let updatePlayer world (playerProp: CharacterProp) =
        let cameraRotation = playerProp.Rotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)

        // current forward (from camera-adjusted rotation)
        let forward = cameraRotation.Forward

        // movement input
        let walkDirection =
            (if World.isKeyboardKeyDown KeyboardKey.W world then forward else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.S world then -forward else v3Zero)

        let turnInput =
            (if World.isKeyboardKeyDown KeyboardKey.D world then -1.0f else 0.0f) +
            (if World.isKeyboardKeyDown KeyboardKey.A world then  1.0f else 0.0f)

        let turnVelocity = turnInput * playerTurnSpeed
        let walkVelocity = walkDirection * playerWalkSpeed
        let playerPosition = playerProp.Position + walkVelocity

        // incremental yaw around up
        let deltaRot = Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)

        // apply turn delta to the original entity rotation (not the camera-adjusted one)
        let playerRotation = Quaternion.Normalize (deltaRot * playerProp.Rotation)

        // animation blending between Idle and Jog (character props only)
        let isMoving = walkDirection.LengthSquared() > 1e-6f || abs turnInput > 0.0f
        let animations = CharacterProp.locomotionAnimations isMoving blendRate animationRate world.GameTime playerProp

        let playerProp = 
            { playerProp with 
                Position = playerPosition
                Rotation = playerRotation
                Animations = animations }

        playerProp

    // This will bind the animations and props from the screen prop,
    // as well as the position and rotation on the first render.
    // afterwards, control is handed over to the game and physics engine,
    // and the current position and rotation are reflected back to the screen
    // state. This is obviously not true binding as changing the state at the screen
    // level will have no impact on the entity and will immediatly be overwritten.
    // Buut it does give me something at the screen level that I can save and use when
    // initialising, as well as put away into storage in the gamestate when I do zone transitions.
    // If I applied this to other props, I would still have to manipulate them via effectful cues.
    // Maybe i could toggle the binding on the physical property? Ahhh it all feels hacky.
    let doPlayer world (playerProp: CharacterProp) =
        let playerProp =
            if Simulants.GameplayPlayer.GetExists world
            then
                { playerProp with
                    Position = Simulants.GameplayPlayer.GetPosition world
                    Rotation = Simulants.GameplayPlayer.GetRotation world }
            else
                playerProp

        let playerProp =
            if world.Advancing
            then updatePlayer world playerProp
            else playerProp

        do CharacterProp.doEntity Simulants.GameplayPlayer.Name playerProp world

        if world.Advancing then
            do CharacterProp.cameraFollow world playerProp

        playerProp

    // here we define default property values
    static member Properties =
        [define Screen.GameplayState Quit
         define Screen.Zone PlayerApartment
         define Screen.Player None]

    // here we define the behavior of our gameplay
    override this.Process (selectEvents, screen, world) =
        // only process when selected
        if not (screen.GetSelected world) then () else

        let zone = screen.GetZone world
        let zonePath = Zone.toPath zone
        do World.beginGroupFromFile Simulants.GameplayScene.Name zonePath [] world

        if FQueue.contains Select selectEvents then
            do onSelect screen world

        let player = screen.GetPlayer world
        let player = Option.map (doPlayer world) player
        do screen.SetPlayer player world

        // declare quit button
        if World.doButton "Quit" [Entity.Position .= v3 232.0f -144.0f 0.0f; Entity.Text .= "Quit"] world then
            screen.SetGameplayState Quit world

        // ensure game is unpaused when quitting
        if screen.GetGameplayState world = Quit then
            World.setAdvancing true world

        // end scene declaration
        World.endGroup world