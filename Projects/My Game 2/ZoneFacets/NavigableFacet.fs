namespace MyGame2

open Nu
open System.Numerics

type NavigableFacet () =
    inherit Facet (false, false, false)

    let playerWalkSpeed = 0.1f
    let playerTurnSpeed = 0.05f

    override this.Update (entity, world) =
        if not world.Advancing then () else

        // current rotation & forward
        let rotation = entity.GetRotation world
        let forward = rotation.Forward

        // movement input
        let walkDirection =
            (if World.isKeyboardKeyDown KeyboardKey.W world then -forward else v3Zero) +
            (if World.isKeyboardKeyDown KeyboardKey.S world then forward else v3Zero)

        let turnInput =
            (if World.isKeyboardKeyDown KeyboardKey.D world then -1.0f else 0.0f) +
            (if World.isKeyboardKeyDown KeyboardKey.A world then  1.0f else 0.0f)

        let turnVelocity = turnInput * playerTurnSpeed

        let walkVelocity = walkDirection * playerWalkSpeed
        let position = entity.GetPosition world + walkVelocity

        // incremental yaw around up
        let deltaRot = Quaternion.CreateFromAxisAngle (v3Up, turnVelocity)

        // compose � apply delta in local or world space depending on order
        // This gives: newRotation = deltaRot * rotation  (yaw, then existing orientation)
        let rotation =
            Quaternion.Normalize (deltaRot * rotation)

        do entity.SetPosition position world
        do entity.SetRotation rotation world

        let isMoving = walkVelocity <> v3Zero || turnVelocity <> 0.0f
        let walkWeight = if isMoving then 1.0f else 0.0f
        let idleWeight = 1.0f - walkWeight

        let animations = entity.GetAnimations world
        let walkAnimation = animations |> Array.tryFind (fun a -> a.Name = "Jog")
        let idleAnimation = animations |> Array.tryFind (fun a -> a.Name = "Idle")

        match walkAnimation, idleAnimation with
        | Some walkAnim, Some idleAnim ->
            let updatedWalkAnimation = { walkAnim with Weight = walkWeight }
            let updatedIdleAnimation = { idleAnim with Weight = idleWeight }
            let updatedAnimations =
                animations
                |> Array.map (fun a ->
                    if a.Name = "Jog" then updatedWalkAnimation
                    elif a.Name = "Idle" then updatedIdleAnimation
                    else a)
            do entity.SetAnimations updatedAnimations world

        | None, Some idleAnim ->
            let newWalkAnimation = Animation.make world.GameTime None "Jog" Nu.Playback.Loop 30f walkWeight None
            let updatedIdleAnimation = { idleAnim with Weight = idleWeight }
            let updatedAnimations =
                animations
                |> Array.map (fun a ->
                    if a.Name = "Idle" then updatedIdleAnimation
                    else a)
                |> Array.append [| newWalkAnimation |]
            do entity.SetAnimations updatedAnimations world

        | Some walkAnim, None ->
            let updatedWalkAnimation = { walkAnim with Weight = walkWeight }
            let newIdleAnimation = Animation.make world.GameTime None "Idle" Nu.Playback.Loop 30f idleWeight None
            let updatedAnimations =
                animations
                |> Array.map (fun a ->
                    if a.Name = "Jog" then updatedWalkAnimation
                    else a)
                |> Array.append [| newIdleAnimation |]
            do entity.SetAnimations updatedAnimations world

        | None, None ->
            let newWalkAnimation = Animation.make world.GameTime None "Jog" Nu.Playback.Loop 30f walkWeight None
            let newIdleAnimation = Animation.make world.GameTime None "Idle" Nu.Playback.Loop 30f idleWeight None
            let updatedAnimations =
                animations
                |> Array.append [| newWalkAnimation; newIdleAnimation |]
            do entity.SetAnimations updatedAnimations world

