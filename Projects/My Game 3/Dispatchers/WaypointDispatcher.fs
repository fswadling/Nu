namespace MyGame3
open Nu
open System.Numerics
open ImGuiNET

[<AutoOpen>]
module WaypointDispatcherExtensions =
    type Entity with
        member this.GetInvertZ world : bool = this.Get (nameof Entity.InvertZ) world
        member this.SetInvertZ (value: bool) world = this.Set (nameof Entity.InvertZ) value world
        member this.InvertZ = lens (nameof Entity.InvertZ) this this.GetInvertZ this.SetInvertZ

        member this.GetNodePositionsAndRotations world : (Vector3 * Quaternion) list =
            let rec loop acc (entity: Entity) =
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let children = entity.GetChildren world
                let child = Seq.tryExactlyOne children
                let newAcc = (position, rotation) :: acc
                match child with
                | None -> List.rev newAcc
                | Some child -> loop newAcc child
            loop [] this

type WaypointDispatcher () =
    inherit Entity3dDispatcherImSim (false, false, false)

    override this.Edit (op, entity, world) =
        match op with
        | ViewportOverlay _ ->
            let position = entity.GetPosition world
            let rotation = entity.GetRotation world
            let invertZ = entity.GetInvertZ world

            let forwardDir = if invertZ then Vector3.UnitZ else -Vector3.UnitZ
            let forward = Vector3.Transform(forwardDir, rotation)
            let right = Vector3.Transform(Vector3.UnitX, rotation)

            let shaftLength = 2.0f
            let headLength = 0.4f
            let headAngle = 0.3f

            let tip = position + forward * shaftLength
            let shaft = Segment3(position, tip)
            World.imGuiSegment3d shaft 2.0f Color.Yellow world

            let headDir1 = Vector3.Normalize(-forward + right * headAngle)
            let headDir2 = Vector3.Normalize(-forward - right * headAngle)
            let head1 = Segment3(tip, tip + headDir1 * headLength)
            let head2 = Segment3(tip, tip + headDir2 * headLength)
            World.imGuiSegment3d head1 2.0f Color.Yellow world
            World.imGuiSegment3d head2 2.0f Color.Yellow world

        | AppendProperties appendProperties ->
            if ImGui.Button "Add node" then
                let position = World.getEye3dCenter world
                let rotation = World.getEye3dRotation world
                let mountOpt = Some entity.EntityAddress
                let names = Array.skip 3 entity.Names
                let names = [| yield! names; "Node" |]
                let waypoint = World.createEntity<WaypointDispatcher> mountOpt OverlayDescriptor.DefaultOverlay (Some names) entity.Group world
                do waypoint.SetPosition position world
                do waypoint.SetRotation rotation world
                do appendProperties.EditContext.Snapshot SnapshotType.CreateEntity world

            if ImGui.Button "Set camera" then
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                do World.setEye3dCenter position world
                do World.setEye3dRotation rotation world

        | _ ->
            ()

    static member Properties =
        [ define Entity.InvertZ false
          define Entity.Pickable false
          define Entity.CastShadow false
          define Entity.Enabled true
          define Entity.Visible false ]