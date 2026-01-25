namespace MyGame2

open Nu
open System.Numerics
open ImGuiNET

[<AutoOpen>]
module PathDispatcherExtensions =
    type Entity with
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

type PathDispatcher () =
    inherit WaypointDispatcher ()

    override this.Edit (op, entity, world) =
        base.Edit (op, entity, world)
        match op with
        | ViewportOverlay _ -> ()
        | AppendProperties appendProperties ->
            // Add buttons/controls to the property panel
            if ImGui.Button "Add node" then
                let position = World.getEye3dCenter world
                let rotation = World.getEye3dRotation world
                let mountOpt = Some entity.EntityAddress
                let names = Array.skip 3 entity.Names
                let names = [| yield! names; "Node" |]
                let waypoint = World.createEntity<PathDispatcher> mountOpt OverlayDescriptor.DefaultOverlay (Some names) entity.Group world
                do waypoint.SetPosition position world
                do waypoint.SetRotation rotation world
                do appendProperties.EditContext.Snapshot SnapshotType.CreateEntity world

            if ImGui.Button "Set camera" then
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                do World.setEye3dCenter position world
                do World.setEye3dRotation rotation world
        | _ -> ()