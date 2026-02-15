namespace MyGame2

open Nu
open Prime
open ImGuiNET

type ScenarioDispatcher () =
    inherit GroupDispatcherImSim ()

    override this.Edit (op, group, world) =
        base.Edit (op, group, world)
        match op with
        | AppendProperties appendProperties ->
            if ImGui.Button "Add path" then
                let position = World.getEye3dCenter world
                let rotation = World.getEye3dRotation world
                let mountOpt = None
                let names = [| "Node" |]
                let waypoint = World.createEntity<PathDispatcher> mountOpt OverlayDescriptor.DefaultOverlay (Some names) group world
                do waypoint.SetPosition position world
                do waypoint.SetRotation rotation world
                do appendProperties.EditContext.Snapshot SnapshotType.CreateEntity world
         | _ -> ()

    static member Properties =
        [ define Group.InitialCues FDeque.empty<Cue> ]