namespace MyGame3
open Nu
open System.Numerics

[<AutoOpen>]
module WaypointDispatcherExtensions =
    type Entity with
        member this.GetInvertZ world : bool = this.Get (nameof Entity.InvertZ) world
        member this.SetInvertZ (value: bool) world = this.Set (nameof Entity.InvertZ) value world
        member this.InvertZ = lens (nameof Entity.InvertZ) this this.GetInvertZ this.SetInvertZ

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
            
        | _ ->
            ()

    static member Properties =
        [ define Entity.InvertZ false
          define Entity.Pickable false
          define Entity.CastShadow false
          define Entity.Enabled true
          define Entity.Visible false ]