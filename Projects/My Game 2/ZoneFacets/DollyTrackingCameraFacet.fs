namespace MyGame2

open Nu

[<AutoOpen>]
module DollyTrackingCameraFacetExtensions =
    type Entity with
        member this.GetDolly world : EntityName = this.Get (nameof Entity.Dolly) world
        member this.SetDolly (value: EntityName) world = this.Set (nameof Entity.Dolly) value world
        member this.Dolly = lens (nameof Entity.Dolly) this this.GetDolly this.SetDolly

type DollyTrackingCameraFacet () =
    inherit Facet (false, false, false)

    override this.Update (entity, world) =
        if not world.Advancing then () else
        // resolve rail points from labels (same group as entity)
        let dollyEntityName = entity.GetDolly world
        let dollyEntity = entity.Group / dollyEntityName
        let nodeData = dollyEntity.GetNodePositionsAndRotations world
        let railPoints = nodeData |> List.map fst
        let entityPos = entity.GetPosition world
        let closestPointOnRailOpt = Maths.tryClosestPointOnPolyline railPoints entityPos

        match closestPointOnRailOpt with
        | None -> ()
        | Some desiredEyeCenter ->

        // aim at tracked entity
        let target = entity.GetPosition world
        let dir = target - desiredEyeCenter
        let rot = Maths.lookRotation dir

        do World.setEye3dCenter desiredEyeCenter world
        do World.setEye3dRotation rot world

    static member Properties =
        [ define Entity.Dolly "" ]