namespace MyGame2

open Nu
open System.Numerics

type CameraFacingFacet () =
    inherit Facet (false, false, false)

    override this.Update (entity, world) =
        let rotation = entity.GetRotation world * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
        let position = entity.GetPosition world
        do World.setEye3dCenter (position + v3Up * 1.40f - rotation.Forward) world
        do World.setEye3dRotation rotation world
