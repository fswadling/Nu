namespace MyGame2

open Nu

type TriggerVolumeDispatcher () = 
    inherit Entity3dDispatcherImSim (false, false, false)

    static member Radius = 1.0f

    static member Facets =
        [ typeof<RigidBodyFacet> ]

    static member Properties =
        [ define Entity.BodyShape (SphereShape { Radius = TriggerVolumeDispatcher.Radius; TransformOpt = None; PropertiesOpt = None })
          define Entity.Sensor true ]

