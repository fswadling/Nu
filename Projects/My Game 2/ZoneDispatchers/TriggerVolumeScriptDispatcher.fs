namespace MyGame2

open Nu
open Prime


[<AutoOpen>]
module TriggerVolumeScriptDispatcherExtensions =
    type Entity with
        member this.GetTriggerCues world : Cue FDeque = this.Get (nameof Entity.TriggerCues) world
        member this.SetTriggerCues (value: Cue FDeque) world = this.Set (nameof Entity.TriggerCues) value world
        member this.TriggerCues = lens (nameof Entity.TriggerCues) this this.GetTriggerCues this.SetTriggerCues

type TriggerVolumeScriptDispatcher () =
    inherit TriggerVolumeDispatcher ()

    static let handleBodyPenetration (evt : Event<BodyPenetrationData, Entity>) (world : World) =
        let entity = evt.Subscriber
        let triggerCues = entity.GetTriggerCues world
        if FDeque.isEmpty triggerCues then Cascade else

        // enqueue cues onto the group that owns this entity, avoiding duplicates by name
        let group = entity.Group
        let cues = group.GetCues world
        let alreadyActiveNames = cues |> FDeque.map _.Name |> Set.ofSeq
        let canRunCue (cue: Cue) = not (Set.contains cue.Name alreadyActiveNames)
        let cuesToEnqueue = FDeque.filter canRunCue triggerCues

        if FDeque.isEmpty cuesToEnqueue then Cascade else
        do group.SetCues (FDeque.append cues cuesToEnqueue) world
        Cascade

    static member Properties =
        [ define Entity.TriggerCues FDeque.empty ]

    override this.Register (entity : Entity, world : World) =
        // listen for first touch / overlap events (requires Sensor=true on the rigid body)
        World.monitor handleBodyPenetration entity.BodyPenetrationEvent entity world
