namespace MyGame2

open Nu
open Prime

type ScreenTag =
    | Splash
    | Title
    | Credits
    | Zone of Zone:Zone

[<RequireQualifiedAccess>]
module ScreenTag =
    let isZone zone = function
        | Zone currentZone -> currentZone = zone
        | _ -> false

[<AutoOpen>]
module ZoneExtensions =
    type Screen with
        member this.GetZone world : Zone = this.Get (nameof Screen.Zone) world
        member this.SetZone (value: Zone) world = this.Set (nameof Screen.Zone) value world
        member this.Zone = lens (nameof Screen.Zone) this this.GetZone this.SetZone

        member this.GetScenarios world : Scenario Set = this.Get (nameof Screen.Scenarios) world
        member this.SetScenarios (value: Scenario Set) world = this.Set (nameof Screen.Scenarios) value world
        member this.Scenarios = lens (nameof Screen.Scenarios) this this.GetScenarios this.SetScenarios

        member this.GetPlayerScenario world : Scenario option = this.Get (nameof Screen.PlayerScenario) world
        member this.SetPlayerScenario (value: Scenario option) world = this.Set (nameof Screen.PlayerScenario) value world
        member this.PlayerScenario = lens (nameof Screen.PlayerScenario) this this.GetPlayerScenario this.SetPlayerScenario

    type Group with
        member this.GetCues world : Cue FDeque = this.Get (nameof Group.Cues) world
        member this.SetCues (value: Cue FDeque) world = this.Set (nameof Group.Cues) value world
        member this.Cues = lens (nameof Group.Cues) this this.GetCues this.SetCues

// this is the dispatcher that defines the behavior of the screen where gameplay takes place.
type ZoneDispatcher () =
    inherit ScreenDispatcherImSim ()

    // here we define default property values
    static member Properties =
        [ define Screen.Zone NoZone
          define Screen.Scenarios Set.empty
          define Screen.PlayerScenario None ]

    // here we define the behavior of our gameplay
    override this.Process (_, screen, world) =
        let zone = screen.GetZone world
        let zoneGroupPath = Zone.toPath zone

        match zoneGroupPath with
        | None -> ()
        | Some zoneGroupPath ->
            do World.doGroupFromFile Zone.main zoneGroupPath [] world

        let scenarios = screen.GetScenarios world

        for scenario in scenarios do
            let name = Scenario.toName scenario
            let path = Scenario.toPath scenario
            do World.doGroupFromFile name path [] world
