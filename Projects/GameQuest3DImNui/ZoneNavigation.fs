namespace GameQuest3DImNui
open Nu
open System.Numerics

[<AutoOpen>]
module Zones =
    type Zone = 
        | StartingZone
        | SideZone
        | SideZone2
        | SideZone3
        | CrystalsHub
        | CrystalZone1
        | CrystalZone2
        | FinalZone

    type Location = Zone * Position * Rotation

    module Location =
        let getZone (zone, _, _) = zone

    module StartingZone =
        let getNextZone: string -> Location = function
            | "SideZoneTrigger" -> SideZone, v3 19.147f 30.794f 19.441f, Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON) 
            | "SideZone2Trigger" -> SideZone2, v3 37.900f 171.842f 217.568f, Quaternion.CreateFromAxisAngle (v3Up, 0f) 
            | "SideZone3Trigger" -> SideZone3, v3 -7.414f -27.770f -35.525f, Quaternion.CreateFromAxisAngle (v3Up, float32 (Math.PI_MINUS_EPSILON + 1.0))
            | "CrystalsHubTrigger" -> CrystalsHub, v3 16.594f -10.006f 12.200f, Quaternion.CreateFromAxisAngle (v3Up, float32 0f)
            | _ -> failwith "Invalid trigger key"

        let triggers = 
            [| "SideZoneTrigger", v3 -20.163f -10.355f -48.243f, 7.5f
               "SideZone2Trigger", v3 11.235f -12.066f -55.687f, 7.5f
               "SideZone3Trigger", v3 32.584f -11.944f 10.837f, 7.5f
               "CrystalsHubTrigger", v3 -39.704f -13.152f 23.631f, 7.5f |]

        let crystalsInvisibleWalls: (string * Zone * Position * Radius) array =
             [| ("blockCrystals", StartingZone, v3 -22.962f -11.072f 14.153f, 5f)
                ("blockCrystals2", StartingZone, v3 -17.788f -8.950f 17.260f, 5f) |]

    module SideZone =
        let getNextZone: string -> Location = function
            | "StartingZoneTrigger" -> StartingZone, v3 -11.481f -9.834f -45.712f, Quaternion.CreateFromAxisAngle (v3Up, 0f)
            | _ -> failwith "Invalid trigger key"

        let triggers = 
            [| "StartingZoneTrigger", v3 16.138f 29.244f 34.434f, 10f |]

        let witchLocation: Location = 
            SideZone,
            v3 5.146f 39.332f 4.010f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module SideZone2 =
        let getNextZone: string -> Location = function
            | "StartingZoneTrigger" -> StartingZone, v3 2.137f -10.610f -48.265f, Quaternion.CreateFromAxisAngle (v3Up, 0f)
            | _ -> failwith "Invalid trigger key"

        let triggers = 
            [| "StartingZoneTrigger", v3 36.174f 174.825f 210.467f, 5f |]

        let knightLocation: Location =
            SideZone2,
            v3 79.407f 170.238f 233.856f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module SideZone3 =
        let getNextZone: string -> Location = function
            | "StartingZoneTrigger" -> StartingZone, v3 26.145f -10.995f 0.850f, Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON) 
            | _ -> failwith "Invalid trigger key"

        let triggers = 
            [| "StartingZoneTrigger", v3 -0.467f -29.197f -25.340f, 10f |]

        let thiefLocation: Location =
            SideZone3,
            v3 -67.045f -24.224f -36.367f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module CrystalsHub =
        let getNextZone: string -> Location = function
            | "StartingZoneTrigger" -> StartingZone, v3 -31.825f -11.454f 21.349f, Quaternion.CreateFromAxisAngle (v3Up, float32 (Math.PI_MINUS_EPSILON / 2.0))
            | "CrystalZone1Trigger" -> CrystalZone1, v3 38.626f 7.896f -4.102f, Quaternion.CreateFromAxisAngle (v3Up, float32 -(Math.PI_MINUS_EPSILON / 2.0))
            | "CrystalZone2Trigger" -> CrystalZone2, v3 38.626f 7.896f -4.102f, Quaternion.CreateFromAxisAngle (v3Up, float32 -(Math.PI_MINUS_EPSILON / 2.0))
            | "FinalZoneTrigger" -> FinalZone, v3 38.626f 7.896f -4.102f, Quaternion.CreateFromAxisAngle (v3Up, float32 -(Math.PI_MINUS_EPSILON / 2.0))
            | _ -> failwith "Invalid trigger key"

        let triggers =
            [| "StartingZoneTrigger", v3 24.305f -10.500f -0.313f, 5f
               "CrystalZone1Trigger", v3 7.623f -9.442f 52.738f, 5f
               "CrystalZone2Trigger", v3 -24.173f -11.103f 48.844f, 5f
               "FinalZoneTrigger", v3 -27.442f -11.120f -21.414f, 5f |]

        let finalAreaInvisibleWalls: (string * Zone * Position * Radius) array =
            [| ("blockEnding", CrystalsHub, v3 -12.250f -10.669f -13.861f, 5f) |]

        let fireCrystalLocation: Location = 
            CrystalsHub,
            v3 -9.403f -8.652f 18.531f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module CrystalZone1 =
        let getNextZone: string -> Location = function
            | "CrystalHubTrigger" -> CrystalsHub, v3 -3.091f -10.097f 49.690f, Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
            | _ -> failwith "Invalid trigger key"

        let triggers =
            [| "CrystalHubTrigger", v3 52.191f 8.092f -2.771f, 5f |]

        let waterCrystalLocation: Location =
            CrystalZone1,
            v3 0.951f 9.586f 25.180f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

        let airCrystalLocation: Location =
            CrystalZone1,
            v3 -8.128f 7.674f -20.251f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module CrystalZone2 =
        let getNextZone: string -> Location = function
            | "CrystalHubTrigger" -> CrystalsHub, v3 -16.211f -10.545f 47.502f, Quaternion.CreateFromAxisAngle (v3Up, float32 -(Math.PI_MINUS_EPSILON * (3.0 / 2.0))) 
            | _ -> failwith "Invalid trigger key"

        let triggers =
            [| "CrystalHubTrigger", v3 52.191f 8.092f -2.771f, 5f |]

        let earthCrystalLocation: Location =
            CrystalZone2,
            v3 -5.581f 7.227f 2.553f,
            Quaternion.CreateFromAxisAngle (v3Up, 0f)

    module FinalZone =
        let getNextZone: string -> Location = function
            | "CrystalHubTrigger" -> CrystalsHub, v3 -12.650f -10.648f -12.873f, Quaternion.CreateFromAxisAngle (v3Up, float32 -(Math.PI_MINUS_EPSILON * (3.0 / 2.0)))
            | _ -> failwith "Invalid trigger key"

        let triggers =
            [| "CrystalHubTrigger", v3 52.191f 8.092f -2.771f, 5f |]

    let initial = StartingZone, v3 1.158f -8.477f -26.876f, Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON) 

    let getNextZone = function
        | StartingZone -> StartingZone.getNextZone
        | SideZone -> SideZone.getNextZone
        | SideZone2 -> SideZone2.getNextZone
        | SideZone3 -> SideZone3.getNextZone
        | CrystalsHub -> CrystalsHub.getNextZone
        | CrystalZone1 -> CrystalZone1.getNextZone
        | CrystalZone2 -> CrystalZone2.getNextZone
        | FinalZone -> FinalZone.getNextZone

    let getTriggers = function
        | StartingZone -> StartingZone.triggers
        | SideZone -> SideZone.triggers
        | SideZone2 -> SideZone2.triggers
        | SideZone3 -> SideZone3.triggers
        | CrystalsHub -> CrystalsHub.triggers
        | CrystalZone1 -> CrystalZone1.triggers
        | CrystalZone2 -> CrystalZone2.triggers
        | FinalZone -> FinalZone.triggers

    let toAsset = function
        | StartingZone -> Assets.Gameplay.StartingZone
        | SideZone -> Assets.Gameplay.SideZone
        | SideZone2 -> Assets.Gameplay.SideZone2
        | SideZone3 -> Assets.Gameplay.SideZone3
        | CrystalsHub -> Assets.Gameplay.CrystalsHub
        | CrystalZone1 -> Assets.Gameplay.CrystalZone1
        | CrystalZone2 -> Assets.Gameplay.CrystalZone1
        | FinalZone -> Assets.Gameplay.CrystalZone1