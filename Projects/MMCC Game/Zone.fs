namespace MMCCGame
open System
open Nu

type Zone =
    | PlayerApartment

module Zone =
    let toPath = function
        | PlayerApartment -> "Assets/Gameplay/PlayerApartments/Main.nugroup"