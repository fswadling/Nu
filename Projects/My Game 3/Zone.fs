namespace MyGame3
open System
open Nu

type Zone =
    | NoZone
    | PlayerApartment

module Zone =
    let toPath = function
        | NoZone -> None
        | PlayerApartment -> Some "Assets/Gameplay/PlayerApartments/Main.nugroup"