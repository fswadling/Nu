namespace ImSimWithReflecetedState

type Zone =
    | PlayerApartment

module Zone =
    let toPath = function
        | PlayerApartment -> "Assets/Gameplay/PlayerApartments/Main.nugroup"