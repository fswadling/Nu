namespace MyGame3
open Nu
open System.Numerics
open Prime

type Character =
    | Player

[<RequireQualifiedAccess>]
module Character =
    let toPath = function
        | Player -> "Assets/Gameplay/Characters/Player/Player.nuentity"

    let idle = function
        | Player -> "Idle"

    let jog = function
        | Player -> "Jog"

    let locomotionAnimations (isMoving: bool) (blendRate: single) (rate: single) (gameTime: GameTime) (currentAnimations: Animation array) (character: Character) =
        let jogName = jog character

        let currentJogWeight =
            match Array.tryFind (fun (a: Animation) -> a.Name = jogName) currentAnimations with
            | Some a -> a.Weight
            | None -> 0.0f

        let targetJogWeight = if isMoving then 1.0f else 0.0f

        let jogWeight =
            if currentJogWeight < targetJogWeight then min targetJogWeight (currentJogWeight + blendRate)
            elif currentJogWeight > targetJogWeight then max targetJogWeight (currentJogWeight - blendRate)
            else currentJogWeight

        let idleWeight = 1.0f - jogWeight

        let idleAnimation character =
            let idleName = idle character
            Animation.make gameTime None idleName Playback.Loop rate idleWeight None

        let jogAnimation character =
            let jogName = jog character
            Animation.make gameTime None jogName Playback.Loop rate jogWeight None

        let updateOrCreate (makeNew: Character -> Animation) (weight: single) (currentAnimations: Animation array) (character: Character) =
            let fresh = makeNew character
            match Array.tryFind (fun (a: Animation) -> a.Name = fresh.Name) currentAnimations with
            | Some existing -> { existing with Weight = weight }
            | None -> { fresh with Weight = weight }

        [| if idleWeight > 0.0f then updateOrCreate idleAnimation idleWeight currentAnimations character
           if jogWeight > 0.0f then updateOrCreate jogAnimation jogWeight currentAnimations character |]

type [<SymbolicExpansion>] CharacterProp =
    { Character: Character
      Position: Vector3
      Rotation: Quaternion
      Animations: Animation array
      Morphs : (int * single) array }

