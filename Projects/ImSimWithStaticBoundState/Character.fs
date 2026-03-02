namespace ImSimWithStaticBoundState
open System.Numerics
open Nu
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

type [<SymbolicExpansion>] CharacterProp =
    { Position : Vector3
      Rotation : Quaternion
      Character: Character
      Animations : Animation array
      Morphs : FMap<int, single> }

[<RequireQualifiedAccess>]
module CharacterProp =
    let doEntity (name: string) (world: World) (prop: CharacterProp) =
        let propPath = Character.toPath prop.Character
        let morphs = FMap.toArray prop.Morphs

        do World.doEntityFromFile name propPath
            [Entity.Position .= prop.Position
             Entity.Rotation .= prop.Rotation
             Entity.Animations @= prop.Animations
             Entity.Morphs @= morphs]
            world

    let cameraFollow (world: World) (position: Vector3) (rotation: Quaternion) =
        let cameraRotation =rotation * Quaternion.CreateFromAxisAngle (v3Up, float32 Math.PI_MINUS_EPSILON)
        do World.setEye3dCenter (position + v3Up * 1.40f - cameraRotation.Forward) world
        do World.setEye3dRotation cameraRotation world

    let locomotionAnimations (isMoving: bool) (blendRate: single) (rate: single) (gameTime: GameTime) (character: CharacterProp) =
        let jogName = Character.jog character.Character

        let currentJogWeight =
            match Array.tryFind (fun (a: Animation) -> a.Name = jogName) character.Animations with
            | Some a -> a.Weight
            | None -> 0.0f

        let targetJogWeight = if isMoving then 1.0f else 0.0f

        let jogWeight =
            if currentJogWeight < targetJogWeight then min targetJogWeight (currentJogWeight + blendRate)
            elif currentJogWeight > targetJogWeight then max targetJogWeight (currentJogWeight - blendRate)
            else currentJogWeight

        let idleWeight = 1.0f - jogWeight

        let idleAnimation character =
            let idleName = Character.idle character
            Animation.make gameTime None idleName Playback.Loop rate idleWeight None

        let jogAnimation character =
            let jogName = Character.jog character
            Animation.make gameTime None jogName Playback.Loop rate jogWeight None

        let updateOrCreate (makeNew: Character -> Animation) (weight: single) (character: CharacterProp) =
            let fresh = makeNew character.Character
            match Array.tryFind (fun (a: Animation) -> a.Name = fresh.Name) character.Animations with
            | Some existing -> { existing with Weight = weight }
            | None -> { fresh with Weight = weight }

        [| if idleWeight > 0.0f then updateOrCreate idleAnimation idleWeight character
           if jogWeight > 0.0f then updateOrCreate jogAnimation jogWeight character |]