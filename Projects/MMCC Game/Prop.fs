namespace MMCCGame
open System.Numerics
open Nu
open Prime

type PropType =
    | CharacterProp of Character

[<RequireQualifiedAccess>]
module PropType =
    let toPath = function
        | CharacterProp character -> Character.toPath character

type [<SymbolicExpansion>] AnimatedProp =
    { PropType : PropType
      Position : Vector3
      Rotation : Quaternion
      Animations : Animation array
      Morphs : (int * single) array }

[<RequireQualifiedAccess>]
module AnimatedProp =
    let toContent (name: string) (prop: AnimatedProp) =
        let propPath = PropType.toPath prop.PropType
        Content.entityFromFile name propPath
            [Entity.Position := prop.Position
             Entity.Rotation := prop.Rotation
             Entity.Animations := prop.Animations
             Entity.Morphs := prop.Morphs]
