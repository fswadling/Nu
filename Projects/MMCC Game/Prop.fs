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
