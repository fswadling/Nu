namespace GameQuest3DImNui

open System.Numerics

[<AutoOpen>]
module Domain =
    type Position = Vector3
    type Rotation = Quaternion
    type Radius = single
    type Tag = string
    type Trigger = Tag * Position * Radius
    type LabelSize = Vector3
    type Prompt = string
    type Response = string
    type TextCrawl = string
