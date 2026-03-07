namespace MyGame3
open Nu
open Prime
open System.Numerics

type AppearProgress = single

type PortraitExpression = 
    | Neutral

type ExpositionPortrait =
    | PlayerPortrait of PortraitExpression

module ExpositionPortrait =
    let toPath = function
        | PlayerPortrait Neutral -> "Assets/Gameplay/Characters/Player/Portraits/PlayerNeutral.png"

type ExpositVariant =
    | Thought
    | Dialogue
    | PortraitDialogue of ExpositionPortrait

type ExpositPhase =
    | AppearPhase
    | DisplayPhase
    | DisappearPhase

type [<SymbolicExpansion>] Exposition =
    { Text: string
      Variant: ExpositVariant
      Phase: ExpositPhase
      AppearProgress: AppearProgress }

[<RequireQualifiedAccess>]
module Exposition =
    let appearRate = 3.0f // progress per second (0→1 in ~0.33s)
    let defaultSize = v2 400.0f 60.0f
    let defaultPosition = v2 0f 140f
    let dialogueSize = v2 400.0f 120.0f
    let dialoguePosition = v2 0f 136f

    let make text variant =
        { Text = text
          Variant = variant
          Phase = AppearPhase
          AppearProgress = 0.0f }

    let update (deltaSeconds: single) (exposition: Exposition) =
        match exposition.Phase with
        | AppearPhase ->
            let progress = exposition.AppearProgress + appearRate * deltaSeconds
            if progress >= 1.0f
            then Some { exposition with Phase = DisplayPhase; AppearProgress = 1.0f }
            else Some { exposition with AppearProgress = progress }
        | DisplayPhase ->
            Some exposition
        | DisappearPhase ->
            let progress = exposition.AppearProgress - appearRate * deltaSeconds
            if progress <= 0.0f then None
            else Some { exposition with AppearProgress = progress }

    let advance (exposition: Exposition) =
        match exposition.Phase with
        | AppearPhase | DisplayPhase ->
            { exposition with Phase = DisappearPhase }
        | DisappearPhase ->
            exposition

    let position (exposition: Exposition) =
        match exposition.Variant with
        | Thought -> defaultPosition
        | Dialogue | PortraitDialogue _ -> dialoguePosition

    let size (exposition: Exposition) =
        match exposition.Variant with
        | Thought | Dialogue -> defaultSize
        | PortraitDialogue _ -> dialogueSize

