namespace GameQuest3DImNui

open Nu

[<AutoOpen>]
module Actors =
    type Crystal =
        | Fire
        | Water
        | Air
        | Earth

        override this.ToString () =
            match this with
            | Fire -> "Fire"
            | Water -> "Water"
            | Air -> "Air"
            | Earth -> "Earth"

    module Crystal =
        let staticAsset = function
        | Fire
        | Water
        | Air
        | Earth -> Assets.Gameplay.Crystal

    type Companion =
        | Hero
        | BlueWitch
        | Knight
        | Thief

        override this.ToString (): string = 
            match this with
            | BlueWitch -> "BlueWitch"
            | Knight -> "Knight"
            | Thief -> "Thief"
            | Hero -> "Hero"

    module Companion =
        let animatedModel = function
            | BlueWitch -> Assets.Gameplay.BlueWitch
            | Knight -> Assets.Gameplay.Knight
            | Thief -> Assets.Gameplay.Thief
            | Hero -> Assets.Gameplay.MainCharacter

        let idle = function
            | BlueWitch
            | Knight
            | Thief
            | Hero ->
            Animation.loop GameTime.zero None "Armature|Idle"

        let running = function
            | BlueWitch
            | Knight
            | Thief
            | Hero ->
            Animation.loop GameTime.zero None "Armature|Running"

        let fightIdle = function
            | BlueWitch
            | Knight
            | Thief
            | Hero ->
            Animation.loop GameTime.zero None "Armature|FightIdle"

        let attack = function
            | Hero -> Animation.loop GameTime.zero None "Armature|Punch"
            | BlueWitch -> Animation.loop GameTime.zero None  "Armature|CastSpell"
            | Knight -> Animation.loop GameTime.zero None "Armature|PunchCombo"
            | Thief -> Animation.loop GameTime.zero None "Armature|Punching"

    type Enemy =
        | Horns

        override this.ToString (): string = 
            match Horns with
            | Horns -> "Horns"

    module Enemy =
        let animatedModel = function
            | Horns -> Assets.Gameplay.Horns

        let fightIdle = function
            | Horns -> Animation.loop GameTime.zero None "Armature|Idle"

        let attack = function
            | Horns -> Animation.loop GameTime.zero None "Armature|Bite"

    type Actor =
        | Crystal of Crystal
        | Companion of Companion
        | Enemy of Enemy

        override this.ToString () =
            match this with
            | Companion companion -> companion.ToString()
            | Enemy monster -> monster.ToString()
            | Crystal crystal -> $"{crystal} Crystal"

    module Actor =
        let idleAsset = function
            | Crystal crystal ->
                Choice1Of2 (Crystal.staticAsset crystal)
            | Companion companion ->
                let asset = Companion.animatedModel companion
                let animation = Companion.idle companion
                Choice2Of2 (asset, animation)
            | Enemy enemy ->
                let asset = Enemy.animatedModel enemy
                let animation = Enemy.fightIdle enemy
                Choice2Of2 (asset, animation)
