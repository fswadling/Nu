namespace GameQuest3DImNui

open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    let TextCrawl1 = Game / "TextCrawl1"
    let TextCrawl2 = Game / "TextCrawl2"
    let MainMenu = Game / "MainMenu"

    let Explore = Game / "Explore"
    let ExploreGroup = Explore / "Group"
    let Zone = ExploreGroup / "Zone"
    let PlayerCharacter = ExploreGroup / "PlayerCharacter"

    let Battle = Game / "Battle"
    let BattleGroup = Battle / "Group"

    let TeamMember1 = BattleGroup / "TeamMember1"
    let TeamMember2 = BattleGroup / "TeamMember2"
    let TeamMember3 = BattleGroup / "TeamMember3"
    let TeamMember4 = BattleGroup / "TeamMember4"

    let Horns = BattleGroup / "Horns"

    let GameOver = Game / "GameOver"