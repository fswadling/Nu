namespace GameQuest3DImNui
open System
open Prime
open Nu

// this module contains asset constants that are used by the game.
// having an Assets module is optional, but can prevent you from duplicating string literals across the code base.
[<RequireQualifiedAccess>]
module Assets =

    // these are assets from the Gui package. Note that we don't actually have any assets here yet, but they can be
    // added to the existing package at your leisure!
    [<RequireQualifiedAccess>]
    module Gui =

        let PackageName = "Gui"

    // these are assets from the Gui package. Also no assets here yet.
    [<RequireQualifiedAccess>]
    module Gameplay =

        let PackageName = "Gameplay"

        let TestPackage = "Test"

        let StartingZone = asset<StaticModel> TestPackage "StartingArea"
        let SideZone = asset<StaticModel> TestPackage "SideArea1"
        let SideZone2 = asset<StaticModel> TestPackage "SideArea2"
        let SideZone3 = asset<StaticModel> TestPackage "SideArea3"
        let CrystalsHub = asset<StaticModel> TestPackage "CrystalsHub"
        let CrystalZone1 = asset<StaticModel> TestPackage "CrystalZone1"
        let MainCharacter = asset<AnimatedModel> TestPackage "MainCharacter"
        let BlueWitch = asset<AnimatedModel> TestPackage "BlueWitch"
        let Knight = asset<AnimatedModel> TestPackage "Knight"
        let Thief = asset<AnimatedModel> TestPackage "Thief"
        let Crystal = asset<StaticModel> TestPackage "Crystal"
        let Horns = asset<AnimatedModel> TestPackage "Horns"
        let FieldSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> TestPackage "field" }
        let FightSong = { FadeInTime = 0L; FadeOutTime = Constants.Audio.FadeOutTimeDefault; StartTime = 0L; RepeatLimitOpt = None; Volume = Constants.Audio.SongVolumeDefault; Song = asset<Song> TestPackage "fight" }

    module User = 
        let GameQuestSlot1 = "Slot1.sav"
        let GameQuestSlot2 = "Slot2.sav"
        let GameQuestSlot3 = "Slot3.sav"
        let GameQuestSlot4 = "Slot4.sav"