namespace GameQuest3DImNui

open Prime

module Persistence =
    type SaveSlot =
        | Slot1
        | Slot2
        | Slot3
        | Slot4

    type SlotsState = Map<SaveSlot, System.IO.FileInfo>

    type PersistenceState =
        { Events: ProgressionEvent FQueue
          Location: Location }

    let getSlotFileName slot = 
        match slot with
        | Slot1 -> Assets.User.GameQuestSlot1
        | Slot2 -> Assets.User.GameQuestSlot2
        | Slot3 -> Assets.User.GameQuestSlot3
        | Slot4 -> Assets.User.GameQuestSlot4

    let getSlotName slot = 
        match slot with
        | Slot1 -> "Slot 1"
        | Slot2 -> "Slot 2"
        | Slot3 -> "Slot 3"
        | Slot4 -> "Slot 4"

    let getFileInfo slot =
        if System.IO.File.Exists (getSlotFileName slot) 
        then Some (System.IO.FileInfo(getSlotFileName slot))
        else None

    let getSlotsState () =
        [ Slot1; Slot2; Slot3; Slot4 ]
        |> List.map (fun slot -> slot, getFileInfo slot)
        |> List.choose (fun (slot, fileInfo) -> fileInfo |> Option.map (fun f -> slot, f))
        |> Map.ofList

    let getSlotText (slotState: SlotsState) (slot: SaveSlot) =
        slotState
        |> Map.tryFind slot
        |> Option.map (fun fileInfo -> fileInfo.LastWriteTime.ToShortDateString())
        |> Option.defaultValue (getSlotName slot)

    let save (slot: SaveSlot) (persistenceState: PersistenceState) =
        let stringified = scstring persistenceState
        do System.IO.File.WriteAllText((getSlotFileName slot), stringified)

    let hasSaveFile (slot: SaveSlot) =
        System.IO.File.Exists (getSlotFileName slot)

    let load (slot: SaveSlot): PersistenceState =
        let text = System.IO.File.ReadAllText (getSlotFileName slot)
        scvalue<PersistenceState> text

    let toProgression (persistenceState: PersistenceState) =
        let _, sm = Progression.initial
        let sm = StateMachine.zip persistenceState.Events sm
        persistenceState.Events, sm