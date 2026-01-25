
# Game Design Document (GDD) - My Game 2

---

## 1. Game Overview

**Title:** My Game 2  
**Engine:** Nu Game Engine (F#)  
**Target Framework:** .NET 10.0  
**Genre:** 3D Narrative Adventure

My Game 2 is a modular, narrative-driven 3D adventure game. It features a flexible zone/scenario system, immediate-mode simulation, a robust scripting engine, and support for dynamic, branching storylines.

---

## 2. Core Architecture

- **Immediate-Mode Simulation:** Uses Nu's ImSim architecture for real-time, frame-based world updates.
- **Component-Based Entities:** Entities are composed of facets (e.g., NavigableFacet, InteractableFacet) for modular behavior.
- **Script-Driven:** Game flow, cutscenes, and events are controlled by a powerful scripting system.

---

## 3. Zone / Scenario System (Detailed)

### 3.1 Zones
- **Definition:** A Zone represents a major area or level in the game (e.g., town, dungeon, overworld).
- **Implementation:**
  - Defined as a discriminated union (e.g., `NoZone`, `Forest`, `Castle`).
  - Each zone is mapped to a Nu `Screen`.
  - Properties:
    - `Zone`: The zone type (enum value)
    - `Scenarios`: Set of active scenarios in this zone
    - `PlayerScenario`: The scenario currently being experienced by the player
  - Each zone can have associated data: name, asset path, background music, etc.

### 3.2 Scenarios
- **Definition:** A Scenario is a self-contained narrative or gameplay sequence (e.g., cutscene, quest, event) that can be activated within a zone.
- **Implementation:**
  - Defined as a discriminated union (e.g., `Intro`, `BossFight`, `RescueEvent`).
  - Each scenario can have a name and asset path.
  - Managed as a set on each zone screen (`Scenarios` property).
  - The `PlayerScenario` property tracks the currently active scenario for the player.

### 3.3 System Dynamics
- When a zone screen is active, it loads its associated group (level data) and all active scenarios.
- Scenarios can be triggered, added, or removed at runtime via script operations (`AddScenario`, `RemoveScenario`).
- Scripts can also set the player's current scenario (`SetPlayerScenario`).
- This enables dynamic narrative progression, branching storylines, and modular content.

### 3.4 Example Flow
1. Player enters a new zone (e.g., `Zone1`).
2. The zone screen loads the base environment and any active scenarios (e.g., an intro cutscene).
3. As the player progresses, scripts can add or remove scenarios (e.g., start a boss fight, trigger a dialogue).
4. The player can be moved to a new zone, and the process repeats.

### 3.5 Script Operations (Relevant)
- `AddScenario (zone, scenario)`: Adds a scenario to a zone.
- `RemoveScenario (zone, scenario)`: Removes a scenario from a zone.
- `SetPlayerScenario (scenario)`: Sets the current player scenario.
- `SwitchToZone (zone)`: Changes the active zone.

### 3.6 Benefits
- **Modularity:** Zones and scenarios can be developed and tested independently.
- **Flexibility:** Supports dynamic, script-driven changes to the game world and narrative.
- **Scalability:** New zones and scenarios can be added with minimal code changes.

---

## 4. Gameplay Systems

### 4.1 Character System
- Characters are defined as discriminated unions (e.g., `Placeholder`).
- Each character can have a name, asset path, and emote height.
- Characters can be spawned, moved, or removed via scripts.

### 4.2 Player Controls
- **Movement:**
  - W/S: Move forward/backward
  - A/D: Turn left/right
  - Smooth animation blending between "Idle" and "Jog" states
- **Interaction:**
  - E: Interact with objects (if within range and interactable)

### 4.3 Camera System
- **Follow Camera:** Follows behind the player, offset above character.
- **Dolly Camera:** Follows a predefined rail path, aiming at the player.
- **Scripted Camera:** Camera can be moved or animated via script operations.

### 4.4 Interaction System
- Objects can be made interactable with prompts, interaction distance, and custom scripts.

### 4.5 Trigger Volumes
- Physics-based triggers (e.g., for events, cutscenes) using sphere shapes and sensors.
- Can queue scripts when entered by the player.

---


## 5. Cue System (Narrative/Event System)

- **Durable State Machine:** Functional, serializable coroutines for cutscenes, events, and gameplay logic.
- **Cue Operations:**
  - Instant: Print, PlaySong, Enable/Disable entities, Move/Add/Remove characters, Switch zones, etc.
  - Control Flow: AwaitSignal, Fork, IfElse, Loop
  - Timed: Wait, FadeIn/Out, Animation, Dialogue, Camera movement
- **Signal Conditions:** Cues can wait for complex conditions (all/any signals, specific events).
- **Exposition System:** Supports dialogue, thoughts, and portrait dialogue with timing and fast-forward.

---


## 6. Progression System

- **Advents:** Story milestones tracked in a persistent queue.
- **Cue Progression Modes:** Manual, Automatic, FastForward (accelerated playback).
- **Branching:** Cues can branch or loop based on advents.

---

## 7. Asset Structure

```
Assets/
??? Gui/
?   ??? Title.nugroup      # Title screen layout
?   ??? Credits.nugroup    # Credits screen layout
??? [Additional asset packages]

AssetGraph.nuag            # Asset dependency graph
```

---

## 8. Technical Notes

- **.NET 10.0** target, leveraging modern F# features.
- **Nu Engine**: Immediate-mode simulation, component/facet-based architecture.
- **Post-build asset pipeline** via Nu.Pipe.
- **Warnings as errors** for strict code quality.
- **Server GC and Tiered PGO** enabled for performance.

---

## 9. Future Development

- Expand `Zone` and `Scenario` unions for more content.
- Add more character types and assets.
- Implement additional scenario logic and branching.
- Integrate more audio, animation, and visual assets.

---

This GDD provides a comprehensive overview of My Game 2's architecture, systems, and design philosophy, with a focus on the flexible and powerful zone/scenario system that underpins its narrative and gameplay structure.
