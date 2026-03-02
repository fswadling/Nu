# State Management Approaches for Physics-Driven Entities in Nu

## The Problem

All three projects solve the same fundamental problem: managing a player character or any other prop (with position, rotation, animations) in a zone-based game where entities are scene-scoped but game state (like player data) needs to survive zone transitions. The core tension is between the entity's live state (affected by physics/input each frame) and the screen-level model state (needed for persistence and zone transitions).

---

## Project 1: MMCC Game — Model-View-Content with `:=` Bindings

**Architecture:** Uses the MMCC pattern via `ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand>`. The player is stored in a `Gameplay` model as an `AnimatedProp option`. The entity is declared via `Content.entityFromFile` with `:=` bindings.

**How it works:**

1. `StartPlaying` message initialises the player prop in the model from a waypoint.
2. `UpdatePlayer` message reads the entity's **live** position/rotation from the world (`player.GetPosition world`), computes movement/animations, and writes results back into the model.
3. The `:=` bindings push the updated model values back to the entity.

**Key code (`Gameplay.fs` — `UpdatePlayer` message):**

```fsharp
// This breaks the unidiretional model... Will lead to undefined behaviour.
// In any case editing the gameplay state wont touch this so it doesnt fix the need for
// effectful cues. Still though it seems to work.
let player = Simulants.GameplayPlayer
let entityRotation = player.GetRotation world
let position = player.GetPosition world
// ... compute movement ...
let prop = { prop with Position = position; Rotation = entityRotation; Animations = animations }
```

**Drawbacks:**

- **Breaks unidirectional data flow for physics read-back.** The `Message` handler reads mutable entity state from the `world` (position/rotation after physics) rather than purely from the model — violating MMCC's core principle in the entity → model direction.
- An `Edit` override provides a manual **"Sync Player from Entity"** button, highlighting that the entity → model direction isn't automatic.

**Advantages:**

- **Cues can be pure/model-driven.** Because `:=` bindings push model changes to the entity, cues can be implemented as `Message` handlers that simply return an updated model (e.g., setting `Player.Position`). The binding takes care of propagating those changes to the entity — no effectful entity manipulation needed.

---

## Project 2: ImSimWithStaticBoundState — ImSim with Static Property Assignment

**Architecture:** Uses `ScreenDispatcherImSim` (immediate-mode sim). The player `CharacterProp` is stored in screen-level state. The entity is declared via `World.doEntityFromFile` with `.=` (static assignment) — meaning prop values are pushed to the entity **only on initial creation**.

**How it works:**

1. `doEntity` declares the entity with `.=` static assignment (position/rotation only applied at first render).
2. `updatePlayer` reads the entity's live position/rotation, computes movement, and **directly sets them on the entity** via `SetPosition`/`SetRotation`.
3. Only animations are written back to the screen-level `CharacterProp`.

**Key code (`Gameplay.fs` — `doPlayer`):**

```fsharp
// This sets the player position and rotation using static assignment and lets them evolve unbounded.
// This means that the player position and rotation in the screen level state is not kept up to date
// with the entity and will only reflect the initial position and rotation.
// The problem with this is that if I want to do an action that removes the entity from the current scene
// with the intention of adding it back in later, the position and rotation changes will be lost.
do CharacterProp.doEntity Simulants.GameplayPlayer.Name world playerProp
// ... compute movement ...
do Simulants.GameplayPlayer.SetPosition position world
do Simulants.GameplayPlayer.SetRotation rotation world
{ playerProp with Animations = animations }
```

**Drawbacks:**

- **Screen-level state becomes stale.** Position and rotation in the screen model only reflect the initial values; once the entity starts moving, the model is permanently out of date.
- **Zone transitions lose state.** If the entity is removed from the scene (e.g. zone transition), the current position/rotation are lost because they were never reflected back to the model.
- **Manual reflection required.** To work around this you'd need to explicitly copy entity state back into the model before any scene-clearing operation — doable but inelegant.
- **Split control.** Some properties (animations) are driven by the model, while others (position/rotation) are driven directly on the entity, creating inconsistent control flow.
- **Cues must be effectful.** Since physical properties live on the entity, cues that manipulate positions/rotations would need to operate directly on entities rather than through the model.

---

## Project 3: ImSimWithReflectedState — ImSim with Entity-to-Model Reflection

**Architecture:** Also uses `ScreenDispatcherImSim`. The player `CharacterProp` is stored in screen-level state. The entity is declared via `World.doEntityFromFile` with a mix of `.=` (static) and `@=` (dynamic assignment) operators.

**How it works:**

1. On each frame, `doPlayer` checks if the entity exists.
   - **If it exists:** reflects the entity's live position/rotation back into the `CharacterProp`.
   - **If it doesn't exist:** uses the prop's stored values (enabling zone-transition recovery).
2. Computes movement updates on the prop.
3. Pushes the updated prop back to the entity via `doEntity` with `@=` dynamic assignment.
4. Stores the updated prop back in screen state.

**Key code (`Gameplay.fs` — `doPlayer`):**

```fsharp
// This will bind the animations and props from the screen prop,
// as well as the position and rotation on the first render.
// afterwards, control is handed over to the game and physics engine,
// and the current position and rotation are reflected back to the screen
// state. This is obviously not true binding as changing the state at the screen
// level will have no impact on the entity and will immediatly be overwritten.
// Buut it does give me something at the screen level that I can save and use when
// initialising, as well as put away into storage in the gamestate when I do zone transitions.
let playerProp =
    if Simulants.GameplayPlayer.GetExists world
    then { playerProp with
             Position = Simulants.GameplayPlayer.GetPosition world
             Rotation = Simulants.GameplayPlayer.GetRotation world }
    else playerProp
// ... compute movement, push back via doEntity ...
```

**Drawbacks:**

- **Not true binding.** Changing position/rotation at the screen level has no effect — the entity's live state immediately overwrites it on the next frame.
- **One-way reflection only.** It's entity → model, never model → entity (except at initial creation). This is a "read-back" pattern, not a genuine two-way sync.
- **Feels hacky.** It gives a saveable/transferable state at the screen level (useful for zone transitions and persistence), but the screen state is really just a mirror of the entity, not the source of truth.


---

## Comparison Table

| Concern | MMCC (`:=` bindings) | ImSim Static (`.=`) | ImSim Reflected (`@=` + read-back) | My Game 2 (fully effectful) |
|---|---|---|---|---|
| **Screen state stays current** | ✅ Yes (model updated each frame) | ❌ No (stale after first frame) | ✅ Yes (reflected each frame) | N/A (no model) |
| **Survives zone transitions** | ✅ Yes (in model) | ❌ No (lost on entity removal) | ✅ Yes (reflected to screen state) | ❌ Must be manually choreographed |
| **Respects unidirectional flow** | ❌ No (reads world in Message) | N/A (ImSim is imperative) | N/A (ImSim is imperative) | N/A (imperative by design) |
| **True two-way binding** | ❌ No | ❌ No | ❌ No | N/A (no binding) |
| **Cues can be pure/model-driven** | ✅ Yes (`:=` pushes model to entity) | ❌ No | ❌ No | ❌ No (all effectful) |
| **Consistent property control** | ✅ All through model | ❌ Split (model + direct entity) | ⚠️ Model is a mirror, entity is source of truth | ✅ All direct on entity |
| **Rich scripting / cross-zone ops** | ❌ Would need custom layer | ❌ Would need custom layer | ❌ Would need custom layer | ✅ Built-in via Durable CE |
| **Serialisable state** | ✅ Model is serialisable | ⚠️ Partial (model is stale) | ✅ Reflected state is serialisable | ❌ No model to serialise |

## What Would Solve This

A mechanism at the engine level that provides **true bidirectional binding** between entity physical properties and screen/model-level state — so that:

1. Physics changes on the entity automatically propagate to the model.
2. Model changes (from cues, zone transitions, save/load) automatically propagate to the entity.
3. Cues and game logic can remain pure/model-driven without needing effectful entity manipulation.
4. Cross-zone operations and rich scripting (à la My Game 2's `Durable` CE) can coexist with model-driven state.
