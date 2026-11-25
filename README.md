# EventHorizon `v0.0.1-alpha`

> **Status: Very Early Alpha (Experimental)** — EventHorizon is an active research/engineering prototype exploring purely functional event-driven 3D games, pointer-memoized tree diffing, and deterministic rollback multiplayer. Expect breaking API changes as the design evolves.

**Purely functional, event-driven 3D games and rigid-body physics in Haskell**, backed by a self-contained **Three.js** + **Rapier3D** runtime.

In **EventHorizon**, your Haskell game logic **never runs on 60 Hz frame ticks**. Three.js and Rapier3D simulate and render continuously at 60 FPS, waking your pure Haskell update function only when a discrete event occurs—player input, a physics collision/sensor trigger, or a declared timer.

---

## Quick Start: Run the Example Games

Requires **GHC ≥ 9.6** and **Cabal ≥ 3.0**. The browser frontend (`static/bundle.js`) is pre-built, so no Node.js setup is needed for single-player.

```bash
# 1. Sky Ace — 3D flight & dogfight → open http://localhost:3000
cabal run sky-ace

# 2. Chamber Shift — 3-level 3D physics puzzle → open http://localhost:3001
cabal run chamber-shift

# 3. Bastion Break — 3-fortress ballistic demolition → open http://localhost:3002
cabal run bastion-break

# 4. YoursCraft — Minecraft-like 3D voxel builder & diffing stress test → open http://localhost:3003
cabal run yours-craft
# Compare no-diff vs flat-diff vs memoized chunked SceneTree diffing (up to ~40,000 blocks):
cabal run yours-craft -- --bench
```

Want to try **2-player Sky Ace** right away? *(Requires Node.js installed for the headless Rapier3D physics sidecar, plus `npm --prefix ./frontend install` once if `frontend/node_modules` is not present.)*

```bash
# Open http://localhost:3000/?player=1 and http://localhost:3000/?player=2 in two tabs
cabal run sky-ace -- --local-multi
```

Want to build **Sky Ace** from scratch step-by-step? Follow the **[7-Step Sky Ace Tutorial](tutorial/sky-ace/TUTORIAL.md)** and run any stage directly:

```bash
cabal run sky-ace-tutorial -- 1   # Steps 1..7 on http://localhost:3000
```

See [Example Games](#example-games) below for full controls and networked server/client multiplayer instructions.

---

## Core Idea

### Single-Player (`play` / `playWith`)

Write a 3D action, puzzle, or demolition game with two pure functions parameterized by your own **type-safe identifier types**:

- **`bid`** — your **body ID** type identifying 3D physics/visual entities (e.g., `data MyBody = Player | Tree Int | Bullet Int`)
- **`tid`** — your **timer ID** type identifying scheduled timers (e.g., `data MyTimer = SpawnWave | BulletExpire Int`, or `Void` if unused)

```haskell
import Game.EventHorizon

play
  :: (Ord bid, Ord tid, Show bid, Show tid)
  => Config
  -> world
  -> (world -> Scene bid tid)                           -- Declarative 3D SceneTree, physics, timers & HUD
  -> (Event bid tid -> Snapshot bid -> world -> world)  -- Pure update: runs ONLY on discrete events
  -> IO ()
```

Use `playWith` when your update function also emits one-shot physics or audio commands (`ApplyImpulse`, `ApplyTorqueImpulse`, `SetVelocity`, `Teleport`, `PlaySound`):

```haskell
playWith
  :: (Ord bid, Ord tid, Show bid, Show tid)
  => Config
  -> world
  -> (world -> Scene bid tid)
  -> (Event bid tid -> Snapshot bid -> world -> (world, [Command bid]))
  -> IO ()
```

### Multi-User (`Game pid bid tid world`)

Turn a pure game into a networked multi-player game with **zero networking or rollback code** by adding a user-defined **`pid`** (**player ID** type, e.g., `data Pilot = Pilot1 | Pilot2`) alongside **`bid`** (body ID) and **`tid`** (timer ID), and separating the shared 3D world (`WorldScene`) from each player's viewport (`PlayerView`: camera + HUD):

```haskell
data Game pid bid tid world = Game
  { gameInit  :: world
  , gameWorld :: world -> WorldScene bid tid
  , gameView  :: pid -> world -> PlayerView bid
  , gameStep  :: MultiEvent pid bid tid -> Snapshot bid -> world -> (world, [MultiCommand pid bid])
  }
```

Run the exact same `Game` locally across browser tabs (`playLocalMulti`) or over a network (`serveMulti` + `connectMulti`) with client-side prediction, deterministic Rapier3D snapshot rollback, sound deduplication, and visual error smoothing.

---

## Highlights

- **Event-driven hybrid execution**: Continuous 60 Hz physics, drives (`WalkDrive`, `VelocityDrive`, `ForceDrive`, `SeekDrive`, `FlightDrive`), and kinematic motion run in Rapier3D/Three.js; Haskell wakes only on discrete `KeyDown`/`KeyUp`, `MouseDown`/`MouseUp` (with 3D raycast picking), `CollisionStart`/`CollisionEnd`, and `TimerFired` events.
- **Type-safe user-defined IDs (`pid`, `bid`, `tid`)**: Define ordinary ADTs for players (`pid`), 3D bodies (`bid`), and timers (`tid`) with `deriving (Eq, Ord, Show)`. The protocol transparently interns them to compact integer wire IDs—no string prefix parsing or JSON boilerplate.
- **Ergonomic 3D math & mesh algebra**: `Vec3` supports `Num` (`+`, `-`, `negate`) and vector operators (`^+^`, `^-^`, `*^`, `^*`, `^/`, `<.>`, `><`); `Quat` and `Mesh` form `Semigroup`/`Monoid` (`<>`, `mempty`), with `(#>)` for quaternion-vector rotation.
- **$O(\Delta \log N)$ incremental scene diffing**: Compose scenes as a `SceneTree` using `memo`/`memo2`/`memo3` and `mapTree`/`intMapTree`. Untouched persistent subtrees reuse their previous heap pointers via 2-generation + weak `StableName` memoization and skip diffing in $O(1)$ (with rotation-safe reconciliation for `Data.Map`).
- **Input-only multiplayer & rollback**: Authoritative Haskell + headless Node Rapier3D server broadcasts stamped player inputs and lightweight periodic confirmations; predicting clients roll back pure `world` values in $O(1)$ alongside Rapier3D snapshots and smoothly decay visual corrections.
- **Zero-asset procedural Web Audio**: Synthesize frequency sweeps, tones, and filtered noise directly in the browser via `PlaySound`.

---

## Performance: With vs. Without Incremental Diffing

### Real-World Stress Test: YoursCraft (`cabal run yours-craft -- --bench`)

Every time you click to place or break a single block in **YoursCraft**, the world is stored as nested persistent maps of 8×8 chunks (`Map ChunkCoord (Map VoxelCoord BlockKind)`). Below is the measured cost per click (render + diff + JSON wire encoding) across 1,706 to 44,865 blocks (GHC 9.6 `-O2`):

| World Size | No Diffing (Full Scene JSON) | Naive Flat Diff (`bodies [...]`) | Memoized Chunked `SceneTree` (`mapTree`) | Speedup (Memo vs. No-Diff / Flat) |
| ---: | ---: | ---: | ---: | ---: |
| **1,706 blocks** (36 chunks) | `140.55 ms` · `720.5 KB` | `3.49 ms` · `696 B` | **`0.143 ms`** · **`696 B`** | **984×** / **24.4×** |
| **10,192 blocks** (144 chunks) | `1,005.31 ms` · `4.2 MB` | `35.28 ms` · `700 B` | **`0.565 ms`** · **`700 B`** | **1,780×** / **62.5×** |
| **44,865 blocks** (576 chunks) | `4,579.31 ms` · `18.6 MB` | `185.16 ms` · `700 B` | **`2.793 ms`** · **`700 B`** | **1,639×** / **66.3×** |

- **No Diffing**: Rebuilding and serializing the full scene takes **4.6 seconds** and **18.6 MB** per click at 44,865 blocks—completely unplayable.
- **Naive Flat Diff**: Server-side diffing slashes network payload **27,000×** (down to **700 bytes**), but walking an unmemoized flat list still costs **185 ms** per click (dropping >11 frames at 60 FPS).
- **Memoized Chunked `SceneTree`**: Skipping untouched chunks in $O(1)$ via `memo` + `reallyUnsafePtrEquality#` brings the entire render + diff + JSON encode cycle down to **2.79 ms** at 44,865 blocks (**0.14 ms** at 1,706 blocks)—comfortably inside a single 16.6 ms 60 FPS frame.

*(You can also press **`M`** live inside `yours-craft` to switch between Memoized Chunked `SceneTree` and Naive Flat Rebuild while playing.)*

### Synthetic Microbenchmark (`cabal run event-horizon-bench`)

Pure render + diff latency for 1 non-local entity mutation per step across 50 steps (GHC 9.6 `-O2`):

| Bodies ($N$) | Naive Flat Diff | `intMapTree` + `memo` | `mapTree` + `memo` |
| ---: | ---: | ---: | ---: |
| **1,000** | `0.274 ms/step` | `0.0064 ms/step` (**42.7×**) | `0.0070 ms/step` (**39.3×**) |
| **10,000** | `4.772 ms/step` | `0.1876 ms/step` (**25.4×**) | `0.0156 ms/step` (**306.0×**) |
| **50,000** | `32.808 ms/step` | `0.1078 ms/step` (**304.4×**) | `0.0258 ms/step` (**1,271.6×**) |

---

## Example Games

All three example games are self-contained and serve a pre-built browser bundle (`static/bundle.js`).

> **Prerequisites**: GHC ≥ 9.6, Cabal ≥ 3.0. Node.js is only needed if you run multiplayer modes (`--local-multi` / `--server` / `--connect`, which use the headless Rapier3D WASM sidecar) or rebuild `static/bundle.js`.

### 1. Sky Ace — 3D Flight & 2-Player Dogfight (`sky-ace`)

Fly through a forested valley, shoot down patrolling wave targets, avoid crashing into trees or terrain, and optionally dogfight a second pilot in multiplayer.

```bash
# Single-player — open http://localhost:3000
cabal run sky-ace

# Local 2-player — open http://localhost:3000/?player=1 and http://localhost:3000/?player=2
cabal run sky-ace -- --local-multi

# Networked multiplayer (authoritative server + predicting rollback clients)
cabal run sky-ace -- --server --port 4000
cabal run sky-ace -- --connect localhost:4000 --port 3000
cabal run sky-ace -- --connect localhost:4000 --port 3001
```

- **W / S** or **↑ / ↓**: Climb / Dive
- **A / D** or **← / →**: Turn (with auto-banking)
- **Shift**: Boost · **Space / Left Click**: Shoot · **R**: Restart

### 2. Chamber Shift — 3D Physics Puzzle (`chamber-shift`)

Solve **3 handcrafted 3D puzzle chambers** by pushing physics crates onto floor pressure plates, flipping airlock toggles, and racing timed pedestal switches to open sliding kinematic doors.

```bash
cabal run chamber-shift
```

Open **http://localhost:3001**:
- **WASD / Arrow Keys**: Walk & push crates
- **E** or **Click nearby pedestal**: Press button
- **R**: Reset level · **1 / 2 / 3**: Jump to Level 1, 2, or 3

### 3. Bastion Break — 3D Siege Demolition (`bastion-break`)

Demolish **3 destructible 3D fortresses** by clicking directly on stone arches, timber towers, and TNT barrels to fire parabolic cannonballs and explosive bombs, triggering radial `ApplyImpulse` shockwaves and chain reactions to topple every Golden Idol.

```bash
cabal run bastion-break
# Optional: start with discrete FixedCamera steps instead of the smooth CameraPivot rig
cabal run bastion-break -- --step-camera
```

Open **http://localhost:3002**:
- **Left Click on fortress**: Fire ballistic shot directly at the clicked 3D point
- **Q / Tab / Right Click**: Switch between Iron Cannonball and Explosive Bomb
- **A / D** or **← / →**: Orbit camera around the island
- **C**: Toggle live between smooth 60 FPS `CameraPivot` orbit and discrete `FixedCamera` steps
- **Space / Enter**: Advance fortress · **R**: Retry · **1 / 2 / 3**: Select fortress

### 4. YoursCraft — 3D Voxel Builder & Diffing Stress Test (`yours-craft`)

Explore a procedural chunked 3D voxel landscape of rolling hills, ridges, and trees, breaking and placing blocks on exact raycast faces—designed specifically to stress-test EventHorizon's incremental `SceneTree` + `memo` diffing engine.

```bash
# Playable world (~1,700 blocks) — open http://localhost:3003
cabal run yours-craft

# Customize world radius or disable memoization live:
cabal run yours-craft -- --radius 24 --port 3003
cabal run yours-craft -- --no-memo

# Run the built-in with-diffing vs without-diffing benchmark (~1.7k / 10k / 40k blocks):
cabal run yours-craft -- --bench
```

- **WASD / Arrow Keys**: Walk · **Space**: Jump (`ApplyImpulse`) · **Q / E**: Rotate camera
- **Left Click**: Break block · **Right Click**: Place block on clicked face
- **1–6**: Select block (Grass, Dirt, Stone, Wood, Brick, Glowstone)
- **M**: Toggle live between Memoized Chunked `SceneTree` and Naive Flat Rebuild · **R**: Reset world

---

## Project Structure

```text
src/Game/EventHorizon.hs          Top-level re-export
src/Game/EventHorizon/
  Math.hs                         Vec3, Quat, Pose, Color & math operators
  Types.hs                        Events, Keys, MouseButton, Snapshot, Sound, Command
  Memo.hs                         2-generation + weak StableName memo / memo2 / memo3
  Scene.hs                        Mesh, Material, Collider, Drive, Body, SceneTree, mapTree/intMapTree
  Diff.hs                         O(delta log N) pointer-equality tree diff + rotation reconciliation
  Protocol.hs                     Bidirectional wire ID interning & delta JSON protocol
  Engine.hs                       Single-player Warp/WebSocket runtime (play, playWith)
  Multi.hs                        Multi-user Game, WorldScene, PlayerView, MultiEvent
  Rollback.hs                     TickTimers, 180-tick history buffer, confirmServerTick, hashSnapshot
  MultiEngine.hs                  playLocalMulti, serveMulti, connectMulti & headless Rapier bridge
frontend/src/
  engine.js                       Three.js + Rapier3D browser runtime, Web Audio & visual error smoothing
  headless-physics.mjs            Headless Node Rapier3D WASM sidecar for deterministic server/client ticks
examples/{sky-ace,chamber-shift,bastion-break,yours-craft}/  Four complete example games
tutorial/sky-ace/                                7-step incremental Sky Ace tutorial & walkthrough
bench/DiffBench.hs                               Large-scene incremental diff benchmark
test/Spec.hs                                     Unit, diff, memo, gameplay, and rollback tests
```

---

## Testing, Benchmarks & Frontend Build

```bash
# Run the full test suite
cabal test

# Run the 1k / 10k / 50k body incremental diff benchmark
cabal run event-horizon-bench

# Rebuild static/bundle.js after editing frontend/src/engine.js
npm --prefix ./frontend install
npm --prefix ./frontend run build
```

---

## Future Work

- **GHC WebAssembly (`wasm32-wasi` + JSFFI) backend**: EventHorizon's core (`Math`, `Types`, `Memo`, `Scene`, `Diff`, `Protocol`, `Multi`, `Rollback`) and all four example games are pure Haskell with no native OS dependencies—only `Engine.hs` and `MultiEngine.hs` depend on Warp/WebSockets/process I/O. Compiling games to WebAssembly via GHC's WASM backend will enable:
  - **Zero-install browser demos** (playable directly on GitHub Pages with no local GHC/Cabal install).
  - **In-browser multiplayer prediction**: Running the predicting Haskell client and Rapier3D WASM side-by-side in the same browser tab, connecting directly to `serveMulti` over WebSocket without a local Haskell client process.
- **Composable continuous controllers**: Replacing built-in `Drive` presets (`FlightDrive`, `WalkDrive`, `SeekDrive`) with a small, composable declarative controller algebra executed at 60 Hz in the physics runtime.
- **Merkle-tree state resync**: Leveraging `memo`-hashed persistent trees for branch-level delta recovery on rare multiplayer desyncs or late joins.

---

## License

MIT
