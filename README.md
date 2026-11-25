# EventHorizon `v0.0.1-alpha`

> **Status: Very Early Alpha (Experimental)** — EventHorizon is an early-stage research and engineering prototype exploring purely functional event-driven 3D games, pointer-memoized tree diffing, and deterministic rollback multiplayer. Expect breaking API changes as the design evolves.

**Write 3D action, puzzle, demolition, voxel, and networked multiplayer games in pure Haskell—without frame loops, mutable scene graphs, manual diffing, or networking code.**

---

## Quick Start: Play the Example Games

Requires **GHC ≥ 9.6** and **Cabal ≥ 3.0**. The browser runtime (`static/bundle.js`) is pre-built, so no Node.js setup is needed for single-player.

```bash
# 1. Sky Ace — 3D flight & dogfight → open http://localhost:3000
cabal run sky-ace

# 2. Chamber Shift — 3-level 3D physics puzzle → open http://localhost:3001
cabal run chamber-shift

# 3. Bastion Break — 3-fortress ballistic demolition → open http://localhost:3002
cabal run bastion-break

# 4. YoursCraft — Minecraft-like 3D voxel builder & diffing stress test → open http://localhost:3003
cabal run yours-craft
```

Want to try **2-player Sky Ace** right away? Multiplayer uses **Node.js (≥ 18)** for the headless Rapier3D physics sidecar:

```bash
# 1. Install Node.js & npm if you don't already have them:
#    Ubuntu / Debian:  sudo apt install nodejs npm
#    macOS (Homebrew): brew install node
#    (or download from https://nodejs.org)

# 2. Install the Rapier3D sidecar dependency (once):
npm --prefix ./frontend install

# 3. Start the 2-player server, then open two browser tabs:
#    http://localhost:3000/?player=1  and  http://localhost:3000/?player=2
#    (Or on the same Wi-Fi/LAN, Player 2 can open http://<your-ip-or-hostname>:3000/?player=2
#     in any browser—no Haskell or Node.js needed on the second machine!)
cabal run sky-ace -- --local-multi
```

Want to build **Sky Ace** from scratch? Follow the **[7-Step Sky Ace Tutorial](tutorial/sky-ace/TUTORIAL.md)** and run any stage directly:

```bash
cabal run sky-ace-tutorial -- 1   # Steps 1..7 on http://localhost:3000
```

---

## The Programmer Experience

### What You Do *Not* Have to Care About

When writing a game in EventHorizon, you never touch:

- **60 Hz frame loops or `dt` integration** — your Haskell code never runs on animation frames.
- **WebGL, shaders, Three.js handles, or GPU memory management** — you return a pure declarative 3D tree.
- **Physics engine plumbing or imperative rigid-body handles** — you declare colliders, collision masks, and continuous `Drive`s.
- **Stringly-typed entity IDs or JSON serialization** — you tag bodies, timers, and players with your own ordinary Haskell ADTs.
- **Manual diffing or dirty flags for huge worlds** — persistent `Map`/`IntMap` subtrees stay $O(\Delta \log N)$ automatically via `memo` and `mapTree`/`intMapTree`.
- **Sockets, client-side prediction, rollback buffers, or state sync** — you write a multiplayer game as a pure local state machine over player IDs.
- **External audio files** — procedural Web Audio sound effects (`sweep`, `tone`, `noise`) are plain data values.

### What You Actually Write

#### Single-Player: Two Pure Functions

You define your own pure game state and **type-safe identifier types**—for 3D bodies and timers (`Void` if unused)—and write just an initial state plus two pure functions:

```haskell
import Game.EventHorizon

-- User-defined domain types (no JSON instances or string IDs needed)
data World   = ...                             -- your pure game state
data BodyId  = Player | Tree Int | Bullet Int  deriving (Eq, Ord, Show)
data TimerId = SpawnWave | BulletExpire Int    deriving (Eq, Ord, Show)

initWorld :: World
render    :: World -> Scene BodyId TimerId                     -- declarative 3D scene, physics, timers & HUD
step      :: Event BodyId TimerId -> Snapshot BodyId -> World -> World  -- runs ONLY on discrete events

main :: IO ()
main = play (defaultConfig "My Game") initWorld render step
```

When you also want one-shot physics or audio actions (`ApplyImpulse`, `ApplyTorqueImpulse`, `SetVelocity`, `Teleport`, `PlaySound`), return a list of commands and run with `playWith`:

```haskell
step :: Event BodyId TimerId -> Snapshot BodyId -> World -> (World, [Command BodyId])

main :: IO ()
main = playWith (defaultConfig "My Game") initWorld render step
```

Along the way you get idiomatic 3D math and scene combinators:
- **`Vec3`**: `Num` (`+`, `-`, `negate`) plus vector operators (`^+^`, `^-^`, `*^`, `^*`, `^/`, `<.>`, `><`)
- **`Quat` & `Mesh`**: `Semigroup` / `Monoid` (`<>`, `mempty`), plus `(#>)` to rotate a vector by a quaternion
- **Typed collision matching**: `matchPair`, `matchPred`, `matchEq`, and `only` so unordered collisions pattern-match exhaustively without string prefixes

#### Multi-Player: Three Pure Functions

To make a game multi-player, add a user-defined player ID type and split `render` into the shared 3D world and each player's viewport (camera + HUD):

```haskell
data PlayerId = Pilot1 | Pilot2 deriving (Eq, Ord, Show, Enum, Bounded)

initWorld   :: World
renderWorld :: World -> WorldScene BodyId TimerId              -- shared 3D world, physics & timers
renderView  :: PlayerId -> World -> PlayerView BodyId          -- per-player camera & HUD
step        :: MultiEvent PlayerId BodyId TimerId
            -> Snapshot BodyId
            -> World
            -> (World, [MultiCommand PlayerId BodyId])
```

You can test `step` directly in pure Haskell unit tests, run the game locally/over LAN (`playLocalMulti`), or deploy it as an authoritative server (`serveMulti`) with predicting rollback clients (`connectMulti`)—without changing a single line of game logic.

---

## How It Works Under the Hood

Giving the programmer a purely functional, local-feeling model while sustaining 60 FPS 3D physics, tens of thousands of bodies, and smooth multiplayer rests on four architectural mechanisms:

### 1. Hybrid Continuous/Discrete Split (Zero Frame Ticks in Haskell)

EventHorizon splits the game cleanly at the boundary between **continuous dynamics** and **discrete game logic**:

- **Continuous 60 Hz runtime (Three.js + Rapier3D)**: Rigid-body dynamics, collisions, constraints, kinematic interpolation, camera tracking, and declarative continuous controllers (`WalkDrive`, `VelocityDrive`, `ForceDrive`, `SeekDrive`, `FlightDrive`) execute continuously at 60 FPS inside the browser/physics engine.
- **Discrete pure Haskell logic**: Haskell wakes *only* when something discrete happens—`KeyDown`/`KeyUp` (OS key-repeat suppressed, held keys auto-released on window blur), `MouseDown`/`MouseUp` (with built-in 3D raycast picking `Maybe (BodyId, Vec3)`), `CollisionStart`/`CollisionEnd`, or `TimerFired`. Each event arrives with an immutable `Snapshot BodyId` containing simulation time (`snapTime`) and live physics poses/velocities.

### 2. Type-Safe Wire Interning & Delta Snapshots

Your `PlayerId`, `BodyId`, and `TimerId` ADTs never need `ToJSON`/`FromJSON` instances:

- [Protocol.hs](src/Game/EventHorizon/Protocol.hs) maintains bidirectional intern tables mapping user tags to compact integer wire IDs (with bounded retired-ID retention so in-flight WebSocket messages decode safely without leaking memory).
- Simultaneous physics/timer events from a single step are batched and stamped with `sceneEpoch` so stale events from a previous level/restart are silently discarded.
- On the return path, the browser skips all `Static` and `Sensor` bodies and transmits **only dynamic/kinematic bodies that actually moved**, which Haskell merges into a connection-local snapshot cache.
- In [engine.js](frontend/src/engine.js), repeated Three.js geometries and materials are cached and shared across bodies, and `bodyEvents` defaults to `False` for static geometry so static-vs-static contacts never generate useless events.

### 3. Transparent $O(\Delta \log N)$ Incremental Diffing (`memo` + `SceneTree`)

When a world holds tens of thousands of entities (like a voxel landscape), rebuilding a flat list of bodies and re-diffing all $N$ entries on every click would stall the game.

EventHorizon solves this with a plain 3-constructor `SceneTree BodyId` (`EmptyTree | OneBody | Branch`) paired with a transparent pointer-equality memoizer in [Memo.hs](src/Game/EventHorizon/Memo.hs):

```haskell
memo  :: (a -> b) -> (a -> b)
memo2 :: (a -> b -> c) -> (a -> b -> c)
memo3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
```

1. **2-generation + weak `StableName` cache**: `memo` keys results by the GHC `StableName` of `(f, arg)` in WHNF. The engine flips a 2-generation hot table (`advanceMemoGen`) after every diff (bounding memory cleanly), backed by a `System.Mem.Weak` cache keyed on live inputs so deep untouched subtrees in persistent trees remain hot across non-local mutations without retaining dead data.
2. **Zero-friction container mappers**: `mapTree` and `intMapTree` walk `Data.Map.Internal` / `Data.IntMap.Internal` using `memo2`. When 1 entry changes in a 50,000-entry `Map`, only the $\log_2 N \approx 16$ modified spine nodes re-evaluate; every untouched subtree returns its **exact previous `SceneTree` heap pointer** in $O(1)$.
3. **Pointer-equality `diffTree` + rotation reconciliation**: [Diff.hs](src/Game/EventHorizon/Diff.hs) checks `reallyUnsafePtrEquality#` at every `SceneTree` node, skipping untouched branches in $O(1)$. Any structural mismatches (such as `Data.Map` BST rotations) are reconciled over only the small $O(\Delta)$ mismatched leaves so rotations never trigger spurious body respawns.

#### Measured Performance: With vs. Without Incremental Diffing

**Real-World Stress Test — YoursCraft** (`cabal run yours-craft -- --bench`, alternating block place/break edits across 8×8 chunks `Map ChunkCoord (Map VoxelCoord BlockKind)`, measuring full render + diff + JSON wire encoding per click, GHC 9.6 `-O2`):

| World Size | No Diffing (Full Scene JSON) | Naive Flat Diff (`bodies [...]`) | Memoized Chunked `SceneTree` (`mapTree`) | Speedup (Memo vs. No-Diff / Flat) |
| ---: | ---: | ---: | ---: | ---: |
| **1,706 blocks** (36 chunks) | `140.55 ms` · `720.5 KB` | `3.49 ms` · `696 B` | **`0.143 ms`** · **`696 B`** | **984×** / **24.4×** |
| **10,192 blocks** (144 chunks) | `1,005.31 ms` · `4.2 MB` | `35.28 ms` · `700 B` | **`0.565 ms`** · **`700 B`** | **1,780×** / **62.5×** |
| **44,865 blocks** (576 chunks) | `4,579.31 ms` · `18.6 MB` | `185.16 ms` · `700 B` | **`2.793 ms`** · **`700 B`** | **1,639×** / **66.3×** |

- Without diffing, a single click in a 44,865-block world takes **4.6 seconds** and **18.6 MB** of JSON.
- Server-side flat diffing shrinks the wire payload **27,000×** (to **700 bytes**), but still burns **185 ms** walking all blocks.
- Memoized chunked `SceneTree` diffing drops the full cycle to **2.79 ms** (**0.14 ms** at 1,706 blocks)—well within a single 16.6 ms 60 FPS frame.

**Synthetic Microbenchmark** (`cabal run event-horizon-bench`, pure render + diff latency for 1 non-local mutation per step across 50 steps, GHC 9.6 `-O2`):

| Bodies ($N$) | Naive Flat Diff | `intMapTree` + `memo` | `mapTree` + `memo` |
| ---: | ---: | ---: | ---: |
| **1,000** | `0.274 ms/step` | `0.0064 ms/step` (**42.7×**) | `0.0070 ms/step` (**39.3×**) |
| **10,000** | `4.772 ms/step` | `0.1876 ms/step` (**25.4×**) | `0.0156 ms/step` (**306.0×**) |
| **50,000** | `32.808 ms/step` | `0.1078 ms/step` (**304.4×**) | `0.0258 ms/step` (**1,271.6×**) |

### 4. Deterministic Pure-State + Rapier3D Rollback Multiplayer

Because `world` is a pure immutable Haskell value and Rapier3D supports deterministic binary snapshots, EventHorizon provides smooth client-side prediction without exposing netcode to the game author ([Rollback.hs](src/Game/EventHorizon/Rollback.hs), [MultiEngine.hs](src/Game/EventHorizon/MultiEngine.hs)):

- **Identical deterministic physics**: Both the authoritative server (`serveMulti`) and clients run the exact same `@dimforge/rapier3d-compat` WASM binary at fixed 60 Hz integer ticks (`Tick = Word64`), with integer-tick timers (`TickTimers`) and canonically sorted collision pairs.
- **Zero-copy history ring buffer**: Clients predict local inputs immediately (0 ms local lag) while retaining a 180-tick (3-second) ring buffer of `(hfWorld, hfSnapshot, hfTimers, hfInputs)` alongside Rapier3D's `takeSnapshot()` blobs. Snapshotting `world` is **$O(1)$** (just holding an immutable pointer—no user serialization!).
- **Input-only best-case traffic**: In the nominal case, the server never sends game state—only stamped player inputs (`NetTickInputs`) and lightweight periodic confirmations (`NetConfirm tick stateHash` using a 1 mm quantized FNV-1a snapshot hash).
- **Silent rollback + visual error smoothing**: If a delayed remote input arrives (or a hash mismatches), the client rewinds to the confirmed tick, restores the Rapier3D snapshot, silently fast-forwards to the current tick (re-running physics commands while deduplicating `PlaySound` via `dedupSounds`), and smoothly decays any visual position/rotation delta in Three.js over ~100 ms so objects glide instead of popping.

---

## Example Games

### 1. Sky Ace — 3D Flight & 2-Player Dogfight (`sky-ace`)

Fly through a forested valley, shoot down patrolling wave targets, avoid crashing into trees or terrain, and dogfight a second pilot in multiplayer.

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

- **W / S** or **↑ / ↓**: Climb / Dive · **A / D** or **← / →**: Turn (with auto-banking)
- **Shift**: Boost · **Space / Left Click**: Shoot · **R**: Restart

### 2. Chamber Shift — 3D Physics Puzzle (`chamber-shift`)

Solve **3 handcrafted 3D puzzle chambers** by pushing physics crates onto floor pressure plates, flipping airlock toggles, and racing timed pedestal switches to open sliding kinematic doors.

```bash
cabal run chamber-shift   # open http://localhost:3001
```

- **WASD / Arrow Keys**: Walk & push crates · **E** or **Click nearby pedestal**: Press button
- **R**: Reset level · **1 / 2 / 3**: Jump to Level 1, 2, or 3

### 3. Bastion Break — 3D Siege Demolition (`bastion-break`)

Demolish **3 destructible 3D fortresses** by clicking directly on stone arches, timber towers, and TNT barrels to fire parabolic cannonballs and explosive bombs, triggering radial `ApplyImpulse` shockwaves and chain reactions to topple every Golden Idol.

```bash
cabal run bastion-break                # open http://localhost:3002
cabal run bastion-break -- --step-camera  # optional discrete FixedCamera mode
```

- **Left Click on fortress**: Fire ballistic shot at the clicked 3D point
- **Q / Tab / Right Click**: Switch between Iron Cannonball and Explosive Bomb
- **A / D** or **← / →**: Orbit camera · **C**: Toggle Smooth `CameraPivot` vs. discrete `FixedCamera`
- **Space / Enter**: Advance fortress · **R**: Retry · **1 / 2 / 3**: Select fortress

### 4. YoursCraft — 3D Voxel Builder & Diffing Stress Test (`yours-craft`)

Explore a procedural chunked 3D voxel landscape of rolling hills, stone ridges, and trees, breaking and placing blocks on exact raycast faces—designed to stress-test incremental `SceneTree` + `memo` diffing.

```bash
cabal run yours-craft                  # ~1,700 blocks — open http://localhost:3003
cabal run yours-craft -- --radius 24   # larger world
cabal run yours-craft -- --no-memo     # start in naive flat rebuild mode
cabal run yours-craft -- --bench       # run No-Diff vs Flat-Diff vs Memo-Tree benchmark
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
tutorial/sky-ace/                 7-step incremental Sky Ace tutorial & walkthrough
bench/DiffBench.hs                Large-scene incremental diff benchmark
test/Spec.hs                      Unit, diff, memo, gameplay, and rollback tests
```

---

## Testing, Benchmarks & Frontend Build

```bash
# Run the full test suite
cabal test

# Run the synthetic 1k / 10k / 50k body diff benchmark
cabal run event-horizon-bench

# Run the YoursCraft real-world with-vs-without diffing benchmark
cabal run yours-craft -- --bench

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
