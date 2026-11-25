# Building **Sky Ace** Step-by-Step in EventHorizon

This tutorial builds a complete 3D flight and 2-player dogfighting game from scratch in **7 small, runnable steps**. Each step introduces **one** core EventHorizon concept—from a 55-line plane flying with zero Haskell state, all the way to $O(\Delta \log N)$ memoized scene trees, procedural Web Audio, and 2-player multiplayer with zero networking code.

### How to Follow Along

Every step is a complete, standalone Haskell module in this directory, wired into a single Cabal runner:

```bash
# List all steps
cabal run sky-ace-tutorial

# Run any step (opens on http://localhost:3000; pass --port N to change)
cabal run sky-ace-tutorial -- 1
```

Because each step builds cleanly on the previous file, you can also inspect the exact incremental diff between any two steps:

```bash
diff -u tutorial/sky-ace/Step01_FirstFlight.hs tutorial/sky-ace/Step02_Controls.hs
```

---

## Step 1: First Flight — Zero State, Zero Ticks

**File**: [`Step01_FirstFlight.hs`](Step01_FirstFlight.hs) · **Run**: `cabal run sky-ace-tutorial -- 1`

In traditional game loops, your code must update positions on every 60 Hz frame tick (`dt -> world -> world`). In **EventHorizon**, continuous physics and rendering run inside **Rapier3D + Three.js** while Haskell only runs on discrete events.

To prove it, Step 1 uses `world = ()` (no state at all!), `tid = Void` (no timers), and an `handleEvent` function that ignores everything—yet you immediately get a 60 FPS 3D plane soaring over a green valley:

```haskell
data BodyTag = Player | Ground
  deriving (Eq, Ord, Show)

playerBody :: Body BodyTag
playerBody =
  (dynamicBody Player (at (Vec3 0 18 52)) (BoxCollider (Vec3 3.0 0.9 2.6)) airplaneMesh)
    { bodyGravity = 0.0
    , bodyDrive   = FlightDrive { flightSpeed = 25.0, flightPitch = 0, flightYaw = 0, flightRoll = 0, flightAutoLevel = True }
    }
```

**Why it works:**
- **Type-safe `bid`**: `BodyTag` is an ordinary Haskell ADT (`deriving (Eq, Ord, Show)`). EventHorizon transparently interns it to compact integer wire IDs—no strings, no `ToJSON`/`FromJSON` boilerplate.
- **Declarative `Drive` + `FollowCamera`**: Setting `FlightDrive` on `Player` tells Rapier3D how to propel the aircraft at 60 Hz, and `FollowCamera { camTarget = Player, camLocal = True }` smoothly tracks it in Three.js.
- **Mesh Monoid**: `airplaneMesh` composes boxes, cones, and transforms (`translate`, `rotate`, `colored`) purely using `group`.

---

## Step 2: Event-Driven Flight Controls

**File**: [`Step02_Controls.hs`](Step02_Controls.hs) · **Run**: `cabal run sky-ace-tutorial -- 2`

Now let's steer the plane with **W/S** (climb/dive), **A/D** (turn with auto-banking), and **Shift** (boost).

We replace `()` with a tiny state holding the currently pressed keys:

```haskell
newtype GameState = GameState { gsHeldKeys :: Set Key }

handleEvent :: Event BodyTag Void -> Snapshot BodyTag -> GameState -> GameState
handleEvent ev _ gs = case ev of
  KeyDown k -> gs { gsHeldKeys = Set.insert k (gsHeldKeys gs) }
  KeyUp   k -> gs { gsHeldKeys = Set.delete k (gsHeldKeys gs) }
  _         -> gs
```

And derive the `FlightDrive` purely from `gsHeldKeys gs` inside `render`:

```haskell
computeDrive :: Set Key -> Drive
computeDrive keys =
  let held k = Set.member k keys
      pitch = (if held (KeyChar 'w') then 1.3 else 0) + (if held (KeyChar 's') then -1.3 else 0)
      yaw   = (if held (KeyChar 'a') then 1.5 else 0) + (if held (KeyChar 'd') then -1.5 else 0)
      speed = if held KeyShift then 39.0 else 25.0
   in FlightDrive speed pitch yaw 0.0 True
```

**Why it works:**
- Look at the status badge in the bottom-left corner of the browser: the **Events** counter increments *only* when you press or release a key. While you hold `A` to bank in a smooth circle for 10 seconds, Haskell executes **zero times**.
- The browser runtime automatically suppresses OS key-repeat and releases held keys on window blur so controls never stick.

---

## Step 3: Forest, Type-Safe Collisions, Crash & Restart

**File**: [`Step03_ForestAndCrash.hs`](Step03_ForestAndCrash.hs) · **Run**: `cabal run sky-ace-tutorial -- 3`

Let's add a valley full of trees, crash detection, a game-over banner, and instant restart (`R`).

1. **Extend `BodyTag`**: Add `Tree !Int` so every tree has a distinct, type-safe ID.
2. **Build the static world once as a top-level CAF**:
   ```haskell
   staticWorldTree :: SceneTree BodyTag
   staticWorldTree = bodies (groundBody : map treeBody forestTrees)
   ```
   Because `staticWorldTree` is a shared heap pointer across every `render`, EventHorizon's `diffTree` skips the entire forest in **$O(1)$** on every keypress—sending only a ~120-byte delta updating `Player`'s `Drive`!
3. **Pattern-match unordered collisions cleanly with `matchPair`**:
   ```haskell
   CollisionStart a b
     | Just ((), obs) <- matchPair (only Player) asHazard a b
     , gsStatus gs == Flying ->
         gs { gsStatus = Crashed (reasonFor obs) }
   ```
   When crashed, `playerBody` switches to `bodyGravity = 1.0, bodyDrive = NoDrive` so the plane tumbles realistically under gravity!
4. **Reset the physics world with `sceneEpoch`**:
   Pressing `R` returns `initialState { gsEpoch = gsEpoch gs + 1 }`. Whenever `sceneEpoch` changes, EventHorizon emits a `PatchReset` that tears down and cleanly respawns the entire Rapier3D physics world.

---

## Step 4: Shooting, Live Snapshots, Timers & Collision Filters

**File**: [`Step04_ShootingAndTimers.hs`](Step04_ShootingAndTimers.hs) · **Run**: `cabal run sky-ace-tutorial -- 4`

How do you fire a bullet from the nose of a plane whose position lives inside Rapier3D, not inside `GameState`?

That's what **`Snapshot bid`** is for! Whenever an event wakes Haskell, the runtime passes the live positions, rotations, and velocities of dynamic bodies:

```haskell
fireBullet :: Snapshot BodyTag -> GameState -> GameState
fireBullet snap gs =
  case lookupBody Player snap of
    Nothing -> gs
    Just st ->
      let nid      = gsNextId gs
          fwd      = forwardOf (bsRot st)
          spawnPos = bsPos st + 3.4 *^ fwd   -- Vec3 (+) and left scalar (*^)
          vel      = 125.0 *^ fwd
       in gs { gsBullets = IM.insert nid (BulletInfo nid (atRot spawnPos (bsRot st)) vel) (gsBullets gs)
             , gsNextId  = nid + 1
             }
```

Three more EventHorizon essentials introduced in Step 4:
1. **Type-safe `TimerTag` (`Once`)**: We replace `Void` with `data TimerTag = BulletExpire !Int` and declare `[Once (BulletExpire n) 2.2 | ...]` in `sceneTimers`. Removing a bullet removes its timer automatically; if the bullet hasn't hit anything after 2.2 seconds, `TimerFired (BulletExpire n)` wakes Haskell to despawn it.
2. **`CollisionFilter` + `bodyCCD`**: Fast bullets set `bodyCCD = True` (continuous collision detection so they never tunnel through thin tree trunks) and use 16-bit collision masks (`grpPlayer` vs `grpBullet`) so bullets never accidentally clip the player's own nose.
3. **Incremental `intMapTree`**: Bullets are stored in an `IntMap BulletInfo` and rendered with `intMapTree bulletNode (gsBullets gs)`, which walks the Patricia trie using `memo2`.

---

## Step 5: Patrolling AI Targets, Waves & Incremental `SceneTree`

**File**: [`Step05_TargetsAndMemo.hs`](Step05_TargetsAndMemo.hs) · **Run**: `cabal run sky-ace-tutorial -- 5`

Now we add waves of patrolling flying targets, scoring, and short-lived explosion flashes:

- **Periodic AI steering (`Every`)**: Instead of steering AI on every 60 Hz frame, we declare `Every AiManeuver 2.5` in `sceneTimers`. Every 2.5 seconds, Haskell inspects each target's live pose in `Snapshot` and uses vector subtraction and dot products (`right <.> toCenter`, `fwd <.> Vec3 0 1 0`) to gently bank wandering targets back over the valley.
- **Exhaustive timer handling**: Because `TimerTag` is an ADT (`AiManeuver | NextWave | BulletExpire !Int | FxExpire !Int`), GHC verifies every timer case is handled.
- **Memoized `SceneTree` composition**:
  ```haskell
  sceneTree =
    staticWorldTree
      <> bodyNode (playerBody gs)
      <> mapTree targetNode (gsTargets gs)
      <> intMapTree bulletNode (gsBullets gs)
      <> intMapTree fxNode (gsFx gs)
  ```
  Notice `targetNode`, `bulletNode`, and `fxNode` are top-level functions (constant closure pointers), so `mapTree` and `intMapTree` hit the 2-generation + weak `StableName` memo cache on every untouched subtree—scaling cleanly to tens of thousands of bodies in $O(\Delta \log N)$ time.

---

## Step 6: One-Shot Commands, Web Audio & Visual Polish

**File**: [`Step06_CommandsAndAudio.hs`](Step06_CommandsAndAudio.hs) · **Run**: `cabal run sky-ace-tutorial -- 6`

Declarative `Scene` describes *what exists right now*, which is idempotent—calling `render gs` twice should never play a laser sound twice! For **one-shot effects** (sound effects, physics impulses, teleports), upgrade `play` to **`playWith`**:

```haskell
stepGame :: Event BodyTag TimerTag -> Snapshot BodyTag -> GameState -> (GameState, [Command BodyTag])
```

Now `fireBullet` and `handleCollision` emit zero-asset procedural Web Audio commands right alongside the new state:

```haskell
PlaySound (sweep WaveSawtooth 680 180 0.14 0.16)  -- laser pew
PlaySound (noise 0.28 0.30)                        -- explosion burst
PlaySound (sweep WaveSine 440 880 0.35 0.22)       -- wave cleared chime
```

We also add a `HemisphereLight` for natural sky/ground ambient bounce and a custom `Material` (`matCastShadow = False`) on glowing bullets and explosions.

---

## Step 7: 2-Player Multiplayer (With Zero Networking Code)

**File**: [`Step07_Multiplayer.hs`](Step07_Multiplayer.hs) · **Run**: `cabal run sky-ace-tutorial -- 7`

*(Requires Node.js for the headless Rapier3D physics sidecar. Open **http://localhost:3000/?player=1** and **http://localhost:3000/?player=2** in two browser tabs!)*

Compare `Step06_CommandsAndAudio.hs` with `Step07_Multiplayer.hs`: **not a single line of sockets, serialization, prediction, or rollback code enters your game logic.**

Only four small game-level changes turn Sky Ace into a 2-player dogfight:

1. **Add a player ID type (`pid`)**: `data Pilot = Pilot1 | Pilot2 deriving (Eq, Ord, Show, Enum, Bounded)`.
2. **Store per-pilot state in a `Map`**: Replace single `gsStatus`/`gsScore`/`gsHeldKeys` with `gsPilots :: Map Pilot PilotState`.
3. **Split `render` into `renderWorld` + `renderView`**:
   - `renderWorld :: GameState -> WorldScene BodyTag TimerTag` describes the shared 3D sky, forest, jets, targets, and bullets once.
   - `renderView :: Pilot -> GameState -> PlayerView BodyTag` gives each pilot their own `FollowCamera` and score HUD.
4. **Add PvP dogfighting & respawn `Teleport`**: Shooting the rival pilot awards +250 points and schedules `Once (PilotRespawn victim) 3.0`; when it fires, `stepMulti` emits `WorldCmd (Teleport (pilotBody p) (pilotSpawnPose p))` and `PlayerCmd p (PlaySound ...)` so only the respawning pilot hears their respawn chime.

Package those four functions into `Game Pilot BodyTag TimerTag GameState` and pass it to `playLocalMulti` (or `serveMulti` + `connectMulti` for full client-predicted rollback networking—see [`examples/sky-ace/Main.hs`](../../examples/sky-ace/Main.hs)).

---

## Where to Go Next

- Explore **[Chamber Shift](../../examples/chamber-shift/ChamberShift.hs)** to see trigger `Sensor` plates, `Kinematic` sliding doors, and `WalkDrive`.
- Explore **[Bastion Break](../../examples/bastion-break/BastionBreak.hs)** to see 3D mouse raycasting (`MouseDown`), radial `ApplyImpulse` shockwaves, `CompoundCollider`, and the `CameraPivot` smooth-orbit trick.
- Run `cabal run event-horizon-bench` to benchmark `mapTree` / `intMapTree` + `memo` up to 50,000 bodies.
