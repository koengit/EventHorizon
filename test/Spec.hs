{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (foldM, unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified SkyAce
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Game.EventHorizon
import Game.EventHorizon.Diff
import Game.EventHorizon.Memo (advanceMemoGen, clearMemo)
import Game.EventHorizon.Protocol hiding (wsBodies, wsEpoch, wsTimers)
import qualified Game.EventHorizon.Protocol as Proto
import Game.EventHorizon.Rollback
import qualified BastionBreak
import qualified ChamberShift
import qualified YoursCraft
import System.Exit (exitFailure)

samePtr :: a -> a -> Bool
samePtr a b = isTrue# (reallyUnsafePtrEquality# a b)

assertBool :: String -> Bool -> IO ()
assertBool msg cond =
  unless cond $ do
    putStrLn ("FAIL: " <> msg)
    exitFailure

assertApprox :: String -> Double -> Double -> IO ()
assertApprox msg expected actual =
  assertBool
    (msg <> " (expected " <> show expected <> ", got " <> show actual <> ")")
    (abs (expected - actual) < 1e-6)

findBody :: Eq bid => bid -> Scene bid tid -> Maybe (Body bid)
findBody bid sc =
  case filter (\b -> bodyId b == bid) (allSceneBodies sc) of
    (b : _) -> Just b
    [] -> Nothing

testMathOperators :: IO ()
testMathOperators = do
  let a = Vec3 1 2 3
      b = Vec3 4 5 6
  assertBool "Num (+) on Vec3" (a + b == Vec3 5 7 9)
  assertBool "(^+^) on Vec3" (a ^+^ b == Vec3 5 7 9)
  assertBool "(^-^) on Vec3" (b ^-^ a == Vec3 3 3 3)
  assertBool "(*^) left scalar" (2 *^ a == Vec3 2 4 6)
  assertBool "(^*) right scalar" (a ^* 2 == Vec3 2 4 6)
  assertBool "(^/) scalar division" (Vec3 2 4 6 ^/ 2 == a)
  assertApprox "(<.>) dot product" 32 (a <.> b)
  assertBool "(><) cross product" (Vec3 1 0 0 >< Vec3 0 1 0 == Vec3 0 0 1)
  assertBool "negate on Vec3" (-a == Vec3 (-1) (-2) (-3))
  assertBool "vLerp midpoint" (vLerp 0.5 (Vec3 0 0 0) (Vec3 2 4 6) == Vec3 1 2 3)

  -- Quaternion Semigroup/Monoid & (#>) rotation operator
  let Vec3 fx fy fz = forwardOf mempty
  assertApprox "forwardOf mempty x" 0 fx
  assertApprox "forwardOf mempty y" 0 fy
  assertApprox "forwardOf mempty z" (-1) fz

  let qYaw = quatFromEuler (Vec3 0 (pi / 2) 0)
      Vec3 rx ry rz = qYaw #> Vec3 0 0 (-1)
  assertApprox "yaw +90 (#>) x" (-1) rx
  assertApprox "yaw +90 (#>) y" 0 ry
  assertApprox "yaw +90 (#>) z" 0 rz

  -- Inverse quaternion cancels rotation
  let vRot = (qYaw <> quatInv qYaw) #> Vec3 3 4 5
      Vec3 ix iy iz = vRot
  assertApprox "q <> quatInv q x" 3 ix
  assertApprox "q <> quatInv q y" 4 iy
  assertApprox "q <> quatInv q z" 5 iz

  -- Mesh Monoid flattens EmptyMesh
  assertBool "Mesh Monoid identity" (mempty <> cube 1 == cube 1)
  assertBool "Mesh Semigroup combines" (cube 1 <> sphere 2 == GroupMesh [cube 1, sphere 2])

testDiffAndProtocol :: IO ()
testDiffAndProtocol = do
  -- Initial scene produces PatchReset; identical scene produces empty PatchDelta
  let g0 = SkyAce.initialGame
      sc0 = SkyAce.renderGame g0
      (p0, ds0) = diffScene Nothing sc0
  case p0 of
    PatchReset _ -> pure ()
    PatchDelta _ -> assertBool "initial diff must be PatchReset" False

  let (pSame, _) = diffScene (Just ds0) sc0
  case pSame of
    PatchDelta d -> assertBool "identical scene produces empty delta" (isEmptyDelta d)
    PatchReset _ -> assertBool "identical scene should not reset" False

  -- Pressing 'a' updates ONLY the player's Drive (trees are O(1) skipped!)
  let gTurn = SkyAce.handleEvent (KeyDown (KeyChar 'a')) emptySnapshot g0
      scTurn = SkyAce.renderGame gTurn
      (pTurn, dsTurn) = diffScene (Just ds0) scTurn
  case pTurn of
    PatchDelta d -> do
      assertBool "turn delta removes 0 bodies" (null (sdRemoved d))
      assertBool "turn delta adds 0 bodies" (null (sdAdded d))
      assertBool "turn delta updates exactly 1 body (Player)" (length (sdUpdated d) == 1)
      let [upd] = sdUpdated d
      assertBool "updated body is Player" (buId upd == SkyAce.Player)
      assertBool "mesh was NOT resent when only Drive changed" (buMesh upd == Nothing)
      assertBool "drive WAS updated" (buDrive upd /= Nothing)
      assertBool "lights unchanged" (sdLights d == Nothing)
      assertBool "hud unchanged" (sdHud d == Nothing)
    PatchReset _ -> assertBool "turning should not reset scene" False

  -- Wire encoding, interning, delta patch size, and batched decoding
  let (resetBytes, ws1) = encodePatch p0 [] emptyWireState
      (deltaBytes, ws2) =
        encodePatch
          pTurn
          [ApplyImpulse SkyAce.Player (Vec3 0 5 0), PlaySound (noise 0.2 0.2)]
          ws1
  assertBool "reset JSON is valid" (Aeson.decode resetBytes /= (Nothing :: Maybe Aeson.Value))
  assertBool "delta JSON is valid" (Aeson.decode deltaBytes /= (Nothing :: Maybe Aeson.Value))
  assertBool
    "delta patch is dramatically smaller than full scene reset"
    (LBS.length deltaBytes * 10 < LBS.length resetBytes)

  -- Verify Player wire ID round-trips through ClientBatch + SnapshotDelta
  let Just playerWireId = Map.lookup SkyAce.Player (idToWire (Proto.wsBodies ws2))
      Just tree1WireId = Map.lookup (SkyAce.Tree 1) (idToWire (Proto.wsBodies ws2))
      rawBatch =
        LBS.pack $
          "{\"epoch\":0,\"time\":4.5,\"events\":[{\"type\":\"collisionStart\",\"a\":"
            <> show playerWireId
            <> ",\"b\":"
            <> show tree1WireId
            <> "}],\"snapshotDelta\":{\""
            <> show playerWireId
            <> "\":{\"pos\":[1,2,3],\"rot\":[0,0,0,1],\"vel\":[0,0,-20],\"angVel\":[0,0,0]}}}"
  case decodeClientBatch rawBatch of
    Left err -> assertBool ("decodeClientBatch failed: " <> err) False
    Right batch ->
      case resolveClientBatch batch ws2 of
        Nothing -> assertBool "resolveClientBatch should accept matching epoch" False
        Just (evs, snap, _) -> do
          assertBool "decoded typed CollisionStart" (evs == [CollisionStart SkyAce.Player (SkyAce.Tree 1)])
          assertApprox "snapshot time propagated" 4.5 (snapTime snap)
          case lookupBody SkyAce.Player snap of
            Nothing -> assertBool "snapshot contains Player" False
            Just bs -> assertBool "snapshot Player pos updated from delta" (bsPos bs == Vec3 1 2 3)
          -- Static bodies seeded in server snapshot cache are still queryable!
          assertBool "static Ground remains present in merged snapshot" (lookupBody SkyAce.Ground snap /= Nothing)

  -- Stale-epoch batches are safely discarded
  let staleBatch =
        LBS.pack "{\"epoch\":999,\"time\":5.0,\"events\":[{\"type\":\"keydown\",\"key\":\"KeyW\"}],\"snapshotDelta\":{}}"
  case decodeClientBatch staleBatch of
    Left err -> assertBool ("decode staleBatch failed: " <> err) False
    Right b ->
      assertBool "stale epoch batch must be discarded" (resolveClientBatch b ws2 == Nothing)

  -- Restart bumps epoch -> diffScene emits PatchReset
  let gRestart = SkyAce.handleEvent (KeyDown (KeyChar 'r')) emptySnapshot gTurn
      (pReset, _) = diffScene (Just dsTurn) (SkyAce.renderGame gRestart)
  case pReset of
    PatchReset _ -> pure ()
    PatchDelta _ -> assertBool "epoch change must emit PatchReset" False

  -- Key parsing
  assertBool "parseKey KeyW" (parseKey "KeyW" == KeyChar 'w')
  assertBool "parseKey Digit3" (parseKey "Digit3" == KeyChar '3')
  assertBool "parseKey Space" (parseKey "Space" == KeySpace)
  assertBool "parseKey ArrowUp" (parseKey "ArrowUp" == KeyUpArrow)

testSkyAce :: IO ()
testSkyAce = do
  let g0 = SkyAce.initialGame
      sc0 = SkyAce.renderGame g0
  assertBool "fly game spawns forest trees" (length SkyAce.forestTrees >= 20)
  assertBool "fly game includes forest in sceneTree" (length (treeBodies SkyAce.staticWorldTree) >= 21)
  assertBool "fly game spawns wave 1 targets" (Map.size (SkyAce.gsTargets g0) == 6)
  assertBool "scene has Player body" (findBody SkyAce.Player sc0 /= Nothing)

  -- Input updates FlightDrive purely
  let gTurn = SkyAce.handleEvent (KeyDown (KeyChar 'a')) emptySnapshot g0
      gBoost = SkyAce.handleEvent (KeyDown KeyShift) emptySnapshot gTurn
  case findBody SkyAce.Player (SkyAce.renderGame gBoost) of
    Just b -> case bodyDrive b of
      FlightDrive spd _ yaw _ _ -> do
        assertBool "boost increases flight speed" (spd > 30)
        assertBool "A turns left (positive yaw)" (yaw > 0)
      other -> assertBool ("expected FlightDrive, got " <> show other) False
    Nothing -> assertBool "Player missing" False

  -- Shooting spawns bullet ahead of player using Snapshot and emits laser sound
  let playerSnap =
        Snapshot 1.0 $
          Map.singleton
            SkyAce.Player
            (BodyState (Vec3 10 20 30) mempty (Vec3 0 0 (-25)) vZero)
      (gShot, shotCmds) = SkyAce.stepGame (KeyDown KeySpace) playerSnap g0
  assertBool "shooting spawns 1 bullet" (IM.size (SkyAce.gsBullets gShot) == 1)
  assertBool "shooting emits audio command" (not (null shotCmds))
  let [bullet] = IM.elems (SkyAce.gsBullets gShot)
      Vec3 bx by bz = posePos (SkyAce.bulletSpawnPose bullet)
  assertApprox "bullet spawns in front of nose (x)" 10 bx
  assertApprox "bullet spawns in front of nose (y)" 20 by
  assertBool "bullet spawns ahead (-z)" (bz < 30)

  -- Bullet hits target -> destroys both, +100 score, spawns FX
  let ((w, idx) : _) = Map.keys (SkyAce.gsTargets gShot)
      targetBid = SkyAce.Target w idx
      hitSnap =
        Snapshot 1.5 $
          Map.singleton
            targetBid
            (BodyState (Vec3 0 25 (-10)) mempty vZero vZero)
      gHit =
        SkyAce.handleEvent
          (CollisionStart (SkyAce.Bullet (SkyAce.bulletNum bullet)) targetBid)
          hitSnap
          gShot
  assertBool "hit removes bullet" (IM.null (SkyAce.gsBullets gHit))
  assertBool "hit removes target" (not (Map.member (w, idx) (SkyAce.gsTargets gHit)))
  assertBool "hit awards 100 score" (SkyAce.gsScore gHit == 100)
  assertBool "hit spawns explosion fx" (IM.size (SkyAce.gsFx gHit) == 1)

  -- Clearing remaining targets triggers NextWave timer
  let clearAll g =
        case Map.keys (SkyAce.gsTargets g) of
          [] -> g
          ((tw, ti) : _) ->
            let g1 = SkyAce.handleEvent (KeyDown KeySpace) playerSnap g
                [b1] = IM.elems (SkyAce.gsBullets g1)
                g2 =
                  SkyAce.handleEvent
                    (CollisionStart (SkyAce.Bullet (SkyAce.bulletNum b1)) (SkyAce.Target tw ti))
                    hitSnap
                    g1
             in clearAll g2
      gCleared = clearAll gHit
  assertBool "clearing wave sets gsWavePending" (SkyAce.gsWavePending gCleared)
  let gWave2 = SkyAce.handleEvent (TimerFired SkyAce.NextWave) emptySnapshot gCleared
  assertBool "NextWave increments wave to 2" (SkyAce.gsWave gWave2 == 2)
  assertBool "wave 2 spawns more targets" (Map.size (SkyAce.gsTargets gWave2) == 7)

  -- Crashing into a tree triggers Crashed state and R resets cleanly
  let gCrash = SkyAce.handleEvent (CollisionStart SkyAce.Player (SkyAce.Tree 1)) playerSnap g0
  case SkyAce.gsStatus gCrash of
    SkyAce.Crashed _ -> pure ()
    SkyAce.Flying -> assertBool "hitting tree should crash player" False
  let gReset = SkyAce.handleEvent (KeyDown (KeyChar 'r')) emptySnapshot gCrash
  assertBool "restart resets to Flying" (SkyAce.gsStatus gReset == SkyAce.Flying)
  assertBool "restart bumps sceneEpoch" (SkyAce.gsEpoch gReset == SkyAce.gsEpoch gCrash + 1)

testChamberShift :: IO ()
testChamberShift = do
  assertBool "at least 3 puzzle levels exist" (length ChamberShift.levels >= 3)

  -- === LEVEL 1 ===
  let p0 = ChamberShift.initialPuzzle
      lvl1 = head ChamberShift.levels
      doorById lvl did = head (filter (\d -> ChamberShift.doorId d == did) (ChamberShift.lvlDoors lvl))
      d1 = doorById lvl1 ChamberShift.Door1
      d2 = doorById lvl1 ChamberShift.Door2

  assertBool "L1 Door1 starts closed" (not (ChamberShift.isDoorOpen p0 d1))
  assertBool "L1 Door2 starts closed" (not (ChamberShift.isDoorOpen p0 d2))

  -- Pressing E away from button does nothing
  let pAway = ChamberShift.handleEvent (KeyDown (KeyChar 'e')) emptySnapshot p0
  assertBool "L1 Door1 stays closed when pressing E away from button" (not (ChamberShift.isDoorOpen pAway d1))

  -- Entering button zone and pressing E toggles Door1 open
  let pNear = ChamberShift.handleEvent (CollisionStart (ChamberShift.ButtonZone ChamberShift.BtnGate) ChamberShift.Player) emptySnapshot p0
      pOpen1 = ChamberShift.handleEvent (KeyDown (KeyChar 'e')) emptySnapshot pNear
  assertBool "L1 Door1 opens after pressing BtnGate" (ChamberShift.isDoorOpen pOpen1 d1)

  -- Clicking nearby button with mouse also toggles it
  let pClickClose =
        ChamberShift.handleEvent
          (MouseDown MouseLeft (Just (ChamberShift.Button ChamberShift.BtnGate, vZero)))
          emptySnapshot
          pOpen1
  assertBool "L1 Door1 closes on mouse click while nearby" (not (ChamberShift.isDoorOpen pClickClose d1))

  -- Overlapping player + crate on Plate1 keeps Door2 open when player steps off
  let pPlateBoth =
        ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.Plate1) (ChamberShift.Crate 1)) emptySnapshot $
          ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.Plate1) ChamberShift.Player) emptySnapshot pOpen1
      pCrateOnly = ChamberShift.handleEvent (CollisionEnd (ChamberShift.Plate ChamberShift.Plate1) ChamberShift.Player) emptySnapshot pPlateBoth
  assertBool "L1 Door2 stays open with crate on Plate1" (ChamberShift.isDoorOpen pCrateOnly d2)
  let pCrateOff = ChamberShift.handleEvent (CollisionEnd (ChamberShift.Plate ChamberShift.Plate1) (ChamberShift.Crate 1)) emptySnapshot pCrateOnly
  assertBool "L1 Door2 closes when crate leaves Plate1" (not (ChamberShift.isDoorOpen pCrateOff d2))

  -- Reaching exit transitions to Level 2 and bumps epoch
  let pLvl2 = ChamberShift.handleEvent (CollisionStart ChamberShift.Player ChamberShift.Exit) emptySnapshot pCrateOnly
  assertBool "advances to level index 1 (Level 2)" (ChamberShift.psLevelIndex pLvl2 == 1)
  assertBool "advancing level bumps epoch" (ChamberShift.psEpoch pLvl2 == ChamberShift.psEpoch pCrateOnly + 1)

  -- === LEVEL 2 ===
  let lvl2 = ChamberShift.levels !! 1
      dVault = doorById lvl2 ChamberShift.DoorVault
      dExit2 = doorById lvl2 ChamberShift.DoorExit

  let pNearVault =
        ChamberShift.handleEvent
          (CollisionStart ChamberShift.Player (ChamberShift.ButtonZone ChamberShift.BtnVaultOut))
          emptySnapshot
          pLvl2
      pVaultOpen = ChamberShift.handleEvent (KeyDown (KeyChar 'e')) emptySnapshot pNearVault
  assertBool "L2 DoorVault opens on timed button press" (ChamberShift.isDoorOpen pVaultOpen dVault)
  let scVault = ChamberShift.renderPuzzle pVaultOpen
  assertBool "L2 schedules a Once timer for timed button" (not (null (sceneTimers scVault)))
  let (Once tid _ : _) = sceneTimers scVault
      pVaultExpired = ChamberShift.handleEvent (TimerFired tid) emptySnapshot pVaultOpen
  assertBool "L2 DoorVault closes when timer fires" (not (ChamberShift.isDoorOpen pVaultExpired dVault))

  -- Dual-plate AND logic for exit door
  let pOnePlate = ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.PlateA) (ChamberShift.Crate 1)) emptySnapshot pVaultExpired
  assertBool "L2 DoorExit stays closed with only PlateA" (not (ChamberShift.isDoorOpen pOnePlate dExit2))
  let pTwoPlates = ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.PlateB) (ChamberShift.Crate 2)) emptySnapshot pOnePlate
  assertBool "L2 DoorExit opens with both PlateA and PlateB" (ChamberShift.isDoorOpen pTwoPlates dExit2)

  -- Advance to Level 3
  let pLvl3 = ChamberShift.handleEvent (CollisionStart ChamberShift.Player ChamberShift.Exit) emptySnapshot pTwoPlates
  assertBool "advances to level index 2 (Level 3)" (ChamberShift.psLevelIndex pLvl3 == 2)

  -- === LEVEL 3 ===
  let lvl3 = ChamberShift.levels !! 2
      dWest = doorById lvl3 ChamberShift.DoorWest
      dEast = doorById lvl3 ChamberShift.DoorEast
      dMid = doorById lvl3 ChamberShift.DoorMid
      dFinal = doorById lvl3 ChamberShift.DoorFinal

  assertBool "L3 DoorWest starts OPEN" (ChamberShift.isDoorOpen pLvl3 dWest)
  assertBool "L3 DoorEast starts CLOSED" (not (ChamberShift.isDoorOpen pLvl3 dEast))
  assertBool "L3 DoorMid starts CLOSED" (not (ChamberShift.isDoorOpen pLvl3 dMid))

  let pAirlockFlip =
        ChamberShift.handleEvent (KeyDown (KeyChar 'e')) emptySnapshot $
          ChamberShift.handleEvent (CollisionStart (ChamberShift.ButtonZone ChamberShift.BtnAirlock) ChamberShift.Player) emptySnapshot pLvl3
  assertBool "L3 DoorWest closes on airlock flip" (not (ChamberShift.isDoorOpen pAirlockFlip dWest))
  assertBool "L3 DoorEast opens on airlock flip" (ChamberShift.isDoorOpen pAirlockFlip dEast)

  let pBridge = ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.PlateBridge) (ChamberShift.Crate 1)) emptySnapshot pAirlockFlip
  assertBool "L3 DoorMid opens on PlateBridge" (ChamberShift.isDoorOpen pBridge dMid)

  let pFinalReady =
        ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.PlateExit2) (ChamberShift.Crate 2)) emptySnapshot $
          ChamberShift.handleEvent (CollisionStart (ChamberShift.Plate ChamberShift.PlateExit1) (ChamberShift.Crate 1)) emptySnapshot pBridge
  assertBool "L3 DoorFinal opens with both final plates" (ChamberShift.isDoorOpen pFinalReady dFinal)
  let pWon = ChamberShift.handleEvent (CollisionStart ChamberShift.Player ChamberShift.Exit) emptySnapshot pFinalReady
  assertBool "L3 exit completes the game" (ChamberShift.psWon pWon)

testBastionBreak :: IO ()
testBastionBreak = do
  assertBool "3 siege fortresses exist" (length BastionBreak.fortresses == 3)

  -- Ballistic velocity lands exactly on target point under gravity -24
  let start = Vec3 0 9.9 22.2
      target = Vec3 2.5 5.0 (-1.0)
      vel@(Vec3 vx vy vz) = BastionBreak.computeBallisticVel start target 42.0
      Vec3 dx _ dz = target - start
      horiz = sqrt (dx * dx + dz * dz)
      t = max 0.25 (min 1.35 (horiz / 42.0))
      landed = start + t *^ vel + Vec3 0 (0.5 * (-24.0) * t * t) 0
      Vec3 lx ly lz = landed
      Vec3 tx ty tz = target
  assertApprox "ballistic impact x" tx lx
  assertApprox "ballistic impact y" ty ly
  assertApprox "ballistic impact z" tz lz
  assertBool "ballistic arc launches upward/forward" (vy > -5 && vz < 0 && vx > 0)

  -- Clicking on TNT barrel spawns an Iron shot and decrements ammo
  let s0 = BastionBreak.initialSiege
      sc0 = BastionBreak.renderSiege s0
  assertBool "island is present in sceneTree" (findBody BastionBreak.Island sc0 /= Nothing)
  let snap1 =
        Snapshot 1.0 $
          Map.fromList
            [ (BastionBreak.Tnt 1, BodyState (Vec3 0 0.75 0) mempty vZero vZero)
            , (BastionBreak.Block 1, BodyState (Vec3 (-3.2) 2.0 0) mempty vZero vZero)
            , (BastionBreak.Idol 1, BodyState (Vec3 0 5.85 0) mempty vZero vZero)
            , (BastionBreak.Idol 2, BodyState (Vec3 0 9.85 0) mempty vZero vZero)
            ]
      (sShot, shotCmds) =
        BastionBreak.stepSiege
          (MouseDown MouseLeft (Just (BastionBreak.Tnt 1, Vec3 0 0.75 0)))
          snap1
          s0
  assertBool "firing consumes 1 shot" (BastionBreak.ssShotsLeft sShot == BastionBreak.ssShotsLeft s0 - 1)
  assertBool "firing spawns 1 shot body" (Map.size (BastionBreak.ssShots sShot) == 1)
  assertBool "firing emits cannon audio" (not (null shotCmds))

  -- Shot colliding with TNT detonates it and emits radial ApplyImpulse commands on nearby blocks
  let [sh] = Map.elems (BastionBreak.ssShots sShot)
      (sBlast, blastCmds) =
        BastionBreak.stepSiege
          (CollisionStart (BastionBreak.Shot (BastionBreak.shotNum sh)) (BastionBreak.Tnt 1))
          snap1
          sShot
  assertBool "TNT 1 removed after detonation" (not (elem 1 (BastionBreak.ssActiveTnt sBlast)))
  assertBool
    "TNT blast emits ApplyImpulse on Block 1"
    (any (\c -> case c of ApplyImpulse (BastionBreak.Block 1) _ -> True; _ -> False) blastCmds)
  assertBool "TNT blast spawns visual blast ring" (Map.size (BastionBreak.ssFx sBlast) == 1)

  -- Switching to Bomb ammo and detonating on a stone block
  let sBombReady = BastionBreak.handleEvent (KeyDown (KeyChar 'q')) snap1 sBlast
  assertBool "Q switches to AmmoBomb" (BastionBreak.ssAmmoKind sBombReady == BastionBreak.AmmoBomb)
  let (sBombFired, _) =
        BastionBreak.stepSiege
          (MouseDown MouseLeft (Just (BastionBreak.Block 1, Vec3 (-3.2) 2.0 0)))
          (snap1 {snapTime = 2.0})
          sBombReady
  assertBool "firing bomb consumes 1 bomb" (BastionBreak.ssBombsLeft sBombFired == BastionBreak.ssBombsLeft sBombReady - 1)

  -- Dropping both idols into SeaSensor clears Fortress 1 and Enter advances to Fortress 2
  let sIdol1 = BastionBreak.handleEvent (CollisionStart (BastionBreak.Idol 1) BastionBreak.SeaSensor) snap1 s0
      sCleared = BastionBreak.handleEvent (CollisionStart (BastionBreak.Idol 2) BastionBreak.SeaSensor) snap1 sIdol1
  assertBool "clearing all idols marks FortressCleared" (BastionBreak.ssStatus sCleared == BastionBreak.FortressCleared)
  let sFort2 = BastionBreak.handleEvent (KeyDown KeyEnter) snap1 sCleared
  assertBool "Enter advances to Fortress 2" (BastionBreak.ssFortIndex sFort2 == 1)
  assertBool "advancing fortress bumps epoch" (BastionBreak.ssEpoch sFort2 == BastionBreak.ssEpoch sCleared + 1)

  -- CameraMode: SmoothOrbit uses CameraPivot VelocityDrive + FollowCamera; C toggles to StepOrbit FixedCamera
  let sOrbiting = BastionBreak.handleEvent (KeyDown (KeyChar 'd')) snap1 s0
  case findBody BastionBreak.CameraPivot (BastionBreak.renderSiege sOrbiting) of
    Just b -> case bodyDrive b of
      VelocityDrive _ (Vec3 _ yaw _) -> assertBool "SmoothOrbit rotates CameraPivot on D" (yaw > 0)
      other -> assertBool ("expected VelocityDrive on CameraPivot, got " <> show other) False
    Nothing -> assertBool "SmoothOrbit should spawn CameraPivot" False
  let sStep = BastionBreak.handleEvent (KeyDown (KeyChar 'c')) snap1 s0
      sStepped = BastionBreak.handleEvent (KeyDown (KeyChar 'd')) snap1 sStep
  assertBool "C toggles to StepOrbit" (BastionBreak.ssCamMode sStep == BastionBreak.StepOrbit)
  assertBool "StepOrbit hides CameraPivot" (findBody BastionBreak.CameraPivot (BastionBreak.renderSiege sStep) == Nothing)
  assertBool "StepOrbit increments ssCamAngle on D" (BastionBreak.ssCamAngle sStepped > BastionBreak.ssCamAngle sStep)

{-# NOINLINE applyIO #-}
applyIO :: (a -> b) -> a -> IO b
applyIO f x = evaluate (f x)

{-# NOINLINE applyIO2 #-}
applyIO2 :: (a -> b -> c) -> a -> b -> IO c
applyIO2 f x y = evaluate (f x y)

mkTestBody :: Int -> Double -> Body Int
mkTestBody bid y =
  dynamicBody bid (at (Vec3 (fromIntegral bid) y 0)) (SphereCollider 1.0) (sphere 1.0)

renderTestNode :: Int -> Double -> SceneTree Int
renderTestNode bid y = bodyNode (mkTestBody bid y)

renderTestNodeWith :: Double -> Int -> Double -> SceneTree Int
renderTestNodeWith yOff bid y = bodyNode (mkTestBody bid (y + yOff))

treeDepth :: SceneTree bid -> Int
treeDepth EmptyTree = 0
treeDepth (OneBody _) = 1
treeDepth (Branch l r) = 1 + max (treeDepth l) (treeDepth r)

testMemoAndSceneTree :: IO ()
testMemoAndSceneTree = do
  -- 1. memo: pointer reuse across steps + promotion + clearMemo invalidation
  clearMemo
  let memoVec = memo (\n -> Vec3 (fromIntegral n) (fromIntegral (n + 1)) (fromIntegral (n + 2)))
      arg1 = (100001 :: Int)
  v1 <- applyIO memoVec arg1
  v2 <- applyIO memoVec arg1
  assertBool "memo returns equal value" (v1 == v2)
  assertBool "memo hits mcCurr and returns exact same pointer" (samePtr v1 v2)

  advanceMemoGen
  v3 <- applyIO memoVec arg1
  assertBool "memo hits mcPrev after advanceMemoGen and preserves pointer" (samePtr v1 v3)

  advanceMemoGen
  v4 <- applyIO memoVec arg1
  assertBool "memo keeps promoted entry hot across step n+2" (samePtr v1 v4)

  clearMemo
  v5 <- applyIO memoVec arg1
  assertBool "memo recomputes equal value after clearMemo" (v1 == v5)
  assertBool "memo allocates fresh pointer after clearMemo" (not (samePtr v1 v5))

  -- 2. memo2: pointer reuse & invalidation when either argument changes
  clearMemo
  let memoPair = memo2 (\a b -> Vec3 a b (a + b))
      xA = 1.25 :: Double
      yA = 2.5 :: Double
      yB = 3.75 :: Double
  p1 <- applyIO2 memoPair xA yA
  p2 <- applyIO2 memoPair xA yA
  assertBool "memo2 returns exact same pointer on repeat call" (samePtr p1 p2)
  advanceMemoGen
  p3 <- applyIO2 memoPair xA yA
  assertBool "memo2 preserves pointer across advanceMemoGen" (samePtr p1 p3)
  p4 <- applyIO2 memoPair xA yB
  assertBool "memo2 recomputes when second arg changes" (p4 == Vec3 1.25 3.75 5.0 && not (samePtr p1 p4))

  -- 3. SceneTree Monoid, EmptyTree skipping, balanced bodies, treeBodies order
  let bs100 = [mkTestBody i 0.0 | i <- [1 .. 100]]
      t100 = bodies bs100
      b1 = head bs100
      b2 = bs100 !! 1
  assertBool "EmptyTree left identity" (EmptyTree <> bodyNode b1 == OneBody b1)
  assertBool "EmptyTree right identity" (bodyNode b1 <> EmptyTree == OneBody b1)
  assertBool "mconcat empty" ((mconcat [] :: SceneTree Int) == EmptyTree)
  assertBool
    "mconcat skips EmptyTree"
    (mconcat [EmptyTree, bodyNode b1, EmptyTree, bodyNode b2] == Branch (OneBody b1) (OneBody b2))
  assertBool "treeBodies preserves exact body list" (treeBodies t100 == bs100)
  assertBool "bodies builds balanced binary tree (depth <= 8 for 100 bodies)" (treeDepth t100 <= 8)

  -- 4. intMapTree & intMapTreeWith: O(1) untouched subtree pointer sharing + opposite-half mutations
  clearMemo
  let im0 = IM.fromList [(i, 1.0) | i <- [0 .. 255]]
  it0 <- applyIO (intMapTree renderTestNode) im0
  advanceMemoGen
  let im1 = IM.insert 42 99.0 im0
  it1 <- applyIO (intMapTree renderTestNode) im1
  case (it0, it1) of
    (Branch l0 r0, Branch l1 r1) ->
      assertBool
        "intMapTree shares exact pointer for untouched half of root Branch"
        (samePtr l0 l1 || samePtr r0 r1)
    _ -> assertBool "intMapTree of 256 items must be a Branch" False
  let (iRem1, iAdd1, iUpd1) = diffTree it0 it1
  assertBool "intMapTree 1-key update removes 0" (null iRem1)
  assertBool "intMapTree 1-key update adds 0" (null iAdd1)
  assertBool "intMapTree 1-key update updates only key 42" (map buId iUpd1 == [42])

  -- Mutate a key in the opposite half on the next step (tests deep weak cache!)
  advanceMemoGen
  let im2 = IM.insert 200 77.0 im1
  it2 <- applyIO (intMapTree renderTestNode) im2
  case (it1, it2) of
    (Branch l1 (Branch rl1 rr1), Branch l2 (Branch rl2 rr2)) -> do
      assertBool
        "intMapTree shares pointer for previously mutated half on opposite-half edit"
        (samePtr l1 l2)
      assertBool
        "intMapTree deep untouched child inside opposite half stays pointer-shared"
        (samePtr rl1 rl2 || samePtr rr1 rr2)
    _ -> assertBool "intMapTree must remain a nested Branch" False
  let (iRem2, iAdd2, iUpd2) = diffTree it1 it2
  assertBool "intMapTree step 2 removes 0" (null iRem2)
  assertBool "intMapTree step 2 adds 0" (null iAdd2)
  assertBool "intMapTree step 2 updates only key 200" (map buId iUpd2 == [200])

  -- intMapTreeWith: same env -> pointer hit; changed env -> updates all
  advanceMemoGen
  let env1 = 5.0 :: Double
      env2 = 10.0 :: Double
  iw0 <- applyIO2 (intMapTreeWith renderTestNodeWith) env1 im0
  advanceMemoGen
  iw1 <- applyIO2 (intMapTreeWith renderTestNodeWith) env1 im0
  assertBool "intMapTreeWith unchanged env+map returns exact same pointer" (samePtr iw0 iw1)
  iw2 <- applyIO2 (intMapTreeWith renderTestNodeWith) env2 im0
  let (iwRem, iwAdd, iwUpd) = diffTree iw1 iw2
  assertBool "intMapTreeWith env change produces 0 removals/additions" (null iwRem && null iwAdd)
  assertBool "intMapTreeWith env change updates all 256 bodies" (length iwUpd == 256)

  -- 5. mapTree, mapTreeWith, and BST rotation-safe reconciliation!
  clearMemo
  let m0 = Map.fromList [(i * 10, 1.0) | i <- [1 .. 20]]
  mt0 <- applyIO (mapTree renderTestNode) m0
  advanceMemoGen

  -- Insert keys [1..9] sequentially at the low end to force multiple Data.Map BST rotations!
  let checkInsert (prevMap, prevTree) k = do
        let nextMap = Map.insert k 2.0 prevMap
        nextTree <- applyIO (mapTree renderTestNode) nextMap
        let (rems, adds, upds) = diffTree prevTree nextTree
        assertBool ("rotation-safe insert " <> show k <> ": 0 spurious removals") (null rems)
        assertBool ("rotation-safe insert " <> show k <> ": adds ONLY key " <> show k) (map bodyId adds == [k])
        assertBool ("rotation-safe insert " <> show k <> ": 0 spurious updates") (null upds)
        advanceMemoGen
        pure (nextMap, nextTree)
  (mAfterIns, mtAfterIns) <- foldM checkInsert (m0, mt0) [1 .. 9]

  -- Delete keys that trigger rotations back the other way
  let checkDelete (prevMap, prevTree) k = do
        let nextMap = Map.delete k prevMap
        nextTree <- applyIO (mapTree renderTestNode) nextMap
        let (rems, adds, upds) = diffTree prevTree nextTree
        assertBool ("rotation-safe delete " <> show k <> ": removes ONLY key " <> show k) (rems == [k])
        assertBool ("rotation-safe delete " <> show k <> ": 0 spurious additions") (null adds)
        assertBool ("rotation-safe delete " <> show k <> ": 0 spurious updates") (null upds)
        advanceMemoGen
        pure (nextMap, nextTree)
  (mAfterDel, mtAfterDel) <- foldM checkDelete (mAfterIns, mtAfterIns) [1, 2, 3, 200, 190]

  -- Simultaneous rotation (multiple inserts) + in-place modification of existing key 100
  let mBoth =
        Map.insert 100 55.0 $
          Map.insert 1000 3.0 $
            Map.insert 1001 3.0 $
              Map.insert 1002 3.0 mAfterDel
  mtBoth <- applyIO (mapTree renderTestNode) mBoth
  let (bothRem, bothAdd, bothUpd) = diffTree mtAfterDel mtBoth
  assertBool "rotation + modify: 0 spurious removals" (null bothRem)
  assertBool "rotation + modify: adds only [1000,1001,1002]" (map bodyId bothAdd == [1000, 1001, 1002])
  assertBool "rotation + modify: updates only key 100" (map buId bothUpd == [100])

  -- mapTreeWith: unchanged env+map reuses pointer; changed env updates cleanly
  advanceMemoGen
  mw0 <- applyIO2 (mapTreeWith renderTestNodeWith) env1 m0
  advanceMemoGen
  mw1 <- applyIO2 (mapTreeWith renderTestNodeWith) env1 m0
  assertBool "mapTreeWith unchanged env+map returns exact same pointer" (samePtr mw0 mw1)
  mw2 <- applyIO2 (mapTreeWith renderTestNodeWith) env2 m0
  let (mwRem, mwAdd, mwUpd) = diffTree mw1 mw2
  assertBool "mapTreeWith env change updates all 20 bodies cleanly" (null mwRem && null mwAdd && length mwUpd == 20)

  -- 6. Proof that diffTree skips pointer-equal subtrees in O(1) without inspecting leaves
  let poisonMesh = GroupMesh [error "diffTree inspected pointer-equal shared subtree!"]
      poisonBody = staticBody (9999 :: Int) (at vZero) NoCollider poisonMesh
      sharedSubtree = Branch (OneBody poisonBody) (OneBody (mkTestBody 9998 0.0))
      treeA = Branch sharedSubtree (OneBody (mkTestBody 1 0.0))
      treeB = Branch sharedSubtree (OneBody (mkTestBody 1 5.0))
      (pRem, pAdd, pUpd) = diffTree treeA treeB
  assertBool "pointer-equal skip avoids touching shared subtree" (null pRem && null pAdd && map buId pUpd == [1])
  assertBool "identical root skips completely" (diffTree treeA treeA == ([], [], []))

testSkyAceMulti :: IO ()
testSkyAceMulti = do
  let g0 = gameInit SkyAce.skyAceMultiGame
      ws0 = gameWorld SkyAce.skyAceMultiGame g0
      v1 = gameView SkyAce.skyAceMultiGame SkyAce.Pilot1 g0
      v2 = gameView SkyAce.skyAceMultiGame SkyAce.Pilot2 g0
      sc1 = combineScene ws0 v1
      sc2 = combineScene ws0 v2

  -- Both pilots spawn in shared world with distinct per-pilot cameras and HUDs
  assertBool "2P world contains Player (Pilot1)" (findBody SkyAce.Player sc1 /= Nothing)
  assertBool "2P world contains Player2 (Pilot2)" (findBody SkyAce.Player2 sc1 /= Nothing)
  assertBool "Pilot1 and Pilot2 have distinct jet meshes" $
    fmap bodyMesh (findBody SkyAce.Player sc1) /= fmap bodyMesh (findBody SkyAce.Player2 sc1)
  assertBool "Pilot1 camera targets Player" $
    case pvCamera v1 of
      FollowCamera {camTarget = SkyAce.Player} -> True
      _ -> False
  assertBool "Pilot2 camera targets Player2" $
    case pvCamera v2 of
      FollowCamera {camTarget = SkyAce.Player2} -> True
      _ -> False
  assertBool "Pilot1 and Pilot2 have distinct HUDs" (pvHud v1 /= pvHud v2)
  assertBool "sc2 shares static forest with sc1" (length (allSceneBodies sc2) >= 21)

  -- Independent steering per pilot
  let snapBoth =
        Snapshot 1.0 $
          Map.fromList
            [ (SkyAce.Player, BodyState (Vec3 0 18 52) mempty (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Player2, BodyState (Vec3 16 18 52) mempty (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Target 1 1, BodyState (Vec3 0 25 (-10)) mempty vZero vZero)
            ]
      (g1, _) = gameStep SkyAce.skyAceMultiGame (PlayerInput SkyAce.Pilot1 (InKeyDown (KeyChar 'a'))) snapBoth g0
      (g2, _) = gameStep SkyAce.skyAceMultiGame (PlayerInput SkyAce.Pilot2 (InKeyDown KeyShift)) snapBoth g1
      scSteer = combineScene (gameWorld SkyAce.skyAceMultiGame g2) (gameView SkyAce.skyAceMultiGame SkyAce.Pilot1 g2)
  case (findBody SkyAce.Player scSteer, findBody SkyAce.Player2 scSteer) of
    (Just b1, Just b2) -> do
      case bodyDrive b1 of
        FlightDrive spd _ yaw _ _ ->
          assertBool "Pilot1 turning left without boost" (yaw > 0 && spd < 30)
        other -> assertBool ("unexpected Pilot1 drive: " <> show other) False
      case bodyDrive b2 of
        FlightDrive spd _ yaw _ _ ->
          assertBool "Pilot2 boosting straight" (spd > 30 && yaw == 0)
        other -> assertBool ("unexpected Pilot2 drive: " <> show other) False
    _ -> assertBool "both pilot bodies must exist" False

  -- Pilot 1 shoots an AI target -> +100 to Pilot 1 only
  let (gP1Shot, p1ShotCmds) =
        gameStep SkyAce.skyAceMultiGame (PlayerInput SkyAce.Pilot1 (InKeyDown KeySpace)) snapBoth g2
  assertBool "Pilot1 shot spawns 1 bullet" (IM.size (SkyAce.gsBullets gP1Shot) == 1)
  assertBool "Pilot1 shot emits sound command" (not (null p1ShotCmds))
  p1Bullet <- case IM.elems (SkyAce.gsBullets gP1Shot) of
    [b] -> pure b
    other -> assertBool ("expected 1 Pilot1 bullet, got " <> show (length other)) False >> error "unreachable"
  let (gTargetHit, _) =
        gameStep
          SkyAce.skyAceMultiGame
          (WorldCollisionStart (SkyAce.Bullet (SkyAce.bulletNum p1Bullet)) (SkyAce.Target 1 1))
          snapBoth
          gP1Shot
  assertBool "Pilot1 score is 100 after target kill" (SkyAce.psScore (SkyAce.pilotState SkyAce.Pilot1 gTargetHit) == 100)
  assertBool "Pilot2 score remains 0" (SkyAce.psScore (SkyAce.pilotState SkyAce.Pilot2 gTargetHit) == 0)

  -- Dogfight: Pilot 2 shoots down Pilot 1 -> +250 to Pilot 2, Pilot 1 downed with 3s respawn timer
  let (gP2Shot, _) =
        gameStep SkyAce.skyAceMultiGame (PlayerInput SkyAce.Pilot2 (InKeyDown KeySpace)) snapBoth gTargetHit
  p2Bullet <- case IM.elems (SkyAce.gsBullets gP2Shot) of
    [b] -> pure b
    other -> assertBool ("expected 1 Pilot2 bullet, got " <> show (length other)) False >> error "unreachable"
  let (gDogfight, dogfightCmds) =
        gameStep
          SkyAce.skyAceMultiGame
          (WorldCollisionStart (SkyAce.Bullet (SkyAce.bulletNum p2Bullet)) SkyAce.Player)
          snapBoth
          gP2Shot
  assertBool "Pilot2 earns +250 for dogfight takedown" (SkyAce.psScore (SkyAce.pilotState SkyAce.Pilot2 gDogfight) == 250)
  assertBool "dogfight takedown emits crash audio" (not (null dogfightCmds))
  case SkyAce.psStatus (SkyAce.pilotState SkyAce.Pilot1 gDogfight) of
    SkyAce.Crashed _ -> pure ()
    SkyAce.Flying -> assertBool "Pilot1 should be Crashed after dogfight hit" False
  let timersAfterHit = wsTimers (gameWorld SkyAce.skyAceMultiGame gDogfight)
  assertBool
    "Pilot1 respawn timer (3.0s) is scheduled"
    (Once (SkyAce.PilotRespawn SkyAce.Pilot1) 3.0 `elem` timersAfterHit)

  -- Pilot 1 respawns after 3s timer keeping their 100 score and emitting Teleport
  let (gRespawn, respawnCmds) =
        gameStep
          SkyAce.skyAceMultiGame
          (WorldTimerFired (SkyAce.PilotRespawn SkyAce.Pilot1))
          snapBoth
          gDogfight
  assertBool "Pilot1 returns to Flying on respawn" (SkyAce.psStatus (SkyAce.pilotState SkyAce.Pilot1 gRespawn) == SkyAce.Flying)
  assertBool "Pilot1 keeps 100 score after respawn" (SkyAce.psScore (SkyAce.pilotState SkyAce.Pilot1 gRespawn) == 100)
  assertBool
    "Pilot1 respawn emits Teleport command"
    (any (\c -> case c of WorldCmd (Teleport SkyAce.Player _) -> True; _ -> False) respawnCmds)

  -- PlayerLeft despawns pilot, PlayerJoined re-adds pilot
  let (gLeft, _) = gameStep SkyAce.skyAceMultiGame (PlayerLeft SkyAce.Pilot2) snapBoth gRespawn
  assertBool "PlayerLeft removes Player2 body" (findBody SkyAce.Player2 (combineScene (gameWorld SkyAce.skyAceMultiGame gLeft) (gameView SkyAce.skyAceMultiGame SkyAce.Pilot1 gLeft)) == Nothing)
  let (gRejoin, _) = gameStep SkyAce.skyAceMultiGame (PlayerJoined SkyAce.Pilot2) snapBoth gLeft
  assertBool "PlayerJoined restores Player2 body" (findBody SkyAce.Player2 (combineScene (gameWorld SkyAce.skyAceMultiGame gRejoin) (gameView SkyAce.skyAceMultiGame SkyAce.Pilot1 gRejoin)) /= Nothing)

testRollbackEngine :: IO ()
testRollbackEngine = do
  -- 1. Deterministic integer-tick TickTimers
  let t0 = reconcileTickTimers [Once ("b" :: String) 0.05, Once "a" 0.05, Every "pulse" 0.05] emptyTickTimers
      -- 0.05s at 60Hz = 3 ticks
      (f1, t1) = stepTickTimers t0
      (f2, t2) = stepTickTimers t1
      (f3, t3) = stepTickTimers t2
      (f4, t4) = stepTickTimers t3
      (f5, t5) = stepTickTimers t4
      (f6, _) = stepTickTimers t5
  assertBool "timers do not fire on tick 1" (null f1)
  assertBool "timers do not fire on tick 2" (null f2)
  assertBool "simultaneous timers fire in sorted Ord order on tick 3" (f3 == ["a", "b", "pulse"])
  assertBool "timers silent on tick 4" (null f4)
  assertBool "timers silent on tick 5" (null f5)
  assertBool "Every timer repeats on tick 6 while Once stays fired" (f6 == ["pulse"])
  let tCancelled = reconcileTickTimers [] t3
      (fCancel, _) = stepTickTimers tCancelled
  assertBool "removing timer from scene cancels it" (null fCancel)

  -- 2. Deterministic hashSnapshot with 1 mm quantization & quaternion sign invariance
  let snapA =
        Snapshot 1.0 $
          Map.fromList
            [ (SkyAce.Player, BodyState (Vec3 1.0001 2.0 3.0) (Quat 0 0 0 1) (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Player2, BodyState (Vec3 16.0 18.0 52.0) (Quat 0 0.7071 0 0.7071) vZero vZero)
            ]
      snapSameMmAndNegQuat =
        Snapshot 99.0 $
          Map.fromList
            [ (SkyAce.Player, BodyState (Vec3 1.0002 2.0 3.0) (Quat 0 0 0 (-1)) (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Player2, BodyState (Vec3 16.0 18.0 52.0) (Quat 0 (-0.7071) 0 (-0.7071)) vZero vZero)
            ]
      snapMoved =
        Snapshot 1.0 $
          Map.fromList
            [ (SkyAce.Player, BodyState (Vec3 1.05 2.0 3.0) (Quat 0 0 0 1) (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Player2, BodyState (Vec3 16.0 18.0 52.0) (Quat 0 0.7071 0 0.7071) vZero vZero)
            ]
  assertBool "hashSnapshot ignores sub-mm jitter and q/-q sign flip" (hashSnapshot snapA == hashSnapshot snapSameMmAndNegQuat)
  assertBool "hashSnapshot changes when a body moves > 1 mm" (hashSnapshot snapA /= hashSnapshot snapMoved)

  -- 3. Full 2-client + 1-server rollback simulation with 4-tick latency on SkyAce.skyAceMultiGame
  let initSnap =
        Snapshot 0.0 $
          Map.fromList
            [ (SkyAce.Player, BodyState (Vec3 0 18 52) mempty (Vec3 0 0 (-25)) vZero)
            , (SkyAce.Player2, BodyState (Vec3 16 18 52) mempty (Vec3 0 0 (-25)) vZero)
            ]
      initW = gameInit SkyAce.skyAceMultiGame
      initTm = reconcileTickTimers (wsTimers (gameWorld SkyAce.skyAceMultiGame initW)) emptyTickTimers

      -- Pure deterministic micro-physics advance: moves each dynamic pilot & bullet forward 1 tick
      pureStepTick inputs (w0, s0, tm0) =
        let (fired, tm1) = stepTickTimers tm0
            evs = map (uncurry PlayerInput) inputs <> map WorldTimerFired fired
            stepOne (!wAcc, !cAcc) ev =
              let (!wNext, cs) = gameStep SkyAce.skyAceMultiGame ev s0 wAcc
               in (wNext, cAcc <> cs)
            (!w1, cmds) = foldl stepOne (w0, []) evs
            dt = 1.0 / fromIntegral ticksPerSecond
            ws1 = gameWorld SkyAce.skyAceMultiGame w1
            stepBody acc b =
              let prev = Map.findWithDefault (BodyState (posePos (bodyPose b)) (poseRot (bodyPose b)) (bodyInitialVel b) vZero) (bodyId b) (snapBodies s0)
                  vel' = case bodyDrive b of
                    FlightDrive spd _ _ _ _ -> spd *^ forwardOf (bsRot prev)
                    _ -> bsVel prev
                  pos' = bsPos prev + dt *^ vel'
               in Map.insert (bodyId b) (prev {bsPos = pos', bsVel = vel'}) acc
            bodies' = foldl stepBody Map.empty (treeBodies (wsTree ws1))
            s1 = Snapshot (snapTime s0 + dt) bodies'
            tm2 = reconcileTickTimers (wsTimers ws1) tm1
         in (w1, s1, tm2, cmds)

      -- Authoritative server schedule across ticks 0..5:
      --   tick 0: empty
      --   tick 1: Pilot1 turns left ('a')
      --   tick 2: Pilot2 fires laser (delayed remote input arriving 4 ticks later at client 1!)
      --   tick 3: Pilot1 fires laser (local input on client 1)
      --   tick 4: empty
      --   tick 5: empty
      serverInputs :: Map.Map Tick [(SkyAce.Pilot, InputEvent SkyAce.FlyBody)]
      serverInputs =
        Map.fromList
          [ (0, [])
          , (1, [(SkyAce.Pilot1, InKeyDown (KeyChar 'a'))])
          , (2, [(SkyAce.Pilot2, InKeyDown KeySpace)])
          , (3, [(SkyAce.Pilot1, InKeyDown KeySpace)])
          , (4, [])
          , (5, [])
          ]

      runServer (w, s, tm) t =
        let inps = Map.findWithDefault [] t serverInputs
            (w', s', tm', _) = pureStepTick inps (w, s, tm)
         in (w', s', tm')

      (srvW5, srvSnap5, srvTm5) = foldl runServer (initW, initSnap, initTm) [0 .. 5]

      -- Client 1 predicts ticks 0..5 knowing ONLY Pilot 1's local inputs
      client1Local :: Map.Map Tick [(SkyAce.Pilot, InputEvent SkyAce.FlyBody)]
      client1Local =
        Map.fromList
          [ (1, [(SkyAce.Pilot1, InKeyDown (KeyChar 'a'))])
          , (3, [(SkyAce.Pilot1, InKeyDown KeySpace)])
          ]

      runClient1 (w, s, tm, rb, playedLog) t =
        let inps = Map.findWithDefault [] t client1Local
            rb1 = foldl (\r (p, i) -> recordLocalInput t p i r) rb inps
            rb2 = recordHistory t w s tm inps rb1
            (w', s', tm', cmds) = pureStepTick inps (w, s, tm)
            (deduped, rb3) = dedupSounds t cmds rb2
         in (w', s', tm', rb3, playedLog <> [(t, deduped)])

      (c1W5Pred, c1Snap5Pred, _, rbPred, predLog) =
        foldl runClient1 (initW, initSnap, initTm, initRollback initW initSnap, []) [0 .. 5]

  -- Before rollback, Client 1 diverged from server (missed Pilot 2's bullet at tick 2)
  assertBool "predicted Client 1 misses Pilot 2's bullet before rollback" (c1W5Pred /= srvW5)
  assertBool "predicted Client 1 snapshot hash differs from server before rollback" (hashSnapshot c1Snap5Pred /= hashSnapshot srvSnap5)
  assertBool "predicted Client 1 played Pilot 1's laser at tick 3" (any (\(t, cs) -> t == 3 && not (null cs)) predLog)

  -- Server confirms tick 0 and tick 1 -> exact match (ReconcileOk)
  rbAfter0 <- case confirmServerTick 0 [] Nothing rbPred of
    ReconcileOk r -> pure r
    ReconcileRollback {} -> assertBool "tick 0 should be ReconcileOk" False >> pure rbPred
  assertBool "rsConfirmedTick advanced to 0" (rsConfirmedTick rbAfter0 == 0)

  rbAfter1 <- case confirmServerTick 1 [(SkyAce.Pilot1, InKeyDown (KeyChar 'a'))] Nothing rbAfter0 of
    ReconcileOk r -> pure r
    ReconcileRollback {} -> assertBool "tick 1 should be ReconcileOk" False >> pure rbAfter0
  assertBool "rsConfirmedTick advanced to 1" (rsConfirmedTick rbAfter1 == 1)
  assertBool "history frame 0 was pruned after confirming tick 1" (Map.notMember 0 (rsHistory rbAfter1))

  -- Server confirms tick 2 with delayed remote input from Pilot 2 -> MISPREDICTION!
  case confirmServerTick 2 [(SkyAce.Pilot2, InKeyDown KeySpace)] Nothing rbAfter1 of
    ReconcileOk _ -> assertBool "tick 2 must trigger ReconcileRollback on remote Pilot 2 input" False
    ReconcileRollback rewindTick frame2 schedule rbRewound -> do
      assertBool "rollback rewinds to tick 2" (rewindTick == 2)
      assertBool "replay schedule covers ticks 2..5" (map fst schedule == [2, 3, 4, 5])
      assertBool
        "replay schedule preserves Pilot 1's buffered local input at tick 3"
        (lookup 3 schedule == Just [(SkyAce.Pilot1, InKeyDown KeySpace)])

      -- Fast-forward replay from frame2
      let replayStep (w, s, tm, rb, audAcc) (t, inps) =
            let rb1 = recordHistory t w s tm inps rb
                (w', s', tm', cmds) = pureStepTick inps (w, s, tm)
                (deduped, rb2) = dedupSounds t cmds rb1
             in (w', s', tm', rb2, audAcc <> [(t, deduped)])
          (replayedW5, replayedSnap5, replayedTm5, rbAfterReplay, replayLog) =
            foldl
              replayStep
              (hfWorld frame2, hfSnapshot frame2, hfTimers frame2, rbRewound, [])
              schedule

      assertBool "replayed world matches authoritative server identically" (replayedW5 == srvW5)
      assertBool "replayed snapshot hash matches authoritative server identically" (hashSnapshot replayedSnap5 == hashSnapshot srvSnap5)
      assertBool "replayed timers match authoritative server identically" (replayedTm5 == srvTm5)
      assertBool
        "replay plays newly discovered Pilot 2 laser at tick 2"
        (any (\(t, cs) -> t == 2 && not (null cs)) replayLog)
      assertBool
        "dedupSounds suppresses duplicate Pilot 1 laser at tick 3 during replay"
        (all (\(t, cs) -> t /= 3 || null cs) replayLog)

      -- Snapshot hash mismatch forces rollback even when inputs match
      case confirmServerTick 3 [(SkyAce.Pilot1, InKeyDown KeySpace)] (Just 0xDEADBEEF) rbAfterReplay of
        ReconcileRollback rt _ _ _ ->
          assertBool "hash mismatch triggers rollback at tick 3" (rt == 3)
        ReconcileOk _ ->
          assertBool "hash mismatch must trigger rollback" False

testYoursCraft :: IO ()
testYoursCraft = do
  clearMemo
  let v0 = YoursCraft.initialVoxelWorld 12
  assertBool "YoursCraft generates > 500 blocks for radius 12" (YoursCraft.vsBlockCount v0 > 500)
  assertBool "YoursCraft organizes blocks into multiple 8x8 chunks" (Map.size (YoursCraft.vsChunks v0) >= 9)

  -- Clicked face normal detection
  assertBool "top face normal" (YoursCraft.clickedFaceOffset (3, 2, 4) (Vec3 3.1 2.5 3.9) == (0, 1, 0))
  assertBool "east face normal" (YoursCraft.clickedFaceOffset (3, 2, 4) (Vec3 3.5 2.1 4.0) == (1, 0, 0))
  assertBool "south face normal" (YoursCraft.clickedFaceOffset (3, 2, 4) (Vec3 3.0 2.0 4.5) == (0, 0, 1))

  -- Right-click places a block on the clicked top face and produces an exact 1-block delta!
  let sc0 = YoursCraft.renderVoxelMemo v0
      (p0, ds0) = diffScene Nothing sc0
      (initBytes, ws0) = encodePatch p0 [] emptyWireState
  _ <- evaluate (LBS.length initBytes)
  advanceMemoGen
  let (v1, placeCmds) =
        YoursCraft.stepVoxel
          (MouseDown MouseRight (Just (YoursCraft.Voxel 2 10 2, Vec3 2.0 10.5 2.0)))
          emptySnapshot
          v0
  assertBool "placing a block increments block count by 1" (YoursCraft.vsBlockCount v1 == YoursCraft.vsBlockCount v0 + 1)
  assertBool "placing a block emits sound command" (not (null placeCmds))
  let sc1 = YoursCraft.renderVoxelMemo v1
      (p1, ds1) = diffScene (Just ds0) sc1
      (deltaPlaceBytes, _) = encodePatch p1 [] ws0
      (fullResetBytes, _) = encodePatch (PatchReset sc1) [] emptyWireState
  case p1 of
    PatchDelta d -> do
      assertBool "placing 1 block removes 0 bodies" (null (sdRemoved d))
      assertBool "placing 1 block adds ONLY Voxel 2 11 2" (map bodyId (sdAdded d) == [YoursCraft.Voxel 2 11 2])
      assertBool "placing 1 block updates 0 existing bodies" (null (sdUpdated d))
    PatchReset _ -> assertBool "placing 1 block must emit PatchDelta" False
  assertBool
    "delta patch is >100x smaller than full no-diff scene JSON"
    (LBS.length deltaPlaceBytes * 100 < LBS.length fullResetBytes)

  -- Left-click breaks the newly placed block and emits an exact 1-body removal delta!
  advanceMemoGen
  let (v2, breakCmds) =
        YoursCraft.stepVoxel
          (MouseDown MouseLeft (Just (YoursCraft.Voxel 2 11 2, Vec3 2.0 11.0 2.0)))
          emptySnapshot
          v1
  assertBool "breaking a block restores original block count" (YoursCraft.vsBlockCount v2 == YoursCraft.vsBlockCount v0)
  assertBool "breaking a block emits sound command" (not (null breakCmds))
  case diffScene (Just ds1) (YoursCraft.renderVoxelMemo v2) of
    (PatchDelta d, _) -> do
      assertBool "breaking 1 block removes ONLY Voxel 2 11 2" (sdRemoved d == [YoursCraft.Voxel 2 11 2])
      assertBool "breaking 1 block adds 0 bodies" (null (sdAdded d))
      assertBool "breaking 1 block updates 0 bodies" (null (sdUpdated d))
    (PatchReset _, _) -> assertBool "breaking 1 block must emit PatchDelta" False

  -- Space emits ApplyImpulse upward jump on Player
  let (_, jumpCmds) = YoursCraft.stepVoxel (KeyDown KeySpace) emptySnapshot v2
  assertBool
    "Space emits upward ApplyImpulse on Player"
    (any (\c -> case c of ApplyImpulse YoursCraft.Player (Vec3 _ vy _) -> vy > 0; _ -> False) jumpCmds)

  -- First-person mouse-look camera & CameraWalkDrive defaults + V toggle
  assertBool
    "YoursCraft defaults to FirstPersonCamera"
    (case sceneCamera (YoursCraft.renderVoxelMemo v0) of FirstPersonCamera {} -> True; _ -> False)
  let (vWalk, _) = YoursCraft.stepVoxel (KeyDown (KeyChar 'w')) emptySnapshot v0
      playerBodyWalk = filter ((== YoursCraft.Player) . bodyId) (allSceneBodies (YoursCraft.renderVoxelMemo vWalk))
  assertBool
    "Holding W in YoursCraft sets positive forward CameraWalkDrive"
    (case playerBodyWalk of
       [b] -> case bodyDrive b of CameraWalkDrive fwd strafe -> fwd > 0 && abs strafe < 1e-6; _ -> False
       _ -> False)
  let (v3rd, _) = YoursCraft.stepVoxel (KeyDown (KeyChar 'v')) emptySnapshot v0
  assertBool
    "Pressing V toggles YoursCraft to FollowCamera"
    (case sceneCamera (YoursCraft.renderVoxelMemo v3rd) of FollowCamera {} -> True; _ -> False)

main :: IO ()
main = do
  testMathOperators
  testMemoAndSceneTree
  testDiffAndProtocol
  testSkyAce
  testSkyAceMulti
  testRollbackEngine
  testChamberShift
  testBastionBreak
  testYoursCraft
  putStrLn "ALL TESTS PASSED!"
