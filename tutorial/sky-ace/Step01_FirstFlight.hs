{-# LANGUAGE OverloadedStrings #-}

-- | Step 1: First Flight
--
-- A complete, working 3D game in ~55 lines with ZERO game state ('()')
-- and ZERO timers ('Void'). The plane flies continuously at 60 FPS inside
-- Rapier3D + Three.js without ever waking Haskell on frame ticks.
module Step01_FirstFlight (run) where

import Data.Void (Void)
import Game.EventHorizon

-- | Type-safe identifiers for the 3D bodies in our world.
data BodyTag
  = Player
  | Ground
  deriving (Eq, Ord, Show)

render :: () -> Scene BodyTag Void
render () =
  defaultScene
    { sceneGravity = Vec3 0 (-18) 0
    , sceneBackground = skyBlue
    , sceneCamera =
        FollowCamera
          { camTarget = Player
          , camOffset = Vec3 0 3.0 10.5
          , camLookOffset = Vec3 0 0.4 (-14.0)
          , camLocal = True
          , camSmooth = 7.5
          , camFov = 62
          }
    , sceneTree = bodyNode groundBody <> bodyNode playerBody
    , sceneHud =
        [ HudText TopLeft "Step 1: First Flight (zero Haskell ticks!)"
        ]
    }

groundBody :: Body BodyTag
groundBody =
  staticBody
    Ground
    (at (Vec3 0 (-1) 0))
    (BoxCollider (Vec3 600 2 600))
    (colored (Color 0.24 0.52 0.24) (box (Vec3 600 2 600)))

playerBody :: Body BodyTag
playerBody =
  ( dynamicBody
      Player
      (at (Vec3 0 18 52))
      (BoxCollider (Vec3 3.0 0.9 2.6))
      airplaneMesh
  )
    { bodyGravity = 0.0
    , bodyDrive =
        FlightDrive
          { flightSpeed = 25.0
          , flightPitch = 0.0
          , flightYaw = 0.0
          , flightRoll = 0.0
          , flightAutoLevel = True
          }
    }

airplaneMesh :: Mesh
airplaneMesh =
  group
    [ colored (Color 0.20 0.52 0.88) $ box (Vec3 0.72 0.55 2.9)
    , colored gold $ translate (Vec3 0 0 (-1.75)) $ rotate (Vec3 (-pi / 2) 0 0) $ cone 0.36 0.75
    , colored white $ translate (Vec3 0 (-0.02) 0.1) $ box (Vec3 4.4 0.12 0.95)
    , colored white $ translate (Vec3 0 0.08 1.25) $ box (Vec3 1.7 0.10 0.45)
    , colored red $ translate (Vec3 0 0.52 1.2) $ box (Vec3 0.12 0.82 0.58)
    ]

-- | Nothing to react to yet!
handleEvent :: Event BodyTag Void -> Snapshot BodyTag -> () -> ()
handleEvent _ _ () = ()

run :: Int -> IO ()
run port =
  play
    (defaultConfig "Sky Ace Tutorial — Step 1: First Flight") {cfgPort = port}
    ()
    render
    handleEvent
