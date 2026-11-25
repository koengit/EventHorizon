-- | EventHorizon: A pure event-driven 3D game and physics library for Haskell.
--
-- Games are written with two pure functions parameterized by your own
-- type-safe body and timer identifier types (@bid@ and @tid@):
--
-- * @render :: world -> Scene bid tid@
-- * @handleEvent :: Event bid tid -> Snapshot bid -> world -> world@
--   (or @playWith@ returning @(world, [Command bid])@ for impulses, teleports, and audio)
--
-- Unlike frame-polled engines, your Haskell update function only runs when
-- something happens: player input, a physics collision/sensor event, or a timer.
module Game.EventHorizon
  ( -- * Entry points
    Config (..)
  , defaultConfig
  , play
  , playWith

    -- * Multi-User Game API & Runtimes
  , module Game.EventHorizon.Multi
  , module Game.EventHorizon.MultiEngine

    -- * Math, Vectors, Quaternions & Colors
  , module Game.EventHorizon.Math

    -- * Events, Snapshots & Commands
  , module Game.EventHorizon.Types

    -- * Scene & World Building
  , module Game.EventHorizon.Scene

    -- * Transparent Memoization
  , memo
  , memo2
  ) where

import Game.EventHorizon.Engine
import Game.EventHorizon.Math
import Game.EventHorizon.Memo (memo, memo2)
import Game.EventHorizon.Multi
import Game.EventHorizon.MultiEngine
import Game.EventHorizon.Scene
import Game.EventHorizon.Types
