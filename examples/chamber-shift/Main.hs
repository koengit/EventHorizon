{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game.EventHorizon
import ChamberShift (initialPuzzle, renderPuzzle, stepPuzzle)

main :: IO ()
main =
  playWith
    (defaultConfig "Chamber Shift — 3D Physics Puzzle") {cfgPort = 3001}
    initialPuzzle
    renderPuzzle
    stepPuzzle
