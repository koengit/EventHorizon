{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game.EventHorizon
import BastionBreak (CameraMode (..), initialSiegeWith, renderSiege, stepSiege)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let mode =
        if "--step-camera" `elem` args
          then StepOrbit
          else SmoothOrbit
  playWith
    (defaultConfig "Bastion Break — 3D Siege Demolition") {cfgPort = 3002}
    (initialSiegeWith mode)
    renderSiege
    stepSiege
