{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module Game where

import CodeWorld (activityOf)

import GameCore ( Board, addTile, gameHandler )
import Rendering ( render )



initState :: Board
initState = addTile (replicate 4 (replicate 4 0))

game :: IO()
game = activityOf initState gameHandler render