{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module Rendering where

import CodeWorld
    ( black,
      blue,
      brown,
      green,
      orange,
      purple,
      red,
      translucent,
      white,
      yellow,
      blank,
      colored,
      lettering,
      rectangle,
      solidRectangle,
      translated,
      Color,
      Picture )
-- import Data.String  
import Data.Text     (singleton)

import GameCore

type Cell = Int
type Mark = Int

strToPic :: String -> Picture
strToPic []         = blank
strToPic (chr:chrs) = lettering (singleton chr) <>  translated 0.5 0 (strToPic chrs)


getColor :: Mark -> Color
getColor 0    = translucent yellow
getColor 2    = yellow
getColor 4    = translucent brown
getColor 8    = red
getColor 16   = translucent blue
getColor 32   = brown
getColor 64   = translucent green
getColor 128  = blue
getColor 256  = translucent red
getColor 512  = green
getColor 1024 = translucent purple
getColor 2048 = orange
getColor _    = black

-- 
drawMark :: Mark -> Picture
drawMark n 
 | n == 0 = blank
 | otherwise = colored white (strToPic (show n))-- (show n)

-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (cellPicture <> rectangle 3 3 <> colored (getColor cell) (solidRectangle 3 3))
    where
      x = fromIntegral i
      y = fromIntegral j
      cellPicture = translated (-0.8) 0 (drawMark cell)-- <> 
                   -- (colored (getColor cell) (solidRectangle 1.8 1.8)))


drawBoardLine :: [Int] -> Int -> [Cell] -> Picture
drawBoardLine _ _ [] = blank
drawBoardLine [] _ (_:_) = blank
drawBoardLine (x:xs) y (cell:cells) = drawCellAt x y cell <> drawBoardLine xs y cells

drawBoardAll :: [Int] -> [Int] -> Board -> Picture
drawBoardAll _ _ [] = blank
drawBoardAll _ [] (_:_) = blank
drawBoardAll xs (y:ys) (cell:cells) = drawBoardLine xs y cell <> drawBoardAll xs ys cells


-- | Draw a rectangular board.
drawBoard :: Board -> Picture
drawBoard [] = blank
drawBoard board = drawBoardAll [-6, -3 ..] [3, 0 ..] board

getScore :: Int -> Int
getScore x
  | x >= 4096 = x * 2
  | x == 2048 = 4000
  | x == 1024 = 1800
  | x == 512 = 700
  | x == 256 = 360
  | x == 128 = 160
  |otherwise = x

sumScore :: Board -> Int
sumScore = sum . map getScore . concat

printScore :: Board -> Picture
printScore board 
  | stalled board   = translated (-4) 6 (strToPic "You lose!")
  | completed board = translated (-4) 6 (strToPic "You won!")
  | otherwise       = translated (-4) 6 (strToPic "Score:") <> translated 0 6 (strToPic ( show (sumScore board)))

render :: Board -> Picture
render board = drawBoard board <> printScore board