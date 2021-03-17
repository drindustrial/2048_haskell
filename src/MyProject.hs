{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module MyProject where

import Data.List     (elemIndices, intercalate, transpose)
import System.IO     (BufferMode(..), hSetBuffering, stdin)
import System.Random 
import CodeWorld
import Data.String  
import Data.Text     (singleton)
import Data.Maybe 
import Data.Time
import Data.List
import Data.Ord
import Control.Monad

-- Each inner list is a row, starting with the top row
-- A 0 is an empty tile
type Board = [[Int]]


data Direction = North | East | South | West

slideLeft :: Board -> Board
slideLeft = map slideRow
  where slideRow [ ] = [ ]
        slideRow [x] = [x]
        slideRow (x:y:zs)
          | x == 0 = slideRow (y : zs) ++ [0]
          | y == 0 = slideRow (x : zs) ++ [0]
          | x == y = (x + y) : slideRow zs ++ [0]
          | otherwise = x : slideRow (y : zs)

slide :: Direction -> Board -> Board
slide North = transpose . slideLeft . transpose
slide East  = map reverse . slideLeft . map reverse
slide South = transpose . map reverse . slideLeft . map reverse . transpose
slide West  = slideLeft

-- Tells us if the player won the game by getting a 2048 tile
completed :: Board -> Bool
completed b = any (elem 2048) b

-- Tells us if the game is over because there are no valid moves left
stalled :: Board -> Bool
stalled b = all stalled' b && all stalled' (transpose b)
  where stalled' row = notElem 0 row && noNeighbors row
        noNeighbors [ ] = True
        noNeighbors [_] = True
        noNeighbors (x:y:zs)
          | x == y    = False
          | otherwise = noNeighbors (y:zs)

-- Returns tuples of the indices of all of the empty tiles
emptyTiles :: Board -> [(Int, Int)]
emptyTiles = concatMap (uncurry search) . zip [0..3]
  where search n = zip (replicate 4 n) . elemIndices 0

-- Given a point, update replaces the value at the point on the board with the given value
updateTile :: (Int, Int) -> Int -> Board -> Board
updateTile (rowI, columnI) value = updateIndex (updateIndex (const value) columnI) rowI
  where updateIndex fn i list = take i list ++ fn (head $ drop i list) : tail (drop i list)

sumBoard :: Board -> Int
sumBoard = sum . map sum
  
tOr4 :: Int -> Int
tOr4 p 
  | p == 1    = 4
  | otherwise = 2
-- Adds a tile to a random empty spot.
-- 90% of the time the tile is a 2, 10% of the time it is a 4 
addTile :: Board -> Board
addTile b = updateTile newPoint newValue b
 where
  tiles = emptyTiles b
  g = (mkStdGen ((sumBoard b) * 57))
  (ind, nextg) = randomR (0, length tiles - 1) g -- >>= return . (tiles !!)
  newPoint = fromJust (lookup ind $ (zip [0, 1 ..] tiles))
  (newValueP, _) = (randomR (1, 10 :: Int) nextg)-- (randomR (1, 10 :: Int) g) >>= return . \x -> if x == 1 then 4 else 2
  newValue = tOr4 newValueP


addOrNot :: Board -> Board -> Board
addOrNot origBoard afterMBoard
  | origBoard == afterMBoard = origBoard
  | otherwise                = addTile afterMBoard
  
  
oneToTwo :: [Int] -> [[Int]]
oneToTwo [] = []
oneToTwo board1 = (take 4 board1: oneToTwo (drop 4 board1))
 
 -- data Move = MoveLeft | MoveRight | MoveUp | MoveDown
 -- data Direction = North | East | South | West
 
moveToDir :: Move -> Direction
moveToDir MoveLeft = West
moveToDir MoveRight = East
moveToDir MoveUp = North
moveToDir MoveDown = South

--henler
gameHandler :: Event -> Board -> Board
gameHandler (KeyPress "B") board = nb
  where
    bMove = bestMove 5 (concat board)
    b = maybe board (`slide` board) $ Just (moveToDir bMove)
    nb = addOrNot board b
    
gameHandler (KeyPress c) board = b1
  where 
    b = maybe board (`slide` board) $ lookup c $ zip ["W","A","S","D"] [North, West, South, East]
    b1 = addOrNot board b

gameHandler _ board = board

type Cell = Int
type Mark = Int

strToPic :: String -> Picture
strToPic []         = blank
strToPic (chr:chrs) = lettering (singleton chr) <>  translated 0.5 0 (strToPic chrs)

getColor :: Mark -> Color
getColor 0    = translucent(yellow)
getColor 2    = yellow
getColor 4    = translucent (brown)
getColor 8    = red
getColor 16   = translucent (blue)
getColor 32   = brown
getColor 64   = translucent (green)
getColor 128  = blue
getColor 256  = translucent(red)
getColor 512  = green
getColor 1024 = translucent(purple)
getColor 2048 = orange
getColor _    = black

drawMark :: Mark -> Picture
drawMark n 
 | n == 0 = blank
 | otherwise = colored white (strToPic (show n))-- (show n)

-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (cellPicture <> rectangle 3 3 <> (colored (getColor cell) (solidRectangle 3 3)))
    where
      x = fromIntegral i
      y = fromIntegral j
      cellPicture = (translated (-0.8) 0 (drawMark cell))-- <> 
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
printScore board = translated (-4) 6 (strToPic "Score:") <> translated 0 6 (strToPic ( show (sumScore board)))

render :: Board -> Picture
render board = (drawBoard board) <> printScore board


-- solver
--temp
addOrNot' :: Board -> Board -> Maybe [Int]
addOrNot' board1 board2
  | board1 == board2 = Nothing
  | otherwise        = Just (concat (addOrNot board1 board2))

takeTurn :: Move -> [Int] -> Maybe [Int]
takeTurn move board1d = nboard1d
        where 
          board2d = oneToTwo board1d
          b = maybe board2d (`slide` board2d) $ Just (moveToDir move)
          nboard2d = addOrNot board2d b
          nboard1d = addOrNot' board2d b

-- /temp

-- Return the best move
-- Will throw an error if there are no valid moves
bestMove :: Int -> [Int] -> Move
bestMove depth grid 	= snd bestValueMove
	where 
		valueMoves	 	= [ (value, move) |
							move	<- allMoves,
							newGrid <- [ takeTurn move grid ],
							value 	<- [ gridValue depth (fromJust newGrid) ],
							newGrid	/= Nothing ]
		bestValueMove 	= maximumBy (comparing fst) valueMoves
-- <<< I decided not to return Nothing on a dead end, because then we can no longer
-- distinguish dead ends at different depths from eachother. >>>
--
-- Return the value of the grid,
-- + 1 for each depth traversed
-- -100 if a Game Over position is reached
gridValue :: Int -> [Int] -> Int
gridValue depth grid
	| depth == 0		= length $ filter (==0) grid
	| values == [] 	= -100
	| otherwise 		= maximum values
	where
		values 			= [ value | 
							move	<- allMoves,
							newGrid	<- [ takeTurn move grid ],
							value 	<- [ gridValue (depth-1) (fromJust newGrid) + 1],
							newGrid /= Nothing ]


-- /solver
game :: IO()
game = activityOf (addTile (replicate 4 (replicate 4 0))) gameHandler render