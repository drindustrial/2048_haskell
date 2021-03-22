{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module GameCore where
import Data.List ( maximumBy, elemIndices, transpose )
import System.Random ( mkStdGen, Random(randomR) ) 
import CodeWorld ( Event(KeyPress) )

import Data.Maybe ( fromJust ) 
import Data.Ord ( comparing )

size :: Int
size = 4

type Board = [[Int]]

data Move = MoveUp | MoveRight | MoveDown | MoveLeft
  deriving (Show)
-- data Move = MoveLeft | MoveRight | MoveUp | MoveDown
-- 	deriving (Show)

allMoves :: [Move]
allMoves = [MoveLeft, MoveRight, MoveUp, MoveDown]

-- Move all tiles to the possible left position;
-- base function for all moves
slideLeft :: Board -> Board
slideLeft = map slideRow
  where slideRow [ ] = [ ]
        slideRow [x] = [x]
        slideRow (x:y:zs)
          | x == 0 = slideRow (y : zs) ++ [0]
          | y == 0 = slideRow (x : zs) ++ [0]
          | x == y = (x + y) : slideRow zs ++ [0]
          | otherwise = x : slideRow (y : zs)

-- Moving of board in arbitary direction
-- utilize slideLeft and transposed board 
slide :: Move -> Board -> Board
slide MoveUp = transpose . slideLeft . transpose
slide MoveRight  = map reverse . slideLeft . map reverse
slide MoveDown = transpose . map reverse . slideLeft . map reverse . transpose
slide MoveLeft  = slideLeft

-- Return True if player got tile >= 2048
completed :: Board -> Bool
completed = any (>= 2048) . concat

-- Return True if there are no valid moves left
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
emptyTiles = concatMap (uncurry search) . zip [0..(size - 1)]
  where search n = zip (replicate size n) . elemIndices 0

-- Given a point, update replaces the value at the point on the board with the given value
updateBoard :: (Int, Int) -> Int -> Board -> Board
updateBoard (rowI, columnI) value = updateIndex (updateIndex (const value) columnI) rowI
  where updateIndex fn i list = take i list ++ fn (list !! max 0 i) : tail (drop i list) -- (head $ drop i list)

sumBoard :: Board -> Int
sumBoard = sum . map sum
  
-- 90% of the time the tile is a 2, 10% of the time it is a 4 
valueForProb :: Int -> Int
valueForProb p 
  | p == 1    = 4
  | otherwise = 2

-- Adds a tile to a random empty spot.
addTile :: Board -> Board
addTile b = updateBoard newPoint newValue b
 where
  tiles = emptyTiles b
  g = mkStdGen (sumBoard b * 57)
  (ind, nextg) = randomR (0, length tiles - 1) g -- >>= return . (tiles !!)
  newPoint = fromJust (lookup ind $ zip [0, 1 ..] tiles)
  (newValueP, _) = randomR (1, 10 :: Int) nextg-- (randomR (1, 10 :: Int) g) >>= return . \x -> if x == 1 then 4 else 2
  newValue = valueForProb newValueP

-- add tile if voard was cnhaged after move 
addOrNot :: Board -> Board -> Board
addOrNot origBoard afterMBoard
  | origBoard == afterMBoard = origBoard
  | otherwise                = addTile afterMBoard
  

-- Change the dimensionality of board
oneToTwo :: [Int] -> [[Int]]
oneToTwo [] = []
oneToTwo board1 = take size board1: oneToTwo (drop size board1)
 

addOrNot' :: Board -> Board -> Maybe [Int]
addOrNot' board1 board2
  | board1 == board2 = Nothing
  | otherwise        = Just (concat (addOrNot board1 board2))


takeTurn :: Move -> [Int] -> Maybe [Int]
takeTurn move board1d = nboard1d
        where 
          board2d = oneToTwo board1d
          b = maybe board2d (`slide` board2d) $ Just move
          -- nboard2d = addOrNot board2d b
          nboard1d = addOrNot' board2d b

-- solver
-- Return the best move
bestMove :: Int -> [Int] -> Move
bestMove depth grid = snd bestValueMove
	where 
		valueMoves	 	= [ (value, move) |
							move	<- allMoves,
							newGrid <- [ takeTurn move grid ],
                            newGrid	/= Nothing,
							value 	<- [ gridValue depth (fromJust newGrid) ]]
		bestValueMove 	= maximumBy (comparing fst) valueMoves

-- Return the value of the board,
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
                            newGrid /= Nothing,
							value 	<- [ gridValue (depth-1) (fromJust newGrid) + 1]]


-- handler
gameHandler :: Event -> Board -> Board
gameHandler (KeyPress "B") board = nb
  where
    bMove = bestMove 5 (concat board)
    b = maybe board (`slide` board) $ Just (bMove)
    nb = addOrNot board b
    
gameHandler (KeyPress c) board = b1
  where 
    b = maybe board (`slide` board) $ lookup c $ [("W",MoveUp),("A",MoveLeft),("S",MoveDown),("D",MoveRight)] -- [MoveUp, MoveLeft, MoveDown, MoveRight]
    b1 = addOrNot board b


gameHandler _ board = board