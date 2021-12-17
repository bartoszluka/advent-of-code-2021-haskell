module Day04 where

import Data.List
import Inputs

data BingoTile = BingoTile
  { marked :: Bool,
    number :: Int
  }

unmarked = BingoTile False

mark :: Int -> BingoTile -> BingoTile
mark drewNumber tile =
  if number tile == drewNumber
    then BingoTile True drewNumber
    else tile

type Board = [[BingoTile]]

testBoard :: Board
testBoard = map (map unmarked) numbers
  where
    numbers = [[1 .. 5], [6 .. 10], [11 .. 15], [16 .. 20], [21 .. 25]]

markBoard :: Int -> Board -> Board
markBoard = map . map . mark

checkBingo :: Board -> Bool
checkBingo board =
  let concatedBoard = board ++ transpose board
      checkIfBingoed = any (all marked)
   in checkIfBingoed concatedBoard

getUnmarked :: Board -> [Int]
getUnmarked =
  let onlyUnmarked = map $ filter $ not . marked
      getNumbers = map (map number) . onlyUnmarked
   in concat . getNumbers

randomNumbers :: [Int]
randomNumbers = [1, 2, 5, 12, 23, 53, 21, 37, 10, 11, 3, 6]

finalResult :: [Int] -> [Board] -> Int
finalResult [] _ = 0
finalResult (x : xs) boards =
  let newBoards = map (markBoard x) boards
   in case filter checkBingo newBoards of
        [] -> finalResult xs newBoards
        (winningBoard : _) -> x * sum (getUnmarked winningBoard)

done4 f inputNumbers inputBoards = f inputNumbers boards
  where
    boards = map makeBoard inputBoards
    makeBoard = map (map unmarked)

done4part1 = done4 finalResult

finalResult2 :: [Int] -> [Board] -> Int
finalResult2 [] _ = 0
finalResult2 (x : xs) boards =
  let newBoards = map (markBoard x) boards
      notWon = filter (not . checkBingo) newBoards
   in case notWon of
        [lastBoard] -> winLastBoard xs lastBoard
        _ -> finalResult2 xs newBoards

winLastBoard [] _ = 0
winLastBoard (x : xs) board =
  let newBoard = markBoard x board
   in if checkBingo newBoard
        then x * sum (getUnmarked newBoard)
        else winLastBoard xs newBoard

done4part2 = done4 finalResult2
