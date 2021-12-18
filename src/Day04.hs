module Day04 (part1, part2) where

import Data.List (transpose)

data BingoTile = BingoTile
    { marked :: Bool
    , number :: Int
    }

unmarked :: Int -> BingoTile
unmarked = BingoTile False

markTile :: Int -> BingoTile -> BingoTile
markTile drewNumber tile =
    if number tile == drewNumber
        then BingoTile True drewNumber
        else tile

type Board = [[BingoTile]]

markBoard :: Int -> Board -> Board
markBoard = map . map . markTile

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

finalResult :: [Int] -> [Board] -> Int
finalResult [] _ = 0
finalResult (x : xs) boards =
    let newBoards = map (markBoard x) boards
     in case filter checkBingo newBoards of
            [] -> finalResult xs newBoards
            (winningBoard : _) -> x * sum (getUnmarked winningBoard)

done :: (a -> [Board] -> b) -> a -> [[[Int]]] -> b
done f inputNumbers inputBoards = f inputNumbers boards
  where
    boards = map makeBoard inputBoards
    makeBoard = map (map unmarked)

part1 :: [Int] -> [[[Int]]] -> Int
part1 = done finalResult

finalResult2 :: [Int] -> [Board] -> Int
finalResult2 [] _ = 0
finalResult2 (x : xs) boards =
    let newBoards = map (markBoard x) boards
        notWon = filter (not . checkBingo) newBoards
     in case notWon of
            [lastBoard] -> winLastBoard xs lastBoard
            _ -> finalResult2 xs newBoards

winLastBoard :: [Int] -> Board -> Int
winLastBoard [] _ = 0
winLastBoard (x : xs) board =
    let newBoard = markBoard x board
     in if checkBingo newBoard
            then x * sum (getUnmarked newBoard)
            else winLastBoard xs newBoard

part2 :: [Int] -> [[[Int]]] -> Int
part2 = done finalResult2
