{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|" #-}
module DaySpec where

import qualified Data.Set as Set
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Inputs
import Test.Hspec (SpecWith, describe, it, shouldBe)

daysTests :: HasCallStack => SpecWith ()
daysTests = describe "Solutions to each day" $ do
    describe "day 1" $ do
        it "part 1" $ do
            Day01.part1 Inputs.day1 `shouldBe` (1602 :: Int)
        it "part 2" $ do
            Day01.part2 Inputs.day1 `shouldBe` (1633 :: Int)

    describe "day 2" $ do
        it "part 1" $ do
            Day02.part1 Inputs.day2 `shouldBe` (1499229 :: Int)
        it "part 2" $ do
            Day02.part2 Inputs.day2 `shouldBe` (1340836560 :: Int)

    describe "day 3" $ do
        it "part 1" $ do
            Day03.part1 Inputs.day3 `shouldBe` (3985686 :: Int)

        it "part 2" $ do
            Day03.part2 Inputs.day3 `shouldBe` Just (2555739 :: Int)

    describe "day 4" $ do
        it "part 1" $ do
            Day04.part1 Inputs.day4numbers Inputs.day4boards `shouldBe` (39984 :: Int)

        it "part 2" $ do
            Day04.part2 Inputs.day4numbers Inputs.day4boards `shouldBe` (8468 :: Int)

    describe "day 5" $ do
        it "part 1" $ do
            Day05.part1 Inputs.day5 `shouldBe` (5145 :: Int)

        it "part 2" $ do
            Day05.part2 Inputs.day5 `shouldBe` (16518 :: Int)

    describe "day 6" $ do
        it "part 1 from list of ints" $ do
            Day06.part1 Inputs.day6list `shouldBe` (365862 :: Int)

        it "part 1 from strings" $ do
            Day06.part1parsing Inputs.day6 `shouldBe` Just (365862 :: Int)

        it "part 2" $ do
            Day06.part2 Inputs.day6list `shouldBe` (1653250886439 :: Int)

    describe "day 7" $ do
        it "part 1" $ do
            Day07.part1 Inputs.day7 `shouldBe` 348664

        it "part 2" $ do
            Day07.part2 Inputs.day7 `shouldBe` 100220525
    describe "day 8" $ do
        it "part 1" $ do
            Day08.part1 Inputs.day8 `shouldBe` Just 390

        it "part 2" $ do
            Day08.part2 Inputs.day8 `shouldBe` Just 1011785

    describe "day 9" $ do
        it "part 1" $ do
            Day09.part1 Inputs.day9 `shouldBe` Just 585

        it "part 2" $ do
            Day09.part2 Inputs.day9 `shouldBe` Just 827904

    describe "day 10" $ do
        it "part 1" $ do
            Day10.part1 Inputs.day10 `shouldBe` 362271

        it "part 2" $ do
            Day10.part2 Inputs.day10 `shouldBe` 1698395182

    describe "day 11" $ do
        it "part 1" $ do
            Day11.part1 Inputs.day11 `shouldBe` Just 1735

        it "part 2" $ do
            Day11.part2 Inputs.day11 `shouldBe` Just 400

    describe "day 12" $ do
        it "part 1" $ do
            Day12.part1 Inputs.day12 `shouldBe` Just 3563
        it "part 2" $ do
            Day12.part2 Inputs.day12 `shouldBe` Just 105453

    describe "day 13" $ do
        it "part 1" $ do
            Day13.part1 Inputs.day13 `shouldBe` Just 745
        it "part 2" $ do
            Set.toAscList <$> Day13.part2 Inputs.day13 `shouldBe` Just [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (1, 0), (1, 3), (2, 0), (2, 3), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (6, 0), (6, 2), (6, 5), (7, 0), (7, 2), (7, 5), (8, 1), (8, 3), (8, 4), (10, 0), (10, 1), (10, 2), (10, 3), (10, 4), (10, 5), (11, 2), (12, 1), (12, 3), (12, 4), (13, 0), (13, 5), (15, 4), (16, 5), (17, 0), (17, 5), (18, 0), (18, 1), (18, 2), (18, 3), (18, 4), (20, 0), (20, 1), (20, 2), (20, 3), (20, 4), (20, 5), (21, 0), (21, 2), (22, 0), (22, 2), (23, 0), (25, 0), (25, 1), (25, 2), (25, 3), (25, 4), (25, 5), (26, 0), (26, 2), (26, 5), (27, 0), (27, 2), (27, 5), (28, 1), (28, 3), (28, 4), (30, 1), (30, 2), (30, 3), (30, 4), (31, 0), (31, 5), (32, 0), (32, 3), (32, 5), (33, 1), (33, 3), (33, 4), (33, 5), (35, 1), (35, 2), (35, 3), (35, 4), (36, 0), (36, 5), (37, 0), (37, 5), (38, 1), (38, 4)]

    describe "day 14" $ do
        it "part 1" $ do
            Day14.part1 Inputs.day14 `shouldBe` Just 3058
        it "part 2" $ do
            Day14.part2 Inputs.day14 `shouldBe` Just 3447389044530
