{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|" #-}
module DaySpec where

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