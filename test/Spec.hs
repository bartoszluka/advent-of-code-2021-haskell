import Data.List.NonEmpty (NonEmpty)
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import Extra (choose, createRange, readMaybeInt, zipToLonger)
import qualified Inputs
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

main :: IO ()
main = hspec $ do
    describe "Some extra functions" $ do
        describe "Extra.choose" $ do
            it "chooses all values from a list with all Justs" $ do
                property $ \lst -> choose Just lst `shouldBe` (lst :: [Int])
            it "chooses no values from a list full of Nothings" $ do
                choose (const Nothing) [1 .. 10] `shouldBe` ([] :: [Int])

        describe "Extra.readMaybeInt" $ do
            it "converts stringed integers to Maybe Int" $ do
                property $ \i -> readMaybeInt (show i) `shouldBe` (Just i :: Maybe Int)
            it "returns Nothing when the string is not a number" $ do
                readMaybeInt "a1234" `shouldBe` Nothing
                readMaybeInt "a" `shouldBe` Nothing
            it "returns Nothing when the string is empty" $ do
                readMaybeInt "" `shouldBe` Nothing

        describe "Extra.zipToLonger" $ do
            it "zips singleton list and another list" $ do
                property $ \x list -> case list of
                    [] -> () `shouldBe` ()
                    notEmpty@(_ : _) -> zipToLonger [x] notEmpty `shouldBe` (zip (repeat x) notEmpty :: [(Int, Int)])

            it "returns a list of the same length as the longer one" $ do
                let longer = [1 .. 5]
                length (zipToLonger [1] longer) `shouldBe` length longer

        describe "Extra.createRange" $ do
            it "acts as a [a..b] when a <= b" $ do
                property $ \a b ->
                    let (smaller, bigger) = if a <= b then (a, b) else (b, a)
                     in createRange smaller bigger `shouldBe` ([smaller .. bigger] :: [Int])
            it "creates a reversed list when a >= b" $ do
                property $ \a b ->
                    let (smaller, bigger) = if a <= b then (a, b) else (b, a)
                     in createRange bigger smaller `shouldBe` (reverse [smaller .. bigger] :: [Int])

    describe "Final solutions" $ do
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
                Day03.part2 Inputs.day3 `shouldBe` (2555739 :: Int)

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

            -- it "part 1 from strings" $ do
            --     Day06.part2 Inputs.day6 `shouldBe` Just (365862 :: Int)

            it "part 2" $ do
                Day06.part2 Inputs.day6list `shouldBe` (1653250886439 :: Int)

        describe "day 7" $ do
            it "part 1" $ do
                Day07.part1 Inputs.day7 `shouldBe` 348664

            it "part 2" $ do
                Day07.part2 Inputs.day7 `shouldBe` 100220525