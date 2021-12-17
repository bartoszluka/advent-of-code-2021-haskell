import Day01 (howManyIncreased, howManyIncreased2)
import Day02 (depthTimesTravelled, depthTimesTravelled2)
import Day03 (lifeSupportRating, powerConsumed)
import Day04 (done4part1, done4part2)
import Day05 (done5part1, done5part2)
import Day06 (done6parsing, done6part1, done6part2)
import Day07 (done7part1, done7part2)
import qualified Inputs
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Advent of code 2021 day 1" $ do
    it "part 1" $ do
      howManyIncreased Inputs.day1 `shouldBe` (1602 :: Int)
    it "part 2" $ do
      howManyIncreased2 Inputs.day1 `shouldBe` (1633 :: Int)

  describe "Advent of code 2021 day 2" $ do
    it "part 1" $ do
      depthTimesTravelled Inputs.day2 `shouldBe` (1499229 :: Int)
    it "part 2" $ do
      depthTimesTravelled2 Inputs.day2 `shouldBe` (1340836560 :: Int)

  describe "Advent of code 2021 day 3" $ do
    it "part 1" $ do
      powerConsumed Inputs.day3 `shouldBe` (3985686 :: Int)

    it "part 2" $ do
      lifeSupportRating Inputs.day3 `shouldBe` (2555739 :: Int)

  describe "Advent of code 2021 day 4" $ do
    it "part 1" $ do
      done4part1 Inputs.day4numbers Inputs.day4boards `shouldBe` (39984 :: Int)

    it "part 2" $ do
      done4part2 Inputs.day4numbers Inputs.day4boards `shouldBe` (8468 :: Int)

  describe "Advent of code 2021 day 5" $ do
    it "part 1" $ do
      done5part1 Inputs.day5 `shouldBe` (5145 :: Int)

    it "part 2" $ do
      done5part2 Inputs.day5 `shouldBe` (16518 :: Int)

  describe "Advent of code 2021 day 6" $ do
    it "part 1 from list of ints" $ do
      done6part1 Inputs.day6list `shouldBe` (365862 :: Int)

    it "part 1 from strings" $ do
      done6parsing Inputs.day6 `shouldBe` Just (365862 :: Int)

    it "part 2" $ do
      done6part2 Inputs.day6list `shouldBe` (1653250886439 :: Int)

  describe "Advent of code 2021 day 7" $ do
    it "part 1" $ do
      done7part1 Inputs.day7 `shouldBe` 348664

    it "part 2" $ do
      done7part2 Inputs.day7 `shouldBe` 100220525