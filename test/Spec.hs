import Day01 (howManyIncreased, howManyIncreased2)
import Day02 (depthTimesTravelled, depthTimesTravelled2)
import Day03 (lifeSupportRating, powerConsumed)
import Day04 (done4part1, done4part2)
import Day05 (done5part1, done5part2, getPoints)
import Day06 (done6parsing, done6part1, done6part2)
import Day07 (done7part1, done7part2)
import Extra (choose, readMaybeInt)
import qualified Inputs
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

main :: IO ()
main = hspec $ do
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

  describe "Day05.getPoints" $ do
    it "creates a horizontal line when the 'x' values are the same" $ do
      property $ \x y1 y2 ->
        let (smaller, bigger) = if y1 < y2 then (y1, y2) else (y2, y1)
         in getPoints ((x, smaller), (x, bigger)) `shouldBe` zip (repeat x) [smaller .. bigger]
    it "creates a vertical line when the 'y' values are the same" $ do
      property $ \y x1 x2 ->
        let (smaller, bigger) = if x1 < x2 then (x1, x2) else (x2, x1)
         in getPoints ((smaller, y), (bigger, y)) `shouldBe` zip [smaller .. bigger] (repeat y)

  describe "Final solutions" $ do
    describe "day 1" $ do
      it "part 1" $ do
        howManyIncreased Inputs.day1 `shouldBe` (1602 :: Int)
      it "part 2" $ do
        howManyIncreased2 Inputs.day1 `shouldBe` (1633 :: Int)

    describe "day 2" $ do
      it "part 1" $ do
        depthTimesTravelled Inputs.day2 `shouldBe` (1499229 :: Int)
      it "part 2" $ do
        depthTimesTravelled2 Inputs.day2 `shouldBe` (1340836560 :: Int)

    describe "day 3" $ do
      it "part 1" $ do
        powerConsumed Inputs.day3 `shouldBe` (3985686 :: Int)

      it "part 2" $ do
        lifeSupportRating Inputs.day3 `shouldBe` (2555739 :: Int)

    describe "day 4" $ do
      it "part 1" $ do
        done4part1 Inputs.day4numbers Inputs.day4boards `shouldBe` (39984 :: Int)

      it "part 2" $ do
        done4part2 Inputs.day4numbers Inputs.day4boards `shouldBe` (8468 :: Int)

    describe "day 5" $ do
      it "part 1" $ do
        done5part1 Inputs.day5 `shouldBe` (5145 :: Int)

      it "part 2" $ do
        done5part2 Inputs.day5 `shouldBe` (16518 :: Int)

    describe "day 6" $ do
      it "part 1 from list of ints" $ do
        done6part1 Inputs.day6list `shouldBe` (365862 :: Int)

      it "part 1 from strings" $ do
        done6parsing Inputs.day6 `shouldBe` Just (365862 :: Int)

      it "part 2" $ do
        done6part2 Inputs.day6list `shouldBe` (1653250886439 :: Int)

    describe "day 7" $ do
      it "part 1" $ do
        done7part1 Inputs.day7 `shouldBe` 348664

      it "part 2" $ do
        done7part2 Inputs.day7 `shouldBe` 100220525