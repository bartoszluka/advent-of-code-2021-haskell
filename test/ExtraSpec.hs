import Extra (between, choose, count, count', createRange, median, selectKth, zipToLonger)
import Relude.Unsafe ((!!))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (property, (==>))

extraTests :: HasCallStack => SpecWith ()
extraTests = describe "Some extra functions" $ do
    describe "Extra.choose" $ do
        it "chooses all values from a list with all Justs" $ do
            property $ \(list :: [Int]) -> choose Just list `shouldBe` list
        it "chooses no values from a list full of Nothings" $ do
            choose (const Nothing) ([1 .. 10] :: [Int]) `shouldBe` ([] :: [Int])

    describe "Extra.zipToLonger" $ do
        it "zips singleton list and another list" $ do
            property $ \(x :: Int) (list :: [Int]) ->
                not (null list)
                    ==> zipToLonger [x] list `shouldBe` zip (repeat x) list

        it "returns a list of the same length as the longer one" $ do
            let (longer :: [Int]) = [1 .. 5]
            length (zipToLonger ([1] :: [Int]) longer) `shouldBe` length longer

    describe "Extra.createRange" $ do
        it "acts as a [a..b] when a <= b" $ do
            property $ \a b ->
                let (smaller, bigger) = if a <= b then (a, b) else (b, a)
                 in createRange smaller bigger `shouldBe` ([smaller .. bigger] :: [Int])
        it "creates a reversed list when a >= b" $ do
            property $ \a b ->
                let (smaller, bigger) = if a <= b then (a, b) else (b, a)
                 in createRange bigger smaller `shouldBe` (reverse [smaller .. bigger] :: [Int])

    describe "Extra.selectKth" $ do
        it "gets kth element from a list as if the list was sorted" $ do
            property $ \k (list :: [Int]) ->
                k `between` (1, length list) && not (null list)
                    ==> selectKth k list
                        `shouldBe` sort list !! max 0 (k - 1)

    describe "Extra.median" $ do
        it "returns the median of a list (if the length is even then it returns the first of the 2 center elements)" $ do
            property $ \(list :: [Int]) ->
                not (null list)
                    ==> median list `shouldBe` sort list !! ceiling ((genericLength list / 2) -1 :: Double)

    describe "Extra.count" $ do
        it "returns the length if the predicate is always true (const True)" $ do
            property $ \(list :: [Int]) ->
                count (const True) list `shouldBe` length list

        it "acts as a combination of `lenght` and `filter`" $ do
            property $ \(list :: [Int]) ->
                count even list `shouldBe` count' even list
