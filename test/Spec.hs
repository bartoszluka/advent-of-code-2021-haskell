{-# LANGUAGE ScopedTypeVariables #-}

import DaySpec (daysTests)
import Extra (between, choose, count, count', createRange, median, selectKth, zipToLonger)
import ExtraSpec (extraTests)
import Relude.Unsafe ((!!))
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (property, (==>))

main :: IO ()
main = hspec $ do
    extraTests
    daysTests