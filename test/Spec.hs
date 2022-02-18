{-# LANGUAGE ScopedTypeVariables #-}

import DaySpec (daysTests)
import ExtraSpec (extraTests)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    extraTests
    daysTests