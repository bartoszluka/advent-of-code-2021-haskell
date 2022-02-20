{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|" #-}
import DaySpec (daysTests)
import ExtraSpec (extraTests)
import Test.Hspec (hspec)

main :: IO ()
main =
    hspec $ do
        extraTests
        daysTests