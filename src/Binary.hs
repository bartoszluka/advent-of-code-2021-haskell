module Binary where

data BinaryDigit = Zero | One
  deriving (Eq, Enum)

type BinaryNumber = [BinaryDigit]

instance Show BinaryDigit where
  show Zero = "0"
  show One = "1"

invert :: BinaryDigit -> BinaryDigit
invert Zero = One
invert One = Zero

fromChar :: Char -> Maybe BinaryDigit
fromChar '0' = Just Zero
fromChar '1' = Just One
fromChar _ = Nothing

toInt :: BinaryDigit -> Int
toInt Zero = 0
toInt One = 1

binToDecimal :: BinaryNumber -> Int
binToDecimal = foldl (\acc curr -> (2 * acc) + toInt curr) 0

fromString :: String -> Maybe BinaryNumber
fromString =
  foldr (\curr acc -> fromChar curr >>= addToMaybeList acc) (Just [])
  where
    addToMaybeList (Just list) item = Just (item : list)
    addToMaybeList Nothing _ = Nothing
