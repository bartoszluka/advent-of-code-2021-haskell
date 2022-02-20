{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stack where

import Text.Show (Show (show))
import Prelude hiding (show)

newtype Stack a = Stack [a]
    deriving (Show, Eq, Foldable, Semigroup, Monoid)

push :: a -> Stack a -> Stack a
push item (Stack list) = Stack <| item : list

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack list) = uncons list |> fmap (mapSnd Stack)
  where
    mapSnd f (x, y) = (x, f y)

peek :: Stack a -> Maybe a
peek (Stack list) = list |> viaNonEmpty head

type StackOrError a = Either (PoppedError a) (Stack a)

data PoppedError a
    = PoppedEmpty
    | PoppedWrongItem a a

instance Show a => Show (PoppedError a) where
    show PoppedEmpty = "popped an empty stack"
    show (PoppedWrongItem this that) = "tried to pop " <> show this <> " but can only pop " <> show that

popIfEquals :: Eq a => a -> Stack a -> StackOrError a
popIfEquals item stack = case pop stack of
    Nothing -> Left PoppedEmpty
    Just (popped, rest) ->
        if item == popped
            then Right rest
            else Left <| PoppedWrongItem item popped
