module Prelude (
    module Relude,
    module Flow,
    (.>>),
    (.=>),
) where

import Flow
import Relude

infixl 9 .>>
(.>>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
f .>> g = f .> fmap g

infixl 9 .=>
(.=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f .=> g = f >=> g
