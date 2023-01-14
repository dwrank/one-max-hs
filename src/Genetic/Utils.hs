module Genetic.Utils
    (
        random_list
    ) where

import System.Random (Random, randomRIO)

{-# INLINE random_list #-}
random_list :: Random a => Int -> (a, a) -> IO [a]
random_list size range =
    sequence $ replicate size $ randomRIO range

