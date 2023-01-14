module Genetic.Vector.Unboxed.Mutable
    (
        random_vec
    ) where

import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad, PrimState)
import System.Random (Random, StdGen, randomR)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

{-# INLINE random_vec #-}
random_vec :: (V.Unbox a, Random a, Num a)
           => Int
           -> (a, a)
           -> StdGen
           -> V.Vector a
random_vec size range std_gen = runST $ do
    v <- M.new size
    random_elem v size range std_gen
    V.unsafeFreeze v

{-# INLINE random_elem #-}
random_elem :: (PrimMonad m, V.Unbox a, Random a, Num a)
            => M.MVector (PrimState m) a
            -> Int
            -> (a, a)
            -> StdGen
            -> m()
random_elem _ i _ _ | i < 1 = return ()
random_elem v i range std_gen = do
    let (val, std_gen') = randomR range std_gen
    let i' = i - 1
    M.write v i' val
    random_elem v i' range std_gen'

