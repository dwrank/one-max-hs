module Types.Status
    (
      State(..),
      Status(..),
      create_status,
      update_status,
      print_status
    ) where

import qualified Types.Chromosome as Chrom

data State = Running | Done
  deriving (Eq, Show)

data Status a = Status {
  best :: Chrom.Chromosome a,
  state :: State,
  iteration :: Int
} deriving (Show)

{-# INLINE create_status #-}
create_status :: Chrom.Chromosome a -> State -> Int -> Status a
create_status chrom state_ iteration_ =
    Status chrom state_ iteration_

{-# INLINE update_status #-}
update_status :: Show a => [Chrom.Chromosome a] -> Int -> Int -> IO ()
update_status pop i interval
    | i `mod` interval == 0 = do
        print_status pop Running i
    | otherwise = return ()

{-# INLINE print_status #-}
print_status :: Show a => [Chrom.Chromosome a] -> State -> Int -> IO ()
print_status pop state_ i = do
    let best_ = (pop !! 0)
    print $ create_status best_ state_ i
