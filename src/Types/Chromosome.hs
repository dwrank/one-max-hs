module Types.Chromosome
    (
      Chromosome(..),
      build,
      set_genes,
      set_fitness
    ) where

data Chromosome a = Chromosome {
  _genes :: a,
  _fitness :: Int
} deriving (Show)

{-# INLINE build #-}
build :: a -> Chromosome a
build genes = Chromosome genes 0

{-# INLINE set_genes #-}
set_genes :: Chromosome a -> a -> Chromosome a
set_genes chrom genes = chrom { _genes = genes }

{-# INLINE set_fitness #-}
set_fitness :: Chromosome a -> Int -> Chromosome a
set_fitness chrom fitness = chrom { _fitness = fitness }
