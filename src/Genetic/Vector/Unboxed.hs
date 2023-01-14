module Genetic.Vector.Unboxed
    (
        evaluate,
        select,
        crossover,
        mutation
    ) where

import System.Random (newStdGen, StdGen, randomRs)
import Data.Bits (zeroBits, testBit, (.|.), bit)
import Data.List.Split (chunksOf)
import Data.List (sortBy)
import Data.Ord (comparing)
--import System.Random.Shuffle (shuffle')

import qualified Data.Vector.Unboxed as V

import qualified Types.Chromosome as Chrom

{-# INLINE evaluate #-}
evaluate :: [Chrom.Chromosome (V.Vector a)] -> (V.Vector a -> Int) -> [Chrom.Chromosome (V.Vector a)]
evaluate pop eval_fn = do
    let pop' = map (\chrom -> Chrom.set_fitness chrom $ eval_fn $ Chrom._genes chrom) pop
    sortBy (flip $ comparing $ Chrom._fitness) pop'

{-# INLINE select #-}
select :: Int -> [Chrom.Chromosome (V.Vector a)] -> [[Chrom.Chromosome (V.Vector a)]]
select count pop =
    chunksOf count pop

{-# INLINE crossover #-}
crossover :: (V.Unbox a) => IO Int -> [[Chrom.Chromosome (V.Vector a)]] -> IO [Chrom.Chromosome (V.Vector a)]
crossover rng pairs = do
    -- sequence :: [IO [Chrom.Chromosome G]] -> IO [[Chrom.Chromosome G]]
    -- mapM f = sequence . map f
    -- concat <$> :: IO [[Chrom.Chromosome G]] -> IO [Chrom.Chromosome G]
    concat <$> mapM (crossover_pair rng) pairs

{-# INLINE crossover_pair #-}
crossover_pair :: (V.Unbox a) => IO Int -> [Chrom.Chromosome (V.Vector a)] -> IO [Chrom.Chromosome (V.Vector a)]
crossover_pair rng pair = do
    cx_pt <- rng
    let chrom1 = pair !! 0
    let chrom2 = pair !! 1
    let (h1, t1) = split_genes chrom1 cx_pt
    let (h2, t2) = split_genes chrom2 cx_pt
    return [Chrom.set_genes chrom1 $ h1 V.++ t2, Chrom.set_genes chrom2 $ h2 V.++ t1]

{-# INLINE split_genes #-}
split_genes :: (V.Unbox a) => Chrom.Chromosome (V.Vector a) -> Int -> (V.Vector a, V.Vector a)
split_genes chrom cx_pt = do
    let genes = Chrom._genes chrom
    (V.take cx_pt genes, V.drop cx_pt genes)

{-# INLINE mutation #-}
mutation :: (V.Unbox a, Eq a) => IO Int -> [Chrom.Chromosome (V.Vector a)] -> IO [Chrom.Chromosome (V.Vector a)]
mutation rng pop = do
    mapM (mutate rng) pop

{-# INLINE mutate #-}
mutate :: (V.Unbox a, Eq a) => IO Int -> Chrom.Chromosome (V.Vector a) -> IO (Chrom.Chromosome (V.Vector a))
mutate rng chrom = do
    mut_rate <- rng
    if mut_rate < 5 then do
        let genes = Chrom._genes chrom
        --genes' <- V.fromList $ shuffle' (V.toList genes) (V.length genes) <$> newStdGen
        genes' <- shuffle genes <$> newStdGen
        let chrom' = Chrom.set_genes chrom genes'
        return chrom'
    else
        return chrom

{-# INLINE shuffle #-}
shuffle :: (V.Unbox a, Eq a) => V.Vector a -> StdGen -> V.Vector a
shuffle v std_gen = do
    let size = V.length v
    let seq_pos_list = [0..size-1]
    let rand_pos_list = take size (randomRs (0, size - 1) std_gen)
    let pos_pairs = zip seq_pos_list rand_pos_list
    let bitmap = zeroBits
    -- new_vals :: [(index, val)]
    let new_vals = build_index_val_pairs v bitmap pos_pairs []
    -- update with the (index, val) pairs
    v V.// new_vals

{-# INLINE build_index_val_pairs #-}
build_index_val_pairs :: (V.Unbox a, Eq a) => V.Vector a -> Integer -> [(Int, Int)] -> [(Int, a)] -> [(Int, a)]
build_index_val_pairs _v _bitmap [] acc = acc
build_index_val_pairs v bitmap ((i_seq, i_rand) : pos_pairs_tl) acc
    -- skip if the values are the same or an index has been masked
    | (v V.! i_seq) == (v V.! i_rand)
    || testBit bitmap i_seq
    || testBit bitmap i_rand =
        build_index_val_pairs v bitmap pos_pairs_tl acc
    -- otherwise mask the indices and swap the values
    | otherwise = do
        let bitmap' = bitmap .|. (bit i_seq) .|. (bit i_rand)
        let acc' = (i_seq, v V.! i_rand) : (i_rand, v V.! i_seq) : acc
        build_index_val_pairs v bitmap' pos_pairs_tl acc'

