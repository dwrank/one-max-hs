module OneMaxV
    (
        run
    ) where

import System.Random (newStdGen, randomRIO)

--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V

import qualified Types.Status as Status
import qualified Types.Chromosome as Chrom
import Genetic.Utils (random_list)
import Genetic.Vector.Unboxed (evaluate, select, crossover, mutation)
import Genetic.Vector.Unboxed.Mutable (random_vec)

type G = V.Vector Int

run :: IO ()
run = do
    let pop_size = 100
    let gen_size = 1000
    pop <- build_pop pop_size gen_size
    let cx_rng = randomRIO(0, gen_size - 1) :: IO Int
    let mut_rng = randomRIO(0, 99) :: IO Int
    loop pop cx_rng mut_rng 1

build_pop :: Int -> Int -> IO [Chrom.Chromosome G]
build_pop pop_size gen_size = do
    -- sequence :: [IO Chrom.Chromosome G] -> IO [Chrom.Chromosome G]
    sequence [build_chrom gen_size | _ <- [1..pop_size]]

build_chrom' :: Int -> IO (Chrom.Chromosome G)
build_chrom' size = do
    genes <- V.fromList <$> random_list size (0, 1)
    return $ Chrom.build genes

build_chrom :: Int -> IO (Chrom.Chromosome G)
build_chrom size = do
    genes <- random_vec size (0,1) <$> newStdGen
    return $ Chrom.build genes

loop :: [Chrom.Chromosome G] -> IO Int -> IO Int -> Int -> IO()
loop pop cx_rng mut_rng i = do
    let pop' = evaluate pop V.sum
    let best = pop' !! 0

    if Chrom._fitness best == 1000 then do
        Status.print_status pop' Status.Done i
        return ()
    else do
        Status.update_status pop' i 100
        let selected_pairs = select 2 pop'
        pop'' <- crossover cx_rng selected_pairs
        new_pop <- mutation mut_rng pop''
        loop new_pop cx_rng mut_rng (i + 1)

