module OneMaxL
    (
      run
    ) where

import System.Random (randomRIO, newStdGen)
import System.Random.Shuffle (shuffle')
import Data.List (sortBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

import qualified Types.Status as Status
import qualified Types.Chromosome as Chrom
import Genetic.Utils (random_list)

type G = [Int]

run :: IO ()
run = do
    let pop_size = 100
    let gen_size = 1000
    pop <- build_pop pop_size gen_size
    let cx_rng = randomRIO(0, gen_size - 1) :: IO Int
    let mut_rng = randomRIO(0, 99) :: IO Int
    loop pop gen_size cx_rng mut_rng 1

build_pop :: Int -> Int -> IO [Chrom.Chromosome G]
build_pop pop_size gen_size = do
    sequence $ [build_chrom gen_size | _ <- [1..pop_size]]

build_chrom :: Int -> IO (Chrom.Chromosome G)
build_chrom size = do
    genes <- random_list size (0, 1)
    return $ Chrom.build genes

loop :: [Chrom.Chromosome G] -> Int -> IO Int -> IO Int -> Int -> IO()
loop pop gen_size cx_rng mut_rng i = do
    let pop' = evaluate pop
    let best = pop' !! 0

    if Chrom._fitness best == 1000 then do
        Status.print_status pop' Status.Done i
        return ()
    else do
        Status.update_status pop' i 100
        let selected_pairs = select pop'
        pop'' <- crossover selected_pairs cx_rng
        new_pop <- mutation pop'' gen_size mut_rng
        loop new_pop gen_size cx_rng mut_rng (i + 1)

evaluate :: [Chrom.Chromosome G] -> [Chrom.Chromosome G]
evaluate pop = do
    let genes_sum = sum . Chrom._genes
    let pop' = map (\chrom -> Chrom.set_fitness chrom $ genes_sum chrom) pop
    sortBy (flip $ comparing $ Chrom._fitness) pop'

select :: [Chrom.Chromosome G] -> [[Chrom.Chromosome G]]
select pop =
    chunksOf 2 pop

crossover :: [[Chrom.Chromosome G]] -> IO Int -> IO [Chrom.Chromosome G]
crossover pairs rng = do
    -- sequence :: [IO [Chrom.Chromosome G]] -> IO [[Chrom.Chromosome G]]
    -- mapM f = sequence . map f
    pairs' <- mapM (\pair -> crossover_pair pair rng) pairs

    -- concat :: [[Chrom.Chromosome G]] -> [Chrom.Chromosome G]
    -- return :: [Chrom.Chromosome G] -> IO [Chrom.Chromosome G]
    return $ concat pairs'

crossover_pair :: [Chrom.Chromosome G] -> IO Int -> IO [Chrom.Chromosome G]
crossover_pair pair rng = do
    cx_pt <- rng
    let chrom1 = pair !! 0
    let chrom2 = pair !! 1
    let (h1, t1) = split_genes chrom1 cx_pt
    let (h2, t2) = split_genes chrom2 cx_pt
    return [Chrom.set_genes chrom1 $ h1 ++ t2, Chrom.set_genes chrom2 $ h2 ++ t1]

split_genes :: Chrom.Chromosome G -> Int -> ([Int], [Int])
split_genes chrom cx_pt = do
    let genes = Chrom._genes chrom
    (take cx_pt genes, drop cx_pt genes)

mutation :: [Chrom.Chromosome G] -> Int -> IO Int -> IO [Chrom.Chromosome G]
mutation pop gen_size rng = do
    mapM (\chrom -> mutate chrom gen_size rng) pop

mutate :: Chrom.Chromosome G -> Int -> IO Int -> IO (Chrom.Chromosome G)
mutate chrom gen_size rng = do
    mut_rate <- rng
    if mut_rate < 5 then do
        std_gen <- newStdGen
        let genes = Chrom._genes chrom
        let genes' = shuffle' genes gen_size std_gen
        let chrom' = Chrom.set_genes chrom genes'
        return chrom'
    else
        return chrom

