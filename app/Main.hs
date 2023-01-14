module Main (main) where

import System.Environment
import qualified OneMaxL
import qualified OneMaxV

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["l"] -> OneMaxL.run
        ["v"] -> OneMaxV.run
        _     -> print "Invalid arg"
