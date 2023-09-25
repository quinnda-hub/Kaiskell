module Utils.Shuffle (shuffle) where

import System.Random (StdGen, Random (randomR))
import Data.Array.ST (STArray, newListArray, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, writeSTRef, readSTRef)
import Control.Monad (forM)

shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n = newListArray (1,n)