

module Main (
  main
) where

import qualified ML.Unsupervised.Clustering.KMeans as KMeans

import Graphics.Gloss

import System.Random
import Data.Random.Normal
import qualified Data.Vector as V
import Criterion.Main

main = do
  p1 <- mkPoints 20000 0 0 20
  p2 <- mkPoints 20000 40 40 10
  p3 <- mkPoints 20000 (-40) 40 10
  let ps = p1 ++ p2 ++ p3
  defaultMain [ bgroup "kmeans" [ bench "3clusters/20kpoints each" $ whnf (KMeans.fit [1, 2, 3]) $ fmap (\(x, y) -> V.fromList [x , y]) ps]]
  where
    mkPoints :: Int -> Double -> Double -> Double -> IO [(Double, Double)]
    mkPoints n xmean ymean sigma = do
      xs <- normalsIO' (xmean, sigma)
      ys <- normalsIO' (ymean, sigma)
      return $ take n $ zip xs ys
