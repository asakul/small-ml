
module Main (
  main
) where

import qualified ML.Unsupervised.GMM as GMM

import System.Random
import Data.Random.Normal
import qualified Data.Vector.Storable as V
import Criterion.Main

main = do
  p1 <- mkPoints 200 0 0 20
  p2 <- mkPoints 200 40 40 10
  p3 <- mkPoints 200 (-40) 40 10
  let ps = p1 ++ p2 ++ p3
  defaultMain [ bgroup "gmm" [ bench "3clusters/200 points each" $ whnf (GMM.fit GMM.defaultOptions 3) $ fmap (\(x, y) -> V.fromList [x , y]) ps]]
  where
    mkPoints :: Int -> Double -> Double -> Double -> IO [(Double, Double)]
    mkPoints n xmean ymean sigma = do
      xs <- normalsIO' (xmean, sigma)
      ys <- normalsIO' (ymean, sigma)
      return $ take n $ zip xs ys

