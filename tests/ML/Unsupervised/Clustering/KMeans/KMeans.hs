
module Main (
  main
) where

import qualified ML.Unsupervised.Clustering.KMeans as KMeans

import Graphics.Gloss

import System.Random
import Data.Random.Normal
import qualified Data.Vector as V

main = do
  p1 <- mkPoints 10000 0 0 20
  p2 <- mkPoints 10000 40 40 10
  p3 <- mkPoints 10000 (-40) 40 10
  let ps = p1 ++ p2 ++ p3
  let model = KMeans.fit [red, green, blue] $ fmap (\(x, y) -> V.fromList [x , y]) ps
  display (InWindow "KMeans test" (800, 800) (0, 0)) black (mkPicture ps model)
  where
    mkPoints n xmean ymean sigma = do
      xs <- normalsIO' (xmean, sigma)
      ys <- normalsIO' (ymean, sigma)
      return $ take n $ zip xs ys
    mkPicture points km = Pictures $ fmap (\(x, y) -> (color (KMeans.predict km (V.fromList [x, y])) $ translate x y $ circle 2)) points
      

