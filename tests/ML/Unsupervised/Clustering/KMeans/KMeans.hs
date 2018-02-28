
module Main (
  main
) where

import qualified ML.Unsupervised.Clustering.KMeans as KMeans

import Graphics.Gloss

import System.Random
import Data.Random.Normal
import qualified Data.Vector.Storable as V

main = do
  p1 <- mkPoints 10000 0 0 20
  p2 <- mkPoints 10000 40 40 10
  p3 <- mkPoints 10000 (-40) 40 10
  let ps = p1 ++ p2 ++ p3
  let model = KMeans.fit [red, green, blue] $ fmap (\(x, y) -> V.fromList [x , y]) ps
  display (InWindow "KMeans test" (800, 800) (0, 0)) black (mkPicture ps model) 

  p1_1d <- mk1dPoints 500 0 20
  p2_1d <- mk1dPoints 500 40 10
  p3_1d <- mk1dPoints 500 (-40) 10
  print p1_1d
  print p2_1d
  print p3_1d
  let ps_1d = p1_1d ++ p2_1d ++ p3_1d 
  let model_1d = KMeans.fit [red, green, blue] $ fmap V.singleton ps_1d
  print model_1d
  display (InWindow "KMeans test" (800, 800) (0, 0)) black (mkPicture1d ps_1d model_1d) 

  where
    mk1dPoints n xmean sigma = do
      xs <- normalsIO' (xmean, sigma)
      return $ take n $ xs

    mkPoints n xmean ymean sigma = do
      xs <- normalsIO' (xmean, sigma)
      ys <- normalsIO' (ymean, sigma)
      return $ take n $ zip xs ys
    mkPicture points km = Pictures $ fmap (\(x, y) -> (color (KMeans.predict km (V.fromList [x, y])) $ translate x y $ circle 2)) points
    mkPicture1d points km = Pictures $ fmap (\x -> (color (KMeans.predict km (V.fromList [x])) $ translate x 100 $ circle 2)) points
      

