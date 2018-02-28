
module Main (
  main
) where

import qualified ML.Unsupervised.GMM as GMM

import Graphics.Gloss

import System.Random
import Data.Random.Normal
import qualified Data.Vector.Storable as V

main = do
  p1 <- mkPoints 500 0 0 20
  p2 <- mkPoints 500 40 40 10
  p3 <- mkPoints 500 (-40) 40 10
  let ps = p1 ++ p2 ++ p3
  let model = GMM.fit GMM.defaultOptions 3 $ fmap (\(x, y) -> V.fromList [x , y]) ps
  display (InWindow "GMM test" (800, 800) (0, 0)) black (mkPicture ps model) 

  where
    mkPoints n xmean ymean sigma = do
      xs <- normalsIO' (xmean, sigma)
      ys <- normalsIO' (ymean, sigma)
      return $ take n $ zip xs ys
    mkPicture points km = Pictures $ fmap (\(x, y) -> (color ([red, blue, green] !! (GMM.predict km (V.fromList [x, y]))) $ translate (realToFrac x) (realToFrac y) $ circle 2)) points
      

