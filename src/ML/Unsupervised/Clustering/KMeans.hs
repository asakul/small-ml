
module ML.Unsupervised.Clustering.KMeans (
  fit,
  predict
) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

import Data.Ratio ((%))
import Data.List (minimumBy)
import qualified Data.Vector as V

import Debug.Trace

data KMeans a l = KMeans {
  centroids :: [(l, V.Vector a)]
} deriving (Show, Eq)

fit :: (Fractional a, Ord a, Eq l, Show l, Show a) => [l] -> [V.Vector a] -> KMeans a l
fit labels points = fit' (KMeans initialCentroids)
  where
    fit' kmeans = let
                    ass = assignment kmeans
                    updatedKmeans = update kmeans ass in
                    if assignment updatedKmeans == assKMeans
                      then updatedKmeans
                      else fit' updatedKmeans
    update kmeans ass = KMeans $ zip labels $ fmap (\l -> centerOfMass $ snd <$> filter (\x -> fst x == l) ass) labels
    assignment kmeans = fmap (\x -> (assign kmeans x, x)) points
    assign kmeans point = fst $ minimumBy (\x1 x2 -> snd x1 `compare` snd x2) $ (fmap (\(l, cent) -> (l, distance cent point))) $ centroids kmeans
    distance v1 v2 = V.sum $ V.zipWith (\x1 x2 -> (x2 - x1) * (x2 - x1)) v1 v2
    initialCentroids = zip labels points

    centerOfMass :: (Fractional a) => [V.Vector a] -> V.Vector a
    centerOfMass (p:pts) = fmap (/ (fromRational $ (toInteger $ length pts + 1) % 1)) $ foldr sumVectors p pts

    dim = V.length $ head points
    sumVectors :: (Num a) => V.Vector a -> V.Vector a -> V.Vector a
    sumVectors = V.zipWith (+)

predict :: (Ord a, Num a) => KMeans a l -> V.Vector a -> l
predict kmeans point = fst $ minimumBy (\x1 x2 -> snd x1 `compare` snd x2) $ (fmap (\(l, cent) -> (l, distance cent point))) $ centroids kmeans
  where
    distance v1 v2 = V.sum $ V.zipWith (\x1 x2 -> (x2 - x1) * (x2 - x1)) v1 v2

fitMatrix :: (Element a) => [l] -> Matrix a -> KMeans a l
fitMatrix = undefined

