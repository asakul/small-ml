
module ML.Unsupervised.GMM (
  fit,
  FitOptions(..),
  defaultOptions,
  predict
) where

import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V
import Statistics.Distribution
import qualified Data.List as L

import qualified ML.Unsupervised.Clustering.KMeans as KMeans

import Debug.Trace

data GMMComponent a = GMMComponent {
  gmmcPi :: Double,
  gmmcMean :: V.Vector a,
  gmmcSigma :: Matrix a
} deriving (Show)

data GMM a = GMM {
  mixtureComponents :: [GMMComponent a]
} deriving (Show)

normalPdf :: V.Vector Double -> Matrix Double -> V.Vector Double -> Double
normalPdf mu sigma x = (1 / sqrt (2 * pi * det sigma)) * exp (-0.5 * (x - mu) <.> ((inv sigma) #> (x - mu)))

vecsum (v:vs) = foldr (V.zipWith (+)) v vs
vecsum [] = error "Empty list in vecsum"

data FitOptions = FitOptions {
  foMaxIterations :: Int,
  foLikelihoodConvergenceRatio :: Double
}

defaultOptions = FitOptions {
  foMaxIterations = 100,
  foLikelihoodConvergenceRatio = 1.0e-12
}

likelihood :: GMM Double -> [V.Vector Double] -> Double
likelihood gmm points = sum $ fmap (logL gmm) points
  where
    logL gmm point = log $ sum $ fmap (logLFromComponent point) (mixtureComponents gmm)
    logLFromComponent point comp = (gmmcPi comp) * normalPdf (gmmcMean comp) (gmmcSigma comp) point

fit :: FitOptions -> Int -> [V.Vector Double] -> GMM Double
fit options nComponents points = fit' (GMM initialComponents) 0
  where
    fit' gmm itnum = let updatedGmm = update gmm in
                if (abs (likelihood updatedGmm points / likelihood gmm points - 1) < foLikelihoodConvergenceRatio options) || (itnum >= foMaxIterations options)
                  then gmm
                  else fit' updatedGmm (itnum + 1)

    calculateResponsibility gmm comp = zipWith (/) (fmap (respNumerator comp) points) (fmap (respDenominator gmm) points)

    respNumerator component point = (gmmcPi component) * normalPdf (gmmcMean component) (gmmcSigma component) point

    respDenominator gmm point = sum $ fmap (\x -> respNumerator x point) (mixtureComponents gmm)

    update gmm = GMM $ fmap (updateComponent gmm) (mixtureComponents gmm)

    updateComponent gmm comp = comp {gmmcPi = updatePi gmm comp, gmmcMean = updateMu gmm comp, gmmcSigma = updateSigma gmm comp }

    updatePi gmm comp = (sum $ calculateResponsibility gmm comp) / (fromIntegral $ length points)

    updateMu gmm comp = V.map (/ (sum $ calculateResponsibility gmm comp)) (vecsum $ zipWith (\x r -> V.map (* r) x) points (calculateResponsibility gmm comp))
    
    updateSigma gmm comp = (sum $ zipWith (zipR comp) (calculateResponsibility gmm comp) points) / scalar (sum $ calculateResponsibility gmm comp)

    zipR :: GMMComponent Double -> Double -> V.Vector Double -> Matrix Double
    zipR comp x p = scalar x * ((p - gmmcMean comp) `outer` (p - gmmcMean comp))


    dim = V.length $ head points

    initialComponents = let km = KMeans.fit [1..(nComponents)] points
                            predicted = fmap (\p -> (KMeans.predict km p, p)) points in
                          fmap (mkComponent predicted) (KMeans.centroids km)

    mkComponent predicted cent = let filteredPoints = filter (\x -> fst x == fst cent) predicted in
                         GMMComponent { gmmcPi = fromIntegral (length filteredPoints) / fromIntegral (length points),
                                        gmmcMean = snd cent,
                                        gmmcSigma = ident dim * scalar (sqrt ((foldr (\v x -> x + dist (snd cent) (snd v)) 0 filteredPoints) / fromIntegral (length points))) }

    dist :: V.Vector Double -> V.Vector Double -> Double
    dist v1 v2 = (v1 - v2) <.> (v1 - v2)

predict :: GMM Double -> V.Vector Double -> Int
predict gmm point = fst $ L.maximumBy (\x y -> snd x `compare` snd y) $ zip [0..] $ fmap (\comp -> (gmmcPi comp) * normalPdf (gmmcMean comp) (gmmcSigma comp) point) (mixtureComponents gmm)

