{-# LANGUAGE DeriveGeneric #-}

module ReactiveSolar.Orbit where

import GHC.Generics (Generic)

data OrbitElements = OrbitElements { name :: String,
                                     id :: Int,
                                     epoch :: Double,
                                     ecc :: Double,
                                     distPeri :: Double,
                                     incl :: Double,
                                     longAscNode :: Double,
                                     argPeri :: Double,
                                     timePeri :: Double,
                                     meanMotion :: Double,
                                     meanAnomaly :: Double,
                                     trueAnomaly :: Double,
                                     semiMajorAxis :: Double,
                                     distApo :: Double,
                                     period :: Double
                                   } deriving (Show, Generic)

data OrbitHelioCoords = OrbitHelioCoords Double Double Double

calcRadialDist :: Double -> Double -> Double -> Double
calcRadialDist a e v = (a * (1 - e**2)) / (1+(e * cos v)) 

-- calcHelioCoords :: IO OrbitElements -> Double -> OrbitHelioCoords
-- calcHelioCoords o v = let
--   r = calcRadialDist' o v 
--   omega = longAscNode o
--   w = argPeri o
--   i = incl o
--   x = r * (cos omega * cos(w + v)) - (sin omega * sin(w + v) * cos i)
--   y = r * (sin omega * cos(w + v)) - (cos omega * sin(w + v) * cos i)
--   z = r * sin(w + v) * sin i
--   in OrbitHelioCoords x y z

-- orbTest :: IO ()
-- orbTest = do
--   o <- getOrbitDataFromFile "../data/199mercury.txt"
-- putStrLn show $ map $ calcHelioCoords o [0,10..359]
  
  






