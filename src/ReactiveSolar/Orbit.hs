module ReactiveSolar.Orbit
       (calcRadialDist,
        calcHelioCoords,
        sin_d,
        cos_d
        ) where

import ReactiveSolar.Data

-- orbital formulae checked against http://aa.quae.nl/en/reken/hemelpositie.html

-- a = semimajor axis (AU)
-- e = eccentricity 
-- v = true anomaly (angle between perhelion and current position) (degrees)
calcRadialDist :: Double -> Double -> Double -> Double
calcRadialDist a e v = (a * (1 - e**2)) / (1+(e * cos_d v)) 

-- a = semimajor axis (AU)
-- e = eccentricity (degrees)
-- o = longitude of ascending node (degrees)
-- w = argument of perhelion (degrees)
-- i = inclination (degrees)
-- v = true anomaly (angle between perhelion and current position) (degrees)
calcHelioCoords :: Double -> Double -> Double -> Double -> Double -> Double -> OrbitHelioCoords
calcHelioCoords a e o w i v = let
  r = calcRadialDist a e v
  x = r * (cos_d o * cos_d(w + v) - (sin_d o * sin_d(w + v) * cos_d i))
  y = r * (sin_d o * cos_d(w + v) + (cos_d o * sin_d(w + v) * cos_d i))
  z = r * sin_d(w + v) * sin_d i
  in OrbitHelioCoords x y z 
  
-- trig helper functions
toDeg :: Double -> Double
toDeg rad = rad * 180 / pi

toRad :: Double -> Double
toRad deg = deg * pi / 180

sin_d :: Double -> Double
sin_d deg = sin $ toRad deg

cos_d :: Double -> Double
cos_d deg = cos $ toRad deg


