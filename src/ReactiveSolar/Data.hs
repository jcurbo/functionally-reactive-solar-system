{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReactiveSolar.Data
       (readJsonFile,
        initSolarSystem,
        writeJsonFileFromNASAFiles,
        OrbitElements(..),
        Orbit(..),
        OrbitHelioCoords(..),
        SystemState(..),
        CameraState(..)
        ) where

import System.IO
import Data.List.Split
import System.FilePath.Posix (takeBaseName)
import Text.Regex.PCRE
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad
import Data.List
import System.Directory
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import GHC.Generics (Generic)

-- state information for the entire system
data SystemState = SystemState { camState :: CameraState,
                                 orbits   :: [Orbit]
                               } deriving (Show)

data CameraState = CameraState { tilt :: Double,
                                 rot :: Double,
                                 zoom :: Double
                               } deriving (Show)

data OrbitElements = OrbitElements { name :: String,           -- name of object
                                     id :: Int,                -- ID from HORIZONS
                                     epoch :: Double,          -- Julian day of elements
                                     ecc :: Double,            -- eccentricity of orbit 
                                     distPeri :: Double,       -- distance from Sun at periapsis (AU)
                                     incl :: Double,           -- inclination of orbit (deg)
                                     longAscNode :: Double,    -- longtitude of ascending node (deg)
                                     argPeri :: Double,        -- argument of periapsis (deg)
                                     timePeri :: Double,       -- time of periapsis passage (day)
                                     meanMotion :: Double,     -- motion per day along orbit (deg)
                                     meanAnomaly :: Double,    -- mean anomaly (deg)
                                     trueAnomaly :: Double,    -- true anomaly (deg)
                                     semiMajorAxis :: Double,  -- semi-major axis (AU)
                                     distApo :: Double,        -- distance at apoapsis (AU)
                                     period :: Double          -- orbital period (days)
                                   } deriving (Show, Generic)

-- an Orbit has a set of starting parameters (OrbitElements) as well as
-- a current position in the orbit (curTrueAnomaly)
data Orbit = Orbit { elements :: OrbitElements,
                     curTrueAnomaly :: Double
                   } deriving (Show)

-- 3d cartesian coordinates
data OrbitHelioCoords = OrbitHelioCoords { xCoord :: Double,
                                           yCoord :: Double,
                                           zCoord :: Double
                                         } deriving (Show, Eq)

dataDir :: String
dataDir = "../data/"

dataFile :: String
dataFile = "data.json"

-- won't need this if the generic stuff works, but keep it around for now
--
-- instance FromJSON OrbitElements where
--   parseJSON (Object v) =
--     OrbitElements <$>
--     (v .: T.pack "name")     <*>
--     (v .: T.pack "id")       <*>
--     (v .: T.pack "epoch")    <*>
--     (v .: T.pack "ecc")      <*>
--     (v .: T.pack "distPeri") <*>
--     (v .: T.pack "incl")     <*>
--     (v .: T.pack "longAscNode") <*>
--     (v .: T.pack "argPeri")  <*>
--     (v .: T.pack "timePeri") <*>
--     (v .: T.pack "meanMotion") <*>
--     (v .: T.pack "meanAnomaly") <*>
--     (v .: T.pack "trueAnomaly") <*>
--     (v .: T.pack "semiMajorAxis") <*>
--     (v .: T.pack "distApo")   <*>
--     (v .: T.pack "period")

instance FromJSON OrbitElements

instance ToJSON OrbitElements

getOrbitDataFromNASAFile :: String -> IO OrbitElements
getOrbitDataFromNASAFile filename = do
  h <- openFile filename ReadMode
  orbData <- readOrbDataFile h
  hClose h
  let basename = takeBaseName filename
      [[_,_id,_name]] = basename =~ "(\\d+)(\\w+)" :: [[String]]
      _id' = read _id :: Int
      (o1,o2) = splitAt 2 (splitOn "," orbData)
      o1' = read (head o1) :: Double
      o2' = map read (init o2) :: [Double]
      [_epoch, _ecc, _distPeri, _incl, _longAscNode, _argPeri, _timePeri, _meanMotion, _meanAnomaly,
       _trueAnomaly, _semiMajorAxis, _distApo, _period] = o1' : o2'
      orb = OrbitElements _name _id' _epoch _ecc _distPeri _incl _longAscNode _argPeri _timePeri _meanMotion _meanAnomaly _trueAnomaly _semiMajorAxis _distApo _period
  return orb

readOrbDataFile :: Handle -> IO String
readOrbDataFile inh = do
  inpStr <- hGetLine inh
  if inpStr =~ "^\\$\\$SOE" :: Bool
    then hGetLine inh
    else readOrbDataFile inh

-- scans data directory for NASA HORIZONS files, grabs the data we need, and writes
-- it out as a JSON file
writeJsonFileFromNASAFiles :: IO ()
writeJsonFileFromNASAFiles = do
  files <- liftM (filter (isSuffixOf ".txt")) (getDirectoryContents dataDir)
  dataAll <- mapM (getOrbitDataFromNASAFile . (dataDir ++)) files
  let jsonStr = encodePretty dataAll
  L.writeFile (dataDir ++ "data.json") jsonStr
  return ()

readJsonFile :: IO [OrbitElements]
readJsonFile = do
  dataFileContents <- L.readFile (dataDir ++ dataFile)
  let o = decode dataFileContents :: Maybe [OrbitElements]
      j = fromJust o
  return j
  
initSolarSystem :: IO [Orbit]
initSolarSystem = do
  d <- readJsonFile
  mapM (\x -> return (Orbit x (trueAnomaly x))) d
  

       
