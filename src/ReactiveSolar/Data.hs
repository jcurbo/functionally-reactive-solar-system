{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReactiveSolar.Data
       (readJsonFile,
        --initSolarSystem,
        writeJsonFileFromNASAFiles,
        OrbitElements(..),
        Orbit(..),
        OrbitHelioCoords(..),
        SystemState(..),
        CameraState(..),
        initState,
        getTilt,
        getRot,
        getZoom,
        updateTrueAnomaly,
        getScale,
        updateState,
        timeloop
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
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Text.Printf

-- state information for the entire system
data SystemState = SystemState { camState :: CameraState,
                                 orbits   :: [Orbit],
                                 scalefac :: Int,
                                 delayTime :: Int
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
  
-- initSolarSystem :: IO [Orbit]
-- initSolarSystem = do
--   d <- readJsonFile
--   mapM (\x -> return (Orbit x (trueAnomaly x))) d
  
-- updateTrueAnomalyDaily :: SystemState -> SystemState
-- updateTrueAnomalyDaily sysState = let
--   n = map (\x -> Orbit (elements x) ((curTrueAnomaly x) + (meanMotion $ elements x))) $ orbits sysState
--   in SystemState (camState sysState) n


-- delay is in milliseconds; e.g. a delay of (24 * 60 * 60 * 1000) would set fac to 1, and would
-- advance the true anomaly by the value of meanMotion (which is in degrees/day)
updateTrueAnomaly :: SystemState -> SystemState
updateTrueAnomaly sysState = let
  -- number of milliseconds in a day divided by the delay tells us how much to scale meanMotion
  fac = (24 * 60 * 60 * 1000) `div` (delayTime sysState)
  n = map (\x -> Orbit
                 (elements x)
                 (curTrueAnomaly x + (fromIntegral (scalefac sysState) * (meanMotion (elements x) / fromIntegral fac)))
          ) $ orbits sysState
  in SystemState (camState sysState) n (scalefac sysState) (delayTime sysState)

initState :: IO (IORef SystemState)
initState = do
  d <- readJsonFile
  ssData <- mapM (\x -> return (Orbit x (trueAnomaly x))) d
  -- default values for the state
      -- camera at 0, 0, -150
  let cam = CameraState 0 0 (-150.0)
      -- scale at real-time
      scaleV = 1
      -- update delay at 100 ms
      delayT = 100
      s = SystemState cam ssData scaleV delayT
  newIORef s

getTilt :: IORef SystemState -> IO Double
getTilt sysState = do
  s <- readIORef sysState
  let t = tilt $ camState s
  return t

getRot :: IORef SystemState -> IO Double
getRot sysState = do
  s <- readIORef sysState
  let t = rot $ camState s
  return t

getZoom :: IORef SystemState -> IO Double
getZoom sysState = do
  s <- readIORef sysState
  let t = zoom $ camState s
  return t

getScale :: IORef SystemState -> IO Int
getScale sysState = do
  s <- readIORef sysState
  let t = scalefac s
  return t

-- update the system state every 'delay' milliseconds
updateState :: IORef SystemState -> IO ()
updateState sysState = do
  modifyIORef sysState updateTrueAnomaly
--  threadDelay (delay * 1000)
  r <- readIORef sysState
  let v = curTrueAnomaly $ last $ orbits r
  printf "%f\n" v
--  updateState sysState delay


-- updateStateM :: IORef SystemState -> Int -> MVar DoOrDie -> IO ()
-- updateStateM sysState delay v = run where
--   run = do
--     putMVar v Do
--     modifyIORef sysState (\x -> updateTrueAnomaly x delay)
--     threadDelay (delay * 1000)
--     r <- readIORef sysState
--     let a = curTrueAnomaly $ last $ orbits r
--     printf "%f\n" a

--     m <- takeMVar v
--     case m of
--       Do -> updateStateM sysState delay v
--       Die -> return ()

data DoOrDie = Do | Die

timeloop :: TMVar DoOrDie -> TMVar DoOrDie -> Int -> IO ()
timeloop tick die delay = run where
  run = do
    i <- forkIO (threadDelay (1000 * delay) >> atomically (putTMVar tick Do))
    r <- atomically (takeTMVar tick `orElse` takeTMVar die)
    case r of
      Do -> run
      Die -> killThread i
      
