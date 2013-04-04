module ReactiveSolar.Data where

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

import ReactiveSolar.Orbit

dataDir = "../data/"
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
  
