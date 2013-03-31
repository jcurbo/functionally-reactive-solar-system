import System.IO
import Text.Regex.Posix
import Data.Maybe
import Data.List.Split

data OrbitElements = OrbitElements { epoch :: Double,
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
                                   } deriving (Show)
                     
getOrbitDataFromFile :: String -> IO (OrbitElements)
getOrbitDataFromFile filename = do
  h <- openFile filename ReadMode
  maybeOrbData <- readOrbDataFile h
  hClose h
  let o = parseOrbData $ fromJust maybeOrbData
  return (o)

readOrbDataFile :: Handle -> IO (Maybe String)
readOrbDataFile inh =
  do ineof <- hIsEOF inh
     if ineof
        then return Nothing
             else do inpStr <- hGetLine inh
                     if inpStr =~ "^\\$\\$SOE" :: Bool
                       then do orbdata <- hGetLine inh
                               return (Just orbdata)
                       else do readOrbDataFile inh

parseOrbData :: String -> OrbitElements
parseOrbData orbData =
  let (o1,o2) = splitAt 2 (splitOn "," orbData)
      o1f = read (head o1) :: Double
      o2f = map read (init o2) :: [Double]
      [epoch, ecc, distPeri, incl, longAscNode, argPeri, timePeri, meanMotion, meanAnomaly,
       trueAnomaly, semiMajorAxis, distApo, period] = o1f : o2f
  in OrbitElements epoch ecc distPeri incl longAscNode argPeri timePeri meanMotion meanAnomaly trueAnomaly semiMajorAxis distApo period

  

  






