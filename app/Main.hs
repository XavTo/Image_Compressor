module Main where

import Utils
import ReadPixel
import WritePixel
import Cluster
import Parser

import System.Exit
import System.Environment
import Text.Read
import Codec.Picture

data Conf = MkConf {
  number :: Int, limit :: Float, path :: String, png :: String}
  deriving Show

myReadImage :: String -> IO (Either String DynamicImage)
myReadImage myPath = readImage myPath

readAndWrite :: Conf -> IO ()
readAndWrite (MkConf n l _ f) = do
    image <- myReadImage f
    let imagePixel = convertRGB8 <$> image
    case imagePixel of
        Left  err -> putStrLn err
        Right img -> writeFile "pixels.txt"
          (writePixelRgb (show (imageData img)) [] (0,1)
          (imageWidth img, imageHeight img))
    myReadPixel "pixels.txt" f n l

classicMethod :: Conf -> IO ()
classicMethod (MkConf n l f _) = do
  str <- myReadFile f
  let tuplePos = parseTuple str []
  let listPixels = parsePixel [] str (allPixel (getSizePixel str)) 0
  let myGroup = Gro {iGroup = 0, groupIndo = (0,0,0)}
  ramdonNumber <- createRandom n listPixels 0
  let clusters = createClusterColor 0 ramdonNumber listPixels []
  imageClustered <- myCalcul (map (addGroup myGroup) listPixels) clusters l n
  displayCluster imageClustered (getGroup imageClustered []) tuplePos 0

checkNumber :: String -> Int
checkNumber b = if (readMaybe b :: Maybe Int) == Nothing then 0 else 1

checkFloat :: String -> Int
checkFloat b = if (readMaybe b :: Maybe Float) == Nothing then 0 else 1

fillConf :: String -> String -> [String] -> Conf -> IO ()
fillConf "-n" b as (MkConf n l f p) =
  if (checkNumber b) == 1 && take 1 b /= "-" && b /= "0"
  then myCheckArgs as (MkConf (n + (read b :: Int)) l f p)
  else putStr "Invalid Args\n" >> exitWith (ExitFailure 84)
fillConf "-l" b as (MkConf n l f p) = if (checkFloat b) == 1 && take 1 b /= "-"
  then myCheckArgs as (MkConf n (l + (read b :: Float)) f p)
  else putStr "Invalid Args\n" >> exitWith (ExitFailure 84)
fillConf "-f" b as (MkConf n l _ p) = myCheckArgs as (MkConf n l b p)
fillConf "-p" b as (MkConf n l f _) = myCheckArgs as (MkConf n l f b)
fillConf _ _ _ _ = putStr "Invalid Args\n" >> exitWith (ExitFailure 84)

myCheckArgs :: [String] -> Conf -> IO ()
myCheckArgs [] (MkConf n l f p) = if f == "" then readAndWrite (MkConf n l f p)
  else classicMethod (MkConf n l f p)
myCheckArgs (a:b:as) (MkConf n l f p) = fillConf a b as (MkConf n l f p)
myCheckArgs _ _ = putStr ""

main :: IO ()
main = do
    let params = MkConf {number = 0, limit = 0.0, path = "", png = ""}
    args <- getArgs
    if length args /= 6
    then putStrLn usage >> exitWith (ExitFailure 84)
    else myCheckArgs args params
