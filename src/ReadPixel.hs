module ReadPixel where

import Utils
import Cluster
import ListToImage
import Parser

import Codec.Picture
import Data.Word

getNumberInTuple :: String -> String -> Int
getNumberInTuple [] _ = 0
getNumberInTuple (b:bs) a = if b >= '0' && b <= '9'
    then getNumberInTuple bs (myAppend a [b]) else ((read a::Int) + 1)

rmNumberInTuple :: String -> String
rmNumberInTuple (a:as) = if a == ',' then as
  else rmNumberInTuple as

stringToTuple :: String -> (Int, Int) -> (Int, Int)
stringToTuple [] _ = (0, 0)
stringToTuple a (0, 0) =
    stringToTuple (rmNumberInTuple a) ((getNumberInTuple (skipUseless a) []), 0)
stringToTuple a (x, y) = (x, (getNumberInTuple (skipUseless a) []))

getTupleSizePixel :: Int -> String -> String -> (Int, Int)
getTupleSizePixel size str fill = if (myNth str (size - 1)) == '\n'
    then stringToTuple (myReverse fill) (0, 0)
    else getTupleSizePixel (size - 1) str (fill ++ [myNth str (size - 1)])

getSizePixel :: String -> (Int, Int)
getSizePixel [] = (0, 0)
getSizePixel a = getTupleSizePixel (length a - 1) a []

allPixel :: (Int, Int) -> Int
allPixel (a, b) = a * b

myReadFile :: String -> IO String
myReadFile filename = readFile filename

createHeader :: (Int, Int) -> (Word8, Word8, Word8)
createHeader (x, y) =
    ((fromIntegral x), (fromIntegral y), 0)

myReadPixel :: String -> String -> Int -> Float -> IO ()
myReadPixel path_r path_w n l = do
    str <- myReadFile path_r
    let infoPixel = getSizePixel str
    let tempList = parsePixel [] str (allPixel infoPixel) 0
    let listPixels = myAppend tempList [(createHeader infoPixel)]
    myKmeans listPixels n l infoPixel
