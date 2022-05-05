module Parser where

import Data.Word

import Utils

getFisrtPixelSeen :: String -> String -> Word8
getFisrtPixelSeen a [] = read a::Word8
getFisrtPixelSeen a (b:bs) = if b >= '0' && b <= '9'
    then getFisrtPixelSeen (myAppend a [b]) bs
    else read a::Word8

getIndexPixel :: String -> Word8 -> Word8
getIndexPixel a ind = if ind /= 0 then getIndexPixel (skipNumber a) (ind - 1)
    else getFisrtPixelSeen [] a

getOnePixel :: String -> (Word8, Word8, Word8)
getOnePixel a = ((getIndexPixel a 0), (getIndexPixel a 1), (getIndexPixel a 2))

goNextPixel :: String -> String
goNextPixel [] = []
goNextPixel (a:as) = if a == '\n' then as else goNextPixel as

removePos :: String -> String
removePos [] = []
removePos (a:as) = if a == ')' then as else removePos as

parsePixelSplit ::
    [(Word8, Word8, Word8)] -> String -> Int -> Int -> [(Word8, Word8, Word8)]
parsePixelSplit myl a size cmpt =
    parsePixel (myAppend myl [(getOnePixel a)]) (goNextPixel a) size (cmpt + 1)

removeFirstTuple :: String -> String
removeFirstTuple (a:as) = if head as == '('
    then rmFirst as else removeFirstTuple as

parsePixel ::
    [(Word8, Word8, Word8)] -> String -> Int -> Int -> [(Word8, Word8, Word8)]
parsePixel myl [] size cmpt = myl
parsePixel myl a size cmpt = parsePixelSplit myl (removeFirstTuple a) size cmpt

getTuple :: String -> (Int, Int)
getTuple str = ((getNumber [] (skipUseless str)),
    getNumber [] (skipUseless(skipNumber (skipUseless str))))

goNextTuple :: String -> String
goNextTuple [] = []
goNextTuple (a:as) = if a == '\n' then as else goNextTuple as

parseTuple :: String -> [(Int, Int)] -> [(Int, Int)]
parseTuple [] end = end
parseTuple str end = parseTuple (goNextTuple str) (myAppend end [(getTuple str)])
