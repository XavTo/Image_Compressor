module WritePixel where

import Utils

import Codec.Picture

addPos :: (Int, Int) -> (Int, Int) -> [Char]
addPos (a, b) (ma, mb)= if (a == ma)
    then "" else ("(" ++ (show a) ++ "," ++ show b ++ ") ")

putInEnd :: [Int] -> [Char] -> [Char]
putInEnd (a:b:c:_) end = (myAppend end ("(" ++ (show a) ++ "," ++
    (show b) ++ "," ++ (show c) ++ ")\n"))

goNextPixelRgb :: [Char] -> Int -> [Char]
goNextPixelRgb a 0 = a
goNextPixelRgb a nb = goNextPixelRgb (skipNumber (skipUseless a)) (nb - 1)

prepareGetNumber :: [Char] -> Int
prepareGetNumber (a:as) = getNumber [a] as

getRbg :: [Char] -> Int -> [Int] -> [Int]
getRbg [] _ end = end
getRbg _ 0 end = end
getRbg a nb end = getRbg (skipNumber (skipUseless a)) (nb - 1)
    (myAppend end [(prepareGetNumber (skipUseless a))])

writePixelRgb :: [Char] -> [Char] -> (Int, Int) -> (Int, Int) -> [Char]
writePixelRgb [] end compt max = myAppend (addPos (0,0) max) end
writePixelRgb a end compt max = writePixelRgb (goNextPixelRgb a 3)
    (myAppend (putInEnd (getRbg a 3 []) end) (addPos compt max))
    (changePos compt max) max
