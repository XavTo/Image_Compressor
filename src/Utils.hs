module Utils where

import Data.Word
import Codec.Picture

data Group = Gro {
  iGroup :: Int,
  groupIndo :: (Word8, Word8, Word8)
} deriving (Eq, Show)

myNth :: [a] -> Int -> a
myNth [] _ = error "index too big"
myNth (a:xs) x = if x /= 0 then myNth xs (x - 1) else a

myAppend :: [a] -> [a] -> [a]
myAppend [] b = b
myAppend (a:as) b = a: myAppend as b

myHead :: [a] -> a
myHead (a:_) = a
myHead [] = error "empty list"

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

getFirstMoy :: [(Word8, Word8, Word8)] -> Int -> Float -> Word8
getFirstMoy ((a, _, _):[]) size end =
    fromIntegral (toInteger
    (round ((end + (myFloat a)) / (fromIntegral (toInteger size) :: Float)))) :: Word8
getFirstMoy ((a, _, _):as) size end = getFirstMoy as size (end + (myFloat a))

getSecondMoy :: [(Word8, Word8, Word8)] -> Int -> Float -> Word8
getSecondMoy ((_, a, _):[]) size end =
    fromIntegral (toInteger
    (round ((end + (myFloat a)) / (fromIntegral (toInteger size) :: Float)))) :: Word8
getSecondMoy ((_, a, _):as) size end = getSecondMoy as size (end + (myFloat a))

getThirdMoy :: [(Word8, Word8, Word8)] -> Int -> Float -> Word8
getThirdMoy ((_, _, a):[]) size end =
    fromIntegral (toInteger
    (round ((end + (myFloat a)) / (fromIntegral (toInteger size) :: Float)))) :: Word8
getThirdMoy ((_, _, a):as) size end = getThirdMoy as size (end + (myFloat a))

rmFirst :: String -> String
rmFirst [] = []
rmFirst (a:as) = as

myRemoveLast :: [a] -> [a]
myRemoveLast [] = error "Empty list!"
myRemoveLast [h] = []
myRemoveLast (h:t) = [h] ++ myRemoveLast t

myRemoveFirst :: [a] -> [a]
myRemoveFirst [] = error "Empty list!"
myRemoveFirst [h] = []
myRemoveFirst (h:t) = t

myTake :: Int -> [a] -> [a]
myTake x [] = if (x >= 0) then [] else error "error"
myTake x (a:xs) = if x /= 1 then a : myTake (x - 1) xs else a : []

myLast :: [a] -> a
myLast [] = error "empty list"
myLast a = myNth a (length a - 1)

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit a = if length a == 1 then [] else myTake (length a - 1) a

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x = myLast x : myReverse (myInit x)

myIsInGroup :: Group -> [Group] -> Bool
myIsInGroup a [] = False
myIsInGroup (Gro id (a, b, c)) ((Gro idx (ax, bx, cx)):xs) = if id == idx then
    True else myIsInGroup (Gro id (a, b, c)) xs

isElement :: Eq a => a -> [a] -> Bool
isElement a [] = False
isElement a (x:xs)
  | a == x = True
  | otherwise = isElement a xs

usage :: String 
usage = "USAGE: ./imageCompressor -n N -l L -f F\n\tNnumber of colors in the \
    \final image\n\tLconvergence limit\n\tFpath to the file containing the \
    \colors of the pixels"

displayTripleTuple :: (Word8, Word8, Word8) -> IO ()
displayTripleTuple (a, b, c) = putStr ("(" ++ (show a)) >> putStr "," >>
    putStr (show b) >> putStr "," >> putStr (show c) >> putStr ")\n"

displayPos :: (Int, Int) -> IO ()
displayPos (x, y) = putStr ("(" ++ show x ++ "," ++ show y ++ ") ")

myInt :: Word8 -> Int
myInt a = (fromIntegral(toInteger a))

myFloat :: Word8 -> Float
myFloat a = (fromIntegral(toInteger a) :: Float)

changePos :: (Int, Int) -> (Int, Int) -> (Int, Int)
changePos (x, y) (mx, my) = if y >= (my - 1) then (x + 1, 0) else (x, y + 1)

displayTripleListTupleNoInfo :: [(Word8, Word8, Word8)] -> IO ()
displayTripleListTupleNoInfo ((a, b, c):[]) =
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n"
displayTripleListTupleNoInfo ((a, b, c):as) =
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n" >>
    displayTripleListTupleNoInfo as

displayTripleListTuple ::
    [(Word8, Word8, Word8)] -> (Int, Int) -> (Int, Int) -> IO ()
displayTripleListTuple ((a, b, c):[]) pos size = displayPos pos >>
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n"
displayTripleListTuple ((a, b, c):as) pos size = displayPos pos >>
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n" >>
    displayTripleListTuple as (changePos pos size) size

displayGroup :: Group -> IO ()
displayGroup (Gro id (a,b,c)) = putStr "id = " >> putStr (show id) >>
    putStr (" (" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c)

displayAllGroup :: [Group] -> IO ()
displayAllGroup [] = putStr ""
displayAllGroup (Gro id (a,b,c):[]) = putStr " id = " >> putStr (show id) >>
    putStr (" (" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n"
displayAllGroup (Gro id (a,b,c):as) = putStr " id = " >> putStr (show id) >>
    putStr (" (" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ")\n" >> displayAllGroup as

displayListGroup ::
    [(Word8, Word8, Word8, Group)] -> (Int, Int) -> (Int, Int) -> IO ()
displayListGroup ((a, b, c, d):[]) pos size = displayPos pos >>
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ") " >> displayGroup d >>
    putStr ")\n"
displayListGroup ((a, b, c, d):as) pos size = displayPos pos >>
    putStr ("(" ++ (show a)) >> putStr "," >> putStr (show b) >>
    putStr "," >> putStr (show c) >> putStr ") " >> displayGroup d >>
    putStr ")\n" >> displayListGroup as (changePos pos size) size

displayDoubleTuple :: (Int, Int) -> IO ()
displayDoubleTuple (a, b) =
    putStr (show a) >> putStr " " >> putStr (show b) >> putStr "\n"

skipUseless :: String -> String
skipUseless [] = []
skipUseless (a:as) = if a < '0' || a > '9' then skipUseless as
    else myAppend [a] as

skipNumber :: String -> String
skipNumber [] = []
skipNumber (a:as) = if a >= '0' && a <= '9' then skipNumber as
    else skipUseless (myAppend [a] as)

getNumber :: String -> String -> Int
getNumber a [] = read a::Int
getNumber a (b:bs) = if b >= '0' && b <= '9'
    then getNumber (myAppend a [b]) bs
    else read a::Int

getNumberW :: String -> String -> Word8
getNumberW a [] = read a::Word8
getNumberW a (b:bs) = if b >= '0' && b <= '9'
    then getNumberW (myAppend a [b]) bs
    else read a::Word8

displayOneGroup ::
    Int -> Int -> [(Word8, Word8, Word8, Group)] -> [(Int, Int)] -> IO ()
displayOneGroup compt nb ((a,b,c,(Gro id (___))):[]) pos = 
    if id == nb then displayPos (myNth pos compt) >> displayTripleTuple (a,b,c)
    else putStr ""
displayOneGroup compt nb ((a,b,c,(Gro id (___))):as) pos = if id == nb
    then displayPos (myNth pos compt) >> displayTripleTuple (a,b,c) >>
    displayOneGroup (compt + 1) nb as pos
    else displayOneGroup (compt + 1) nb as pos

displayMyGroup :: [Group] -> Int -> IO ()
displayMyGroup ((Gro id (a, b, c)):[]) nb = if id == nb then putStr
    ("--\n(" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ ")\n-\n")
    else putStr ""
displayMyGroup ((Gro id (a, b, c)):gs) nb = if id == nb then putStr
    ("--\n(" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ ")\n-\n")
    else displayMyGroup gs nb

displayCluster :: [(Word8, Word8, Word8, Group)] -> [Group] ->
    [(Int, Int)] -> Int -> IO ()
displayCluster a group pos nb =
    if nb == (length group) then putStr "" else
    displayMyGroup group nb >> displayOneGroup 0 nb a pos >>
    displayCluster a group pos (nb + 1)
