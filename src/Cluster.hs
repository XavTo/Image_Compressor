module Cluster where

import Utils
import ListToImage

import Codec.Picture
import Codec.Picture.Types
import System.Random
import Data.Word

findClosetColor ::
  [Group] -> (Word8, Word8, Word8) -> (Word8, Word8, Word8, Group)
findClosetColor groups (r, g, b) = (r, g, b,
  (fdCloClus groups (r, g, b) (Gro (-1) (0, 0, 0))))

fdCloClus ::  [Group] -> (Word8, Word8, Word8) -> Group -> Group
fdCloClus ((Gro id (gr, gg, gb)):gs) r (Gro (-1) (0, 0, 0)) = fdCloClus gs r (Gro id (gr, gg, gb))
fdCloClus ((Gro id (gr, gg, gb)):[]) (r, g, b) (Gro ei (er, eg, eb)) = if
  (distance (myFloat r, myFloat g, myFloat b) (myFloat gr, myFloat gg, myFloat gb))
      < (distance (myFloat r, myFloat g, myFloat b) (myFloat er, myFloat eg, myFloat eb))
  then (Gro id (gr, gg, gb)) else (Gro ei (er, eg, eb))
fdCloClus ((Gro id (gr, gg, gb)):gs) (r, g, b) (Gro ei (er, eg, eb)) = if
  (distance (myFloat r, myFloat g, myFloat b) (myFloat gr, myFloat gg, myFloat gb))
      < (distance (myFloat r, myFloat g, myFloat b) (myFloat er, myFloat eg, myFloat eb))
  then fdCloClus gs (r, g, b) (Gro id (gr, gg, gb)) else fdCloClus gs (r, g, b) (Gro ei (er, eg, eb))

calculEuclid :: (Float, Float, Float) -> (Float, Float, Float) -> Float
calculEuclid (xa, ya, za) (xb, yb, zb) = ((xa - xb) * (xa - xb)) +
  ((ya - yb) * (ya - yb)) + ((za - zb) * (za - zb))

distance :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distance (xa, ya, za) (xb, yb, zb) =
  (sqrt(calculEuclid (xa, ya, za) (xb, yb, zb)))

getMoy :: [(Word8, Word8, Word8)] -> (Word8, Word8, Word8)
getMoy [] = (0,0,0)
getMoy a = ((getFirstMoy a (length a) 0),
  (getSecondMoy a (length a) 0), (getThirdMoy a (length a) 0))

getMoyGroup :: Int -> [(Word8, Word8, Word8, Group)] ->
  [(Word8, Word8, Word8)] -> (Word8, Word8, Word8)
getMoyGroup nb ((a, b, c, (Gro id _)):[]) end = if id /= nb then (getMoy end)
  else (getMoy (myAppend end [(a, b, c)]))
getMoyGroup nb ((a, b, c, (Gro id _)):as) end = if id /= nb
  then getMoyGroup nb as end
  else getMoyGroup nb as (myAppend end [(a, b, c)])

listToGroup :: Int -> [(Word8, Word8, Word8)] -> [Group] -> [Group]
listToGroup compt a end = if compt /= (length a)
  then listToGroup (compt + 1) a (myAppend end [(Gro compt (myNth a compt))])
  else end

getMoyGroups :: Int -> Int -> [(Word8, Word8, Word8, Group)] ->
  [(Word8, Word8, Word8)] -> [Group]
getMoyGroups compt max listPixels end =
  if compt == max then listToGroup 0 end []
  else getMoyGroups (compt + 1) max listPixels
  (myAppend end [getMoyGroup compt listPixels []])

getOnlyCluster :: (Word8, Word8, Word8, Group) -> (Word8, Word8, Word8)
getOnlyCluster (a,b,c,(Gro id (d,e,f))) = (d,e,f)

parcStr :: String -> String -> String
parcStr a [] = a
parcStr a (b:bs) = if b >= '0' && b <= '9' then parcStr a bs
  else (myAppend [b] bs)

parseNumber :: [Word8] -> String -> [Word8]
parseNumber [] [] = []
parseNumber a [] = a
parseNumber a (l:ls) = if l >= '0' && l <= '9'
    then parseNumber (myAppend a [(getNumberW [l] ls)]) (parcStr [l] ls)
    else parseNumber a ls

generateList :: Int -> Int -> IO [Int]
generateList n max = sequence $ replicate n $ randomRIO (0,max::Int)

findOneClusterColor :: Int -> [(Word8, Word8, Word8)] -> Int -> Group
findOneClusterColor compt myListPixel a = (Gro compt (myNth myListPixel a))

createClusterColor ::
  Int -> [Int] -> [(Word8, Word8, Word8)] -> [Group] -> [Group]
createClusterColor compt [] myListPixel end = end
createClusterColor compt nb myListPixel end =
  createClusterColor (compt + 1) (myRemoveLast nb) myListPixel
  (myAppend end [findOneClusterColor compt myListPixel (myLast nb)])

setGroup :: [(Word8, Word8, Word8, Group)] -> [Group] ->
  [(Word8, Word8, Word8, Group)] -> Int -> [(Word8, Word8, Word8, Group)]
setGroup myListPixel gro end compt = if compt == (length gro) then end
  else setGroup myListPixel gro (myAppend end [(addGroup (myNth gro compt)
  (myNth (map (rmGroup) myListPixel) compt))]) (compt + 1)

findGroup :: [Group] -> Group -> Group
findGroup (a:[]) b = a
findGroup ((Gro id a):as) (Gro mid b) = if id == mid then (Gro id a)
  else findGroup as (Gro mid b)

changeValueGroup ::
  [Group] -> (Word8, Word8, Word8, Group) -> (Word8, Word8, Word8, Group)
changeValueGroup group (d,e,f,g) = (d,e,f,(findGroup group g))

groupsToTruple :: Group -> (Word8, Word8, Word8)
groupsToTruple (Gro id (a, b, c)) = (a, b, c)

addGroup :: Group -> (Word8, Word8, Word8) -> (Word8, Word8, Word8, Group)
addGroup i (a,b,c) = (a,b,c,i)

getGroup :: [(Word8, Word8, Word8, Group)] -> [Group] -> [Group]
getGroup ((a, b, c, (Gro id (ga,gb,gc))):[]) end =
  if (myIsInGroup (Gro id (ga,gb,gc)) end) == False
  then (myAppend end [(Gro id (ga,gb,gc))]) else end
getGroup ((a, b, c, (Gro id (ga,gb,gc))):ls) end =
  if (myIsInGroup (Gro id (ga,gb,gc)) end) == False
  then getGroup ls (myAppend end [(Gro id (ga,gb,gc))]) else getGroup ls end

rmGroup :: (Word8, Word8, Word8, Group) -> (Word8, Word8, Word8)
rmGroup (a,b,c,d) = (a,b,c)

addGroupToPixel ::
  [Group] -> (Word8, Word8, Word8, Group) -> (Word8, Word8, Word8, Group)
addGroupToPixel clusters pixels = (findClosetColor clusters (rmGroup pixels))

createGroup :: [(Word8, Word8, Word8, Group)] -> [Group] ->
  Int -> [(Word8, Word8, Word8, Group)] -> [(Word8, Word8, Word8, Group)]
createGroup myListPixel clusters compt end =
  if compt == (length myListPixel) then end
  else createGroup myListPixel clusters (compt + 1)
    (myAppend end [(addGroupToPixel clusters (myNth myListPixel compt))])

checkIfSameColor :: [Int] -> [(Word8, Word8, Word8)] -> [(Word8, Word8, Word8)] -> Bool
checkIfSameColor (n:[]) listPixels end = 
  if (isElement (myNth listPixels n) end) == False then True else False
checkIfSameColor (n:nb) listPixels end =
  if (isElement (myNth listPixels n) end) == False then
    checkIfSameColor nb listPixels (myAppend end [myNth listPixels n])
  else False

createRandom :: Int -> [(Word8, Word8, Word8)] -> Int -> IO [Int]
createRandom n newListPixel 20 = (generateList n ((length newListPixel) - 1))
createRandom n newListPixel loop = do
  ramdonNumber <- generateList n ((length newListPixel) - 1)
  if (checkIfSameColor ramdonNumber newListPixel []) == True
    then return ramdonNumber else createRandom n newListPixel (loop + 1)

toPos :: Float -> Float
toPos f = if f < 0 then (f * (-1)) else f

calculConv :: Float -> Float -> Float -> Float -> Float -> Float -> Float
calculConv ga gb gc na nb nc = distance (ga, gb, gc) (na, nb, nc)

checkConv :: Float -> [Group] -> [Group] -> Bool
checkConv conv [] [] = True
checkConv conv ((Gro _ (ga, gb, gc)):gs) ((Gro _ (na, nb, nc)):ns) =
  if (calculConv (fromIntegral (toInteger ga) :: Float)
  (fromIntegral (toInteger gb) :: Float) (fromIntegral (toInteger gc) :: Float)
  (fromIntegral (toInteger na) :: Float) (fromIntegral (toInteger nb) :: Float)
  (fromIntegral (toInteger nc) :: Float)) > conv
  then False else checkConv conv gs ns

myCalcul :: [(Word8, Word8, Word8, Group)] -> [Group] ->
  Float -> Int -> IO [(Word8, Word8, Word8, Group)]
myCalcul myListPixel group conv nb = do
  let newListPixel = createGroup myListPixel group 0 []
  let newGroup = getMoyGroups 0 (length group) newListPixel []
  if (checkConv conv group newGroup) == True
    then return (map (changeValueGroup newGroup) newListPixel) else
    myCalcul (map (changeValueGroup newGroup) newListPixel) newGroup conv nb

myKmeans :: [(Word8, Word8, Word8)] -> Int -> Float -> (Int, Int) ->  IO ()
myKmeans myListPixel n l (x, y) = do
  let saveLast = myLast myListPixel
  let newListPixel = myRemoveLast myListPixel
  let myGroup = Gro {iGroup = 0, groupIndo = (0,0,0)}
  ramdonNumber <- createRandom n newListPixel 0
  let clusters = createClusterColor 0 ramdonNumber newListPixel []
  imageClustered <- myCalcul (map (addGroup myGroup) newListPixel) clusters l n
  let onlyCluster = map (getOnlyCluster) imageClustered
  let myimage = listToImage (myAppend onlyCluster [saveLast]) x
  writePng ("image.png") myimage
