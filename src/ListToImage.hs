module ListToImage where

import Utils

import Codec.Picture
import Codec.Picture.Types
import Data.Word

getWidth :: (Word8, Word8, Word8) -> Int
getWidth (a, b, c) = (fromIntegral (toInteger a))

getImageWidth :: [(Word8, Word8, Word8)] -> Int
getImageWidth a = getWidth (myLast a)

getHeight :: (Word8, Word8, Word8) -> Int
getHeight (a, b, c) = (fromIntegral (toInteger b))

getImageHeight :: [(Word8, Word8, Word8)] -> Int
getImageHeight a = getHeight (myLast a)

getPixelAtIndex :: (Word8, Word8, Word8) -> PixelRGB8
getPixelAtIndex (a, b, c) = PixelRGB8 a b c

listToImage :: [(Word8, Word8, Word8)] -> Int -> Image PixelRGB8
listToImage a size = generateImage myGenerateImage (getImageWidth a) (getImageHeight a)
    where myGenerateImage x y = (getPixelAtIndex (myNth a (x + (size * y))))
