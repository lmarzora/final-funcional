module Image where

import Data.Word
import System.IO
import Control.Monad

writeImage :: FilePath -> Int -> Int -> [(Double, Double, Double)] -> IO()
writeImage path width height pixels = 
    let pix = map (\(r,g,b) -> (castPixel r, castPixel g, castPixel b)) pixels
    in
        writePPM path width height pix

castPixel :: Double -> Word8
castPixel x = if x > 1 then 255 else if x < 0 then 0 else floor (x * 255)

-- writePPM
writePPM :: FilePath -> Int -> Int -> [(Word8, Word8, Word8)] -> IO()
writePPM path width height pixels = 
    let stringPixels = map (\(r,g,b) -> show r ++ " " ++ show g ++ " " ++ show b ++ " ") pixels
    in
        do
        file <- openFile path WriteMode
        hPutStrLn file "P3"
        hPutStrLn file (show width ++ " " ++ show height)
        hPutStrLn file (show (width * height - (length pixels)))
        hPutStrLn file "255"
        forM_ stringPixels (hPutStr file)