{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random
import System.Environment
import Control.Concurrent (threadDelay)
import GHC.IO.Exception (ExitCode (..))
import Network.HTTP
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)

import Data.Aeson

unif :: Double -> Double -> Double -> Double -> Double
unif u1 u2 sigma mu =
  sqrt ((-2) * log u1) * cos (2 * pi * u2) * sigma + mu

-- 

data RndData = RndData Double Double

instance FromJSON RndData where
 parseJSON (Object v) = fromUint8List <$> v .: "data"

fromUint8List :: [Int] -> RndData
fromUint8List x =
  let (a,b) = splitAt 4 x
  in RndData (aux' a) (aux' b)
    where
      aux' = aux 0.0 1.0
      aux :: Double -> Double -> [Int] -> Double
      aux n d [] = n / d
      aux n d (x:xs) = aux ((n * 256.0) + fromIntegral x) (d * 256.0) xs

fromANU_QRNG :: IO RndData
fromANU_QRNG = do
  print "Using ANU Quantum Random Numbers Server"
  print "(see http://qrng.anu.edu.au)"
  req <- simpleHTTP (getRequest "http://qrng.anu.edu.au/API/jsonI.php?length=8&type=uint8")
  getR <- getResponseBody req
  let Just res = decode $ pack getR
  return res


main :: IO ExitCode
main = do
  args <- getArgs
  case args of 
    [mu', sigma'] -> do
      mu <- readIO mu'
      sigma <- readIO sigma'
      RndData u1 u2 <- fromANU_QRNG
      let s = unif u1 u2 sigma mu
      let ms = max (round (s * 1000000)) 0
      print ("Delay: " ++ show (fromIntegral ms / 1000000) ++ " seconds")
      threadDelay ms
      return ExitSuccess
    _ -> do
      print "Usage: rsleep [mu] [sigma]"
      print "  (where mu is the mean, and sigma the s.d. of the normal distribution)"
      return (ExitFailure 1)