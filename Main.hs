{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Char (isDigit)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)
import Data.Word (Word8)
import GHC.IO.Exception (ExitCode (..))
import Network.HTTP
import System.Environment
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | Performs a Box–Muller transform, obtaining
-- a sample from a normal distribution starting from
-- a pair of samples from the uniform distribution 
-- on the interval [0, 1]
transform
  :: Double
  -> Double 
  -> Double -- ^ the mean (mu)
  -> Double -- ^ the standard deviation (sigma)
  -> Double
transform u1 u2 mu sigma =
  sqrt ((-2) * log u1) * cos (2 * pi * u2) * sigma + mu

data NormalDistr =
  NormalDistr
    ( Double -- ^ the mean (mu)
    -> Double -- ^ the standard deviation (sigma)
    -> Double
    )

instance FromJSON NormalDistr where
 parseJSON (Object v) = fromUint8List <$> v .: "data"

-- | Reads a list of 8 unsigned 8-bit integers
-- to a sample from a normal distribution
fromUint8List :: [Word8] -> NormalDistr
fromUint8List x =
  let (a,b) = splitAt 4 x
  in NormalDistr $ transform (aux' a) (aux' b)
    where
      aux' = aux 0.0 1.0
      aux :: Double -> Double -> [Word8] -> Double
      aux n d [] = n / d
      aux n d (x:xs) = aux ((n * 256.0) + fromIntegral x) (d * 256.0) xs

-- | Obtains a smaple from the ANU Quantum Random Numbers Server
fromANU_QRNG :: IO NormalDistr
fromANU_QRNG = do
  -- print "Using ANU Quantum Random Numbers Server"
  -- print "(see http://qrng.anu.edu.au)"
  req <- simpleHTTP (getRequest "http://qrng.anu.edu.au/API/jsonI.php?length=8&type=uint8")
  getR <- getResponseBody req
  let Just res = decode $ pack getR
  return res

readTime :: String -> Maybe Double
readTime s = 
  case reverse s of
    [] -> Nothing
    's':xs -> readMaybe $ reverse xs
    'm':xs -> (60.0 *) <$> (readMaybe $ reverse xs)
    'h':xs -> (3600.0 *) <$> (readMaybe $ reverse xs)
    'd':xs -> (86400.0 *) <$> (readMaybe $ reverse xs)
    x:xs
      | isDigit x -> readMaybe s
      | otherwise -> Nothing


main :: IO ExitCode
main = do
  args <- getArgs
  case args of 
    [mu', sigma'] -> do
      case (readTime mu', readTime sigma') of
        (Just mu, Just sigma) -> do
          NormalDistr nd <- fromANU_QRNG
          let s = nd mu sigma
          let ms = max (round (s * 1000000)) 0
          -- logerr ("Sleeping for " ++ show (fromIntegral ms / 1000000) ++ " seconds...")
          threadDelay ms
          return ExitSuccess
        _ -> do
          logerr "rsleep: wrong operands"
          logerr "Try 'rsleep --help' for more information."
          return $ ExitFailure 1
    ["--help"] -> do
      logerr "Usage: rsleep [MU] [SIGMA]"
      logerr "\
\Pause for a random number of seconds. The random number \
\of seconds is sampled from a normal distribution of mean MU and \
\standard deviation SIGMA. The random data is obtained \
\from the ANU Quantum Random Numbers Server \
\(visit http://qrng.anu.edu.au).\n\
\MU and SIGMA are arbitrary floating point numbers, with an \
\optional suffix that may be 's' for seconds (the default), \
\'m' for minutes, 'h' for hours or 'd' for days."
      return $ ExitFailure 1
    _ -> do
      logerr "rsleep: wrong operands"
      logerr "Try 'rsleep --help' for more information."
      return $ ExitFailure 1 

logerr :: String -> IO ()
logerr x = hPutStrLn stderr x