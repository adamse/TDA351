module Main where

import Control.Monad (unless, liftM)
import Data.List (mapAccumL)
import System.Exit
import System.Random
import Data.Text.Read (hexadecimal)
import Data.Text (pack)

import DSA (DSAParameters, KeyPair, DSASignature)
import qualified DSA

main = do
  dsaParameters <- getDSAPArameters
  checkDSAParameters dsaParameters

  line <- getLine
  action line dsaParameters

action "genkey" = genKeys
action "sign" = sign
action "verify" = verify
action _ = const $ return ()

genKeys dsaParameters = do
  n <- getNumber
  gen <- getStdGen

  loop n gen
  where
    loop 0 _ = return ()
    loop n gen = do
      let (gen', keyPair) = DSA.genKey gen dsaParameters
      putStrLn $ showKeyPair keyPair
      loop (n-1) gen'

sign dsa = do
  x <- getNumber
  y <- getNumber
  ds <- getContents

  gen <- getStdGen

  let zs = map (readHexNumber) $ lines ds
  let (_, ss) = mapAccumL (\gen z -> DSA.sign gen dsa (x, y) z) gen zs

  putStr . unlines $ map showDSASignature ss

verify dsa = do
  y <- getNumber
  signs <- getContents

  loop y $ lines signs
  where
    loop _ [] = return ()
    loop y (d':r':s':ss) = do
      let d = readHexNumber d'
      let r = readNumber $ r'
      let s = readNumber $ s'
      putStrLn $ if DSA.verify dsa y d (r, s) then "signature_valid" else "signature_invalid"
      loop y ss

getDSAPArameters :: IO DSAParameters
getDSAPArameters = do
  p <- getNumber
  q <- getNumber
  g <- getNumber
  return (p, q, g)

checkDSAParameters :: DSAParameters -> IO ()
checkDSAParameters dsaParameters = do
  gen <- getStdGen
  unless (DSA.verifyParameters gen dsaParameters) $ do
    putStrLn "invalid_group"
    exitFailure

  putStrLn "valid_group"

getNumber :: IO Integer
getNumber = liftM readNumber getLine

readNumber :: String -> Integer
readNumber = read . drop 2

readHexNumber :: String -> Integer
readHexNumber s = let (Right (n, _)) = hexadecimal . pack . drop 2 $ s in n

showKeyPair :: KeyPair -> String
showKeyPair (x, y) = 
  concat ["x=", show x, "\ny=", show y]

showDSASignature :: DSASignature -> String
showDSASignature (r, s) = 
  concat ["r=", show r, "\ns=", show s]
