module DSA where

import Data.Maybe (fromJust)
import System.Random
import System.IO.Unsafe (unsafePerformIO)

import Math
import qualified Primality

type DSAParameters = 
  ( Integer -- ^ p - a prime modulus
  , Integer -- ^ q - a prime divisor
  , Integer -- ^ g - a generator
  )

type KeyPair = 
  ( Integer -- ^ x - the private key
  , Integer -- ^ y - the public key
  )

type DSASignature = 
  ( Integer -- ^ r 
  , Integer -- ^ s
  )

genKey :: RandomGen g => g -> DSAParameters -> KeyPair
genKey gen (p, q, g) | c > q-2   = genKey gen' (p, q, g)
                     | otherwise = (x, y)
  where n         = 160
        (c, gen') = randomR (2^(n-1), 2^n-1) gen
        x         = c + 1
        y         = powM p g x

sign 
  :: DSAParameters 
  -> KeyPair 
  -> Integer -- ^ message digest
  -> DSASignature
sign (p, q, g) (x, _) z = (r, s)
  where k = unsafePerformIO $ randomRIO (1, q - 1) -- TODO: Don't use unsafePerformIO
        r = (powM p g k) `mod` q
        s = fromJust $ divM q (z + mulM q x r) k -- Safe since k is in Z*_q

verify
  :: DSAParameters
  -> Integer -- ^ public key of the sender
  -> Integer -- ^ message digest
  -> DSASignature
  -> Bool
verify (p, q, g) y z (r, s) = v == r
  where v  = mulM p (powM p g u1) (powM p y u2) `mod` q
        u1 = mulM q z w
        u2 = mulM q r w
        w  = fromJust $ invM q s -- Safe since s is in Z*_q

verifyParameters :: DSAParameters -> Bool
verifyParameters (p, q, g) =
  and [ isPrime p
      , isPrime q
      , hasBits 1024 p
      , hasBits 160 q
      , q `divides` (p - 1)
      , powM p g q == 1
      , g > 1
      ]

hasBits :: Integer -> Integer -> Bool
hasBits n x = 2 ^ (n - 1) <= x && x < 2 ^ n

divides :: Integer -> Integer -> Bool
divides a b = b `mod` a == 0

isPrime :: Integer -> Bool
isPrime n = Primality.isPrime (unsafePerformIO getStdGen) n -- TODO: Don't use unsafePerformIO
