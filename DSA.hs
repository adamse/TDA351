module DSA where

import Data.Maybe (fromJust)
import System.Random

import Math
import Primality

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

genKey :: RandomGen g => g -> DSAParameters -> (g, KeyPair)
genKey gen (p, q, g) | c > q-2   = genKey gen' (p, q, g)
                     | otherwise = (gen', (x, y))
  where n         = 160
        (c, gen') = randomR (2^(n-1), 2^n-1) gen
        x         = c + 1
        y         = powM p g x

sign 
  :: RandomGen g => g
  -> DSAParameters 
  -> KeyPair 
  -> Integer -- ^ message digest
  -> (g, DSASignature)
sign gen (p, q, g) (x, _) z = (gen', (r, s))
  where (k, gen') = randomR (1, q - 1) gen
        r = powM p g k `mod` q
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

verifyParameters :: RandomGen g => g -> DSAParameters -> Bool
verifyParameters gen (p, q, g) =
  and [ isPrime gen p
      , isPrime gen q
      , hasBits 1024 p
      , hasBits 160 q
      , q `divides` (p - 1)
      , powM p g q == 1
      , g > 1
      , p > g
      ]

hasBits :: Integer -> Integer -> Bool
hasBits n x = 2 ^ (n - 1) <= x && x < 2 ^ n

divides :: Integer -> Integer -> Bool
divides a b = b `mod` a == 0
