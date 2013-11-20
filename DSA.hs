module DSA where

import Data.Maybe (fromJust)
import System.Random
import System.IO.Unsafe (unsafePerformIO)

import Math

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

sign 
  :: DSAParameters 
  -> KeyPair 
  -> Integer -- ^ message digest
  -> DSASignature
sign (p, q, g) (x, y) z = (r, s)
  where k = unsafePerformIO $ randomRIO (1, q - 1) -- TODO: don't use unsafePerformIO
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
