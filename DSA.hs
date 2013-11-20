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
        s = fromJust $ divM q (z + mulM q x r) k -- Safe since all k has inverse in modulo q
