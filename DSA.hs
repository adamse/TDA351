module DSA where

import Data.Maybe
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

verify
  :: DSAParameters 
  -> Integer -- ^ public key of the sender 
  -> Integer -- ^ message digest
  -> DSASignature
  -> Bool
verify (p, q, g) x z (r, s) = v == r
  where v  = mulM p (powP p g u1) (powP p y u2) `mod` q -- may be wrong, should be ((g^u1 * y^u2) mod p) mod q
        u1 = mulM q z w
        u2 = mulM q r w
        w = fromJust (invM s) `mod` q -- will throw an error if s: Nothing
