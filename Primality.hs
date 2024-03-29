module Primality where

import Data.List (mapAccumL)
import Data.Tuple (swap)
import System.Random

-- | Test n for primality using Miller-Rabin.
-- Testing with 20 witnesses should give a false 
-- positive probability of about (0.25^20) = 9,0*10^-13
isPrime :: RandomGen g => g -> Integer -> Bool
isPrime gen n = and . snd . mapAccumL test gen $ replicate 20 n
  where test gen n = swap $ isProbablyPrime n gen

-- Miller Rabin primality test 
-- from http://hackage.haskell.org/package/Numbers-0.2.1/docs/src/Data-Numbers-Primes.html
-- Miller Rabin Primality from the Haskell Wiki --

-- | Performs a Miller Rabin Primality Test. According to the Wikipedia it's false positive with a probability of less than 25%. It's never false negative. Use it several times to increase confidence.
isProbablyPrime :: RandomGen g => Integer -> g -> (Bool, g)
isProbablyPrime n gen
    | n < 2 = (False, gen')
    | n == 2 = (True, gen')
    | even n = (False, gen')
    | b0 == 1 || b0 == n' = (True, gen')
    | otherwise = (iter (tail b), gen')
    where
        (a, gen') = randomR (2,n-2) gen
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        

pow' :: (Num a, Integral b) => (a -> a -> a) -> (a -> a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)
