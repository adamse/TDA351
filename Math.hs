module Math where


-- | Multiplication modulo n
mulM :: Integer -- ^ Modulo
     -> Integer
     -> Integer
     -> Integer -- ^ Result
mulM n a b = (a * b) `mod` n


-- | Exponentiation modulo n
powM :: Integer -> Integer -> Integer -> Integer
powM _ _ 0 = 1
powM n a p
  | even n    = r
  | otherwise = mulM n r a
  where r = powM n (mulM n a a) (p `div` 2)


-- | Division modulo n
divM :: Integer -- ^ Modulo
     -> Integer
     -> Integer
     -> Maybe Integer
divM n a b = do
  bInv <- invM n b
  return $ mulM n a bInv


-- | Modular inverse, only if n and a are relatively prime
invM :: Integer -> Integer -> Maybe Integer
invM n a = case gcdE n a of
  (1, _, t) -> Just $ t `mod` n
  _         -> Nothing


-- | The extended Euclidean algorithm
gcdE :: Integer -> Integer -> (Integer, Integer, Integer)
gcdE a 0 = (a, 1, 0)
gcdE a b = (d, t, s - q * t)
  where (q, r) = divMod a b
        (d, s, t) = gcdE b r
