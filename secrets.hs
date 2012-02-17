{- I wrote this after reading the wikipedia article on Shamir's Secret Sharing (http://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing).
 - I have no idea what I am doing. I haven't audited this in any way. I'm using the built-in rand functions!
 - XXX Do NOT use this for anything serious. -}

module Main( main ) where

import System
import System.Random
import Data.List

{- TODO
	some padding or something
	parse commandline args (learn getOpt)
-}

-- Taken from: http://www.haskell.org/haskellwiki/Testing_primality
-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
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
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)
-- End of Taken from: http://www.haskell.org/haskellwiki/Testing_primality

trialFactor n = n > 1 && all (\x -> n `mod` x /= 0) ls
    where
      ls	= takeWhile (<= isqrt n) primes
      isqrt	= floor . sqrt . fromIntegral

-- Heavily inspired from some guy on the Internet that I can't remember now
isPrime n
  | n < 500000	= trialFactor n
  | otherwise	= all (millerRabinPrimality n) primes100
  where
    primes100 = take 100 primes

primes = 2 : filter isPrime [3,5..]

nextPrime n
  | n < 50000	= head . dropWhile (<= n) $ primes
  | otherwise	= head . filter isPrime $ [n..]

{- User inputs -}
k = 3
n = 6
d = 1234

p = nextPrime (max n d)
coeff = coefficients k p (mkStdGen 0)

{- Each person gets one of these elements. -}
terms = shares n p d coeff

{- This is the recombination. -}
secret = lagrange p terms

-- The coefficients in the range [0, p) of a random (k-1) polynomial
coefficients :: (RandomGen g) => Integer -> Integer -> g -> [Integer]
coefficients k p g = genericTake (k - 1) ls
  where
    ls = randomRs (0, p-1) g

-- This returns each secret share tuple (index and value) in a list
shares :: Integer -> Integer -> Integer -> [Integer] -> [(Integer, Integer)]
shares n p d coeff = foldr go [] ls
  where
    go i acc	= (i, eval i) : acc
    ls		= [1..n]
    -- Returns: d + a_1*i + a_2*i^2 + ... + a_n*i^n
    eval i = sum ts `mod` p
      where
        powers	= iterate (*i) i		-- [i, i^2, i^3, ...]
        ts	= d : zipWith (*) coeff powers	-- [d, a_1 * i, a_2 * i^2, ..., a_n * i^n]

-- Assumes we'll meet the threshold.
{- We evaluate the Lagrange polynomial at 0 as the
 - secret data (D) has no x term attached to it -}
lagrange :: Integer -> [(Integer, Integer)] -> Integer
lagrange p terms = (sum $ map g terms) `mod` p
  where
    g (c, y)	= y * l c
    l i		= product [f x | (x, _) <- terms, x /= i]
      where
        f x	= (-x) * inverse (i - x) p	-- f = (0-x) / (c-x)
    inverse a 	= fst . extended_gcd a
    extended_gcd a b
      | b == 0		= (1, 0)
      | otherwise	= (t, s - q * t)
        where
          (q, r) = a `divMod` b
          (s, t) = extended_gcd b r

-- -k 3 -n 6 used to create the shares (read d from stdin)
-- -s 1:12 -s 2:24 -s 3:36 used to restore the secret (or take them from stdin)
-- note: gotta keep track of p somehow
main = do
  mapM_ putStrLn =<< getArgs
