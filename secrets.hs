{- I wrote this after reading the wikipedia article on Shamir's Secret Sharing (http://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing).
 - I have no idea what I am doing. I haven't audited this in any way. I'm using the built-in rand functions!
 - XXX Do NOT use this for anything serious. -}

import System.Random
import Data.List
import Data.Ratio

k = 3
n = 6
d = 1234
p = nextPrime (max n d)
--coeff = [166, 94]	-- wiki article coefficients (testing)
coeff = coefficients k p (mkStdGen 0)
terms = shares n p d coeff

-- These are inefficient
allPrimes = sieve [2..]
  where sieve (x:xs) = x : sieve [n | n <- xs, n `mod` x /= 0]
nextPrime n = go allPrimes
  where go = head . dropWhile (<= n)

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
-- Also, Jesus Christ, there's probably a better way than
-- using 40 where-clauses.
lagrange :: Integer -> [(Integer, Integer)] -> Integer
lagrange p terms = (sum $ map g terms) `mod` p
  where
    g (c, y) = y * l c
    l i = product [f x | (x, _) <- terms, x /= i]
      where
        f x = (-x) * inverse (i - x) p	-- f = (0-x) / (c-x)
          where
            inverse a = fst . extended_gcd a
            extended_gcd a b
              | b == 0		= (1, 0)
              | otherwise	= (t, s - q * t)
                where
                  (q, r) = a `divMod` b
                  (s, t) = extended_gcd b r
