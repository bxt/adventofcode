import Data.List (elemIndex)

oeisA000203 = foo 1 zeros where
  foo n (x:xs) = zipWith (+) (cycle (n:take (n-1) zeros)) (x : foo (n+1) xs)
  zeros = repeat 0

needle :: Int
needle = 3310000 -- devided by 10

-- From https://wiki.haskell.org/Prime_numbers
primesToG m = 2 : sieve [3,5..m]
  where
    sieve (p:xs)
       | p*p > m   = p : xs
       | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])

-- dito
minus (x:xs) (y:ys) = case compare x y of
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs

factors = [1, 2, 4, 5, 8, 10, 16, 20, 25, 40, 50, 80, 100, 125, 200, 250, 331, 400, 500, 625, 662, 1000, 1250, 1324, 1655, 2000, 2500, 2648, 3310, 5000, 5296, 6620, 8275, 10000, 13240, 16550, 26480, 33100, 41375, 66200, 82750, 132400, 165500, 206875, 331000, 413750, 662000, 827500, 1655000, 3310000]

trd (a,b,c) = c

main :: IO()
main = print $ filter ((`elem` factors).fst) $ zip oeisA000203 [1 ..]
-- main = print $ filter ((`elem` factors).trd) $ concatMap (\p -> [(p,i,((p**i)-1) / (p-1)) | i <- [2 .. 50000]]) $ primesToG 1000

-- 3310000 = 2^4 * 5^4 * 331 = 662 * 2^3 5^4

-- 2^2 * 2^3*5 * 2^3 * 3^4*4^2 * 2^2*5 * 2^4*5 * 2^3*5^2 * 5^3*2^4
-- 4.0 * 40.0  * 8.0 *  400.0  * 20.0  * 80.0  *  200.0  * 500.0

-- 5000 = 2^3*5^4
-- 1250= 2 * 5^4

-- 1249.0,2.0,1250.0
-- 2647.0,2.0,2648.0
-- 1249 * 2647 = 3306103
--
-- 661.0,2.0,662.0
-- 4999.0,2.0,5000.0
-- 661 * 4999 = 3304339
--
-- 6619.0,2.0,6620.0
-- 499.0,2.0,500.0
-- 6619 * 499 = 3302881

-- 661.0,2.0,662.0
-- 1249.0,2.0,1250.0
-- 3.0,2.0,4.0
-- 661 * 1249 * 3 = 2476767 <- yey!

-- 661.0,2.0,662.0
-- 4999.0,2.0,3747.0
-- 661 * 3747 = 2476767 had this...
