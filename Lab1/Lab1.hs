{-
Lab Assignment 1
authors:
Oskar Selberg    - 910616-1731
Marcus Thorström - 931015-2914
-}
import Test.QuickCheck
------------------------------------------------------------------------
--                              Task 1                                --
------------------------------------------------------------------------
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0         = 1
power n k         = n * power n (k-1)

--It takes (1+k) steps for a function n^k
power_steps :: Integer -> Integer -> Integer
power_steps n k | k < 0 = error "power: negative argument"
power_steps n 0         = 1
power_steps n k | k > 0 = 1 + power_steps n (k-1)

------------------------------------------------------------------------
--                               Task 2                               --
------------------------------------------------------------------------
power1 :: Integer -> Integer -> Integer
power1 n k | k < 0 = error "power1: negative argumenet"
power1 n 0         = 1
power1 n k | k > 0 = product [n | x<-[1..k]]

------------------------------------------------------------------------
--                               Task 3                               --
------------------------------------------------------------------------
power2 :: Integer -> Integer -> Integer
power2 n k | k < 0   = error "power2: negative argument"
power2 n 0           = 1
power2 n k | even k  = power2 (n*n) (k `div` 2)
power2 n k | odd k   = n * power2 n (k-1)

------------------------------------------------------------------------
--                               Task 4                               --
------------------------------------------------------------------------

--A
{-
  The test cases to be tested are:
  2^0 = 1 : this will wheck if the basecase is working
  2^(-1) = error : this can be excluded
  2^8 = 256 : a standard test, if clears all other should clear as well
-}

--B
props_powers :: Integer -> Integer -> Bool
props_powers n k = (power n k == power1 n k) ==
                   (power2 n k == power1 n k)

--C
simple_test :: Bool
simple_test = (props_powers 2 0) && (props_powers 2 8) &&
              (power 2 0 == 1) && (power 2 8 == 256) &&
              (power1 2 0 == 1) && (power1 2 8 == 256) &&
              (power2 2 0 == 1) && (power2 2 8 == 256)

--D
--needed for QuickCheck to not fail on negative
props_powers' :: Integer -> Integer -> Bool
props_powers' n k | k < 0 = True
props_powers' n k    = (power n k == power1 n k) ==
                       (power2 n k == power1 n k)
