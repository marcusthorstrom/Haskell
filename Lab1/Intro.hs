import Test.QuickCheck

--Part 1
power :: Int -> Int -> Int
power x n | n < 0 = error "power: negative argument"
power x 0         = 1
power x n         = x * power x (n-1)


--Part 2
-- x^n
power11 :: Int -> Int -> Int
power11 x n | n < 0 = error "power: negative argument"
power11 x n = product( replicate n x )

power1 :: Int -> Int -> Int
power1 n k | k < 0 = error "power1: negative argumenet"
power1 n 0         = 1
power1 n k | k > 0 = product [n | x<-[1..k]]


--part 3
power2 :: Int -> Int -> Int
power2 n k | k < 0   = error "power2: negative argument"
power2 n 0           = 1
power2 n k | even k  = power2 (n*n) (k `div` 2)
power2 n k | odd k   = n * power2 n (k-1)

props_power n k | k < 0 = True
props_power n k = power1 n k == power11 n k


--Part 4
props_powers :: Int -> Int -> Bool
props_powers n k | k < 0 = True
props_powers n k = (power n k == power1 n k) &&
                       (power2 n k == power1 n k)