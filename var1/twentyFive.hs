rng :: [Int]
rng = [45000000..5000000]

-- odDivisorsAm :: Int -> Int
-- odDivisorsAm n = length [x | x <- [1..(n `div` 2)], (n `mod` x == 0) && odd x]

odDivisorsAm :: Int -> Int -> Int -> Int
odDivisorsAm n d c
    | n `mod` 2 == 0 = odDivisorsAm (n `div` 2) d c
    | c > 5 = 0 
    -- | n `mod` 2 /= 0 = odDivisorsAm (n+1) d c
    | d > round (sqrt (fromIntegral n) + 1) = c
    -- | d > n `div` 2 = c
    | n `mod` d == 0 && odd d && even (n `div` d) = odDivisorsAm n (d+2) (c+1)
    | n `mod` d == 0 && odd d = odDivisorsAm n (d+2) (c+1+(intOfBool (n `div` d /= d)))
    | otherwise = odDivisorsAm n (d+1) c

result :: [Int]
result = [ x | x <- [45000000..50000000], odDivisorsAm x 1 0 == 5]

intOfBool :: Bool -> Int
intOfBool True  = 1
intOfBool False = 0

main :: IO ()
main = do
    print result
