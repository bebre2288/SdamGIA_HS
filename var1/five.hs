import Data.Char
import Data.List

bintodec :: Integral i => i -> Maybe i
bintodec 0 = Just 0
bintodec i | last < 2 = fmap (\x -> 2*x + last) (bintodec (div i 10))
           | otherwise = Nothing
    where last = mod i 10

toBin :: Int -> String
toBin n = map (chr . (+48)) (reverse (toBinH n))

toBinH :: Int -> [Int]
toBinH 0 = []
toBinH n = n `mod` 2 : (toBinH $ n `div` 2)

transform1 :: Int -> String
transform1 n = toBin n

transform2 :: Int -> Int
transform2 n = (read $ (tail (transform1 n)) :: Int)

transform3 :: Int -> Int
transform3 n = maybe 0 (*1) (bintodec  (transform2 n))

transform4 :: Int -> Int
transform4 n = n - (transform3 n)

solve :: Int
solve = length $ group $ sort [transform4 i | i <- [10..1000]]
