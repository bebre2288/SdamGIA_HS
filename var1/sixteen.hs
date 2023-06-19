f :: Int -> Int
f 1 = 1
f n = f (n-1) + n

result :: Int
result = f 40

main :: IO ()
main = print result 
