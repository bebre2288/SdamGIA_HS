import Data.List

input :: String
input = replicate 1000 '9'

func :: [Char] -> (Char, Char) -> [Char]
func []    (pre1, pre2) = pre1:pre2:[]
func [a]   (pre1, pre2) = pre1:pre2:a:[]
func [a,b] (pre1, pre2) = pre1:pre2:a:b:[]
func (a:b:c:xs) (pre1, pre2)
    | not isin = pre1:pre2:a:b:c:xs
    | [a,b,c] == "999" = if pre1 == '8' && pre2 == '8' then func ('9':xs) ('_','_')
                         else if pre1 == '8' then func (xs) ('8','8')
                         else func (xs) ('8','_')
        where isin =  (("888" `isInfixOf` (a:b:c:xs)) || ("999" `isInfixOf` (a:b:c:xs)))

result :: String
result = func input ('_', '_')

main :: IO ()
main = print result
