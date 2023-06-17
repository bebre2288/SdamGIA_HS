input :: String
input = replicate 1001 '9'

func :: [Char] -> [Char]
func []    = []
func [a]   = a : []
func [a,b] = a:b:[]
func (a:b:c:xs)
             | [a,b,c] == "888" = '9' : xs
             | [a,b,c] == "999" = '8' : xs
             | otherwise = func $ [b,c] ++ xs

res :: [Char] -> [Char]
res sq
    | sq == "888" = "9"
    | sq == "999" = "8"
    | otherwise = sq 

dothatshit = [ res [a,b,c] | (a,b,c) <- zip3 input (tail input) (tail (tail input))]
