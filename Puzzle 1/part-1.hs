main = interact solve

solve :: String -> String
solve = show . sum . (fmap readInteger) . lines

readInteger :: String -> Integer
readInteger ('+' : cs) = read cs
readInteger cs = read cs
