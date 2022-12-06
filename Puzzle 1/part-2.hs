import Data.Set hiding (foldr, foldl)
import Data.List (scanl')

main = interact $ solve $ solution . frequencies

solution :: [Integer] -> Integer
solution xs = let
        go seen (x:xs) = if member x seen
                then x
                else go (insert x seen) xs
        in go empty xs

solutionFold :: [Integer] -> Integer
solutionFold = let
        f x (seen, z) = if member x seen
                then (seen, x)
                else (insert x seen, z)
        in foldr f undefined

and :: [Bool] -> Bool
and = let
        f False _ = False
        f True acc = True && acc

frequencies :: [Integer] -> [Integer]
frequencies = (scanl (+) 0) . cycle

solve :: (Show a) => ([Integer] -> a) -> String -> String
solve f = (++ ['\n']) . show . f . (fmap readInteger) . lines

readInteger :: String -> Integer
readInteger ('+' : cs) = read cs
readInteger cs = read cs

myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myFoldl'' f z [] = z
myFoldl'' f z xs = let
        g x acc = f
        foldr g z xs
