import Data.HashMap.Strict hiding (foldr, foldl)
import Data.List (scanl')
import Data.Bool
import Data.Monoid

main = interact $ solve $ solution

solve :: (Show a) => ([String] -> a) -> String -> String
solve f = (++ ['\n']) . show . f .  lines

solution :: [String] -> Integer
solution ls = let
        scores = fmap score ls
        (Sum s1, Sum s2) = mconcat scores
        in s1 * s2

score :: String -> (Sum Integer, Sum Integer)
score s = let
        f c hm = unionWith (+) hm (singleton c 1)
        counts = foldr f empty s
        doubles = any (== 3) counts
        triples = any (== 2) counts
        in (Sum $ bool 0 1 doubles, Sum $ bool 0 1 triples)
