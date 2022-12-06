import Data.HashMap.Strict hiding (foldr, foldl)
import Data.List (scanl')
import Data.Bool
import Data.Monoid

main = interact $ solve $ solution

solve :: (Show a) => ([String] -> a) -> String -> String
solve f = (++ ['\n']) . show . f .  lines

solution :: [String] -> Maybe String
solution (b:bs) = mconcat (fmap (pair b) bs)
                  <> solution bs
solution [] = Nothing

score :: String -> (Sum Integer, Sum Integer)
score s = let
        f c hm = unionWith (+) hm (singleton c 1)
        counts = foldr f empty s
        doubles = any (== 3) counts
        triples = any (== 2) counts
        in (Sum $ bool 0 1 doubles, Sum $ bool 0 1 triples)

pair :: String -> String -> Maybe String
pair s s' = let
  common = inCommon s s'
  in if length s - length common == 1
     then Just common
     else Nothing

inCommon :: String -> String -> String
inCommon s s' = let
  ts = Prelude.filter (uncurry (==)) (zip s s')
  in (fmap fst ts)
