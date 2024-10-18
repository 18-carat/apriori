import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')

type Transaction = String
type CharsCount = (String, Int)

min_sup_rat :: Float
min_sup_rat = 0.6

transactions :: [Transaction]
transactions = ["MONKEY", "DONKEY", "MAKE", "MUCKY", "COOKIE"]

combos :: Int -> String -> [String]
combos 0 _ = [[]]
combos _ [] = []
combos n (x:xs) = map (x:) (combos (n - 1) xs) ++ combos n xs

isSuperset :: String -> String -> Bool
isSuperset s1 s2 = all (`elem` s1) s2

validC :: String -> [CharsCount] -> Bool
validC c l =
  let subsets = combos (length c - 1) c
  in all (\sub -> any (isSuperset sub) (map fst l)) subsets

c :: Int -> [CharsCount] -> [CharsCount]
c 1 _ = Map.toList (foldl' inc Map.empty (concat uniq))
    where
        uniq = map Set.toList (map Set.fromList transactions)
        inc acc char = Map.insertWith (+) [char] 1 acc

c n xs = Map.toList (foldl' inc Map.empty vCombos)
    where
        inXs = Set.fromList (concatMap fst xs)
        uniq = map Set.toList (map Set.fromList transactions)
        vCombos = [x | x <- concatMap (combos n . filter (`Set.member` inXs)) uniq, validC x xs]
        inc acc chars
            | any (isSuperset chars) (map fst xs) = Map.insertWith (+) chars 1 acc
            | otherwise = acc

l :: [CharsCount] -> [CharsCount]
l xs = filter (\(_, i) -> i >= min_sup) xs
    where 
        min_sup = floor (min_sup_rat * fromIntegral (length transactions))

whileNotEmpty :: Int -> [CharsCount] -> IO ()
whileNotEmpty n xs = do
  let curC = c n xs
  let curL = l curC
  
  putStrLn $ "C" ++ show n ++ ": " ++ show curC
  putStrLn $ "L" ++ show n ++ ": " ++ show curL
  
  if null curL
    then return ()
    else whileNotEmpty (n + 1) curL

main :: IO ()
main = do
    let c1 = c 1 []
    let l1 = l c1
    
    putStrLn $ "C1: " ++ show c1
    putStrLn $ "L1: " ++ show l1
    
    whileNotEmpty 2 l1
