{- cabal:
build-depends: base, split
-}
import Data.List.Split (splitOn)

example = [3, 11, 12, 16, 24, 25]

sq, border, hole :: [Int]
sq = [ n ^ 2 | n <- [1, 2..]]
border = [ n^2 - (n-2)^2 | n <- [3, 4..]]
hole = [n^2 - 1 | n <- [1,3..]]

countCones :: [Int] -> Int -> Int
countCones ks n =
  case dropWhile (< n) ks of
    k : _ -> k - n
    []    -> error "empty list"

fillMissing :: (Foldable t, Functor t) => [Int] -> t Int -> Int
fillMissing ks instructions = sum $ fmap (countCones ks) instructions

parse :: [Char] -> [Int]
parse x = fmap read $ splitOn "," x

main :: IO ()
main = 
  do  
    text1 <- readFile "2026/data/01_01.csv"
    text2 <- readFile "2026/data/01_02.csv"
    text3 <- readFile "2026/data/01_03.csv"

    let instructions1 = parse text1
    let instructions2 = parse text2
    let instructions3 = parse text3

    print $ fillMissing sq example
    print $ fillMissing border example
    print $ fillMissing hole example
    print $ fillMissing sq instructions1
    print $ fillMissing border instructions2
    print $ fillMissing hole instructions3