module Main where

main :: IO ()
main = do input <- readFile "input1.txt"
          print "day 1 part 1"
          print $ foldl wib 0 input
          print "day 1 part 2"
          let vals = wob <$> input
          print $ length $ takeWhile (> -1) $ scanl (+) 0 vals

wib :: Int -> Char -> Int
wib n '(' = n + 1
wib n ')' = n - 1
wib _ _ = error "wib broke"

wob :: Char -> Int
wob '(' = 1
wob ')' = -1
wob _ = error "wob bad input"
