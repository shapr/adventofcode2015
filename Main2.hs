{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.IO as TIO
import Data.List (sort)
import Data.Text hiding (minimum, take)
import Data.Attoparsec.Text hiding (take)

main :: IO ()
main = do input <- TIO.readFile "input2.txt"
          print "day 2 part 1"
          let ps = parseOnly (pPresent `sepBy` char '\n') input
          print . sum $ doP <$> fromRight ps
          print "day 2 part 2"
          print . sum $ doR <$> fromRight ps

data Present = P [Int] deriving Show

pPresent = P <$> decimal `sepBy` (char 'x')

doP (P []) = 0
doP (P xs@[a,b,c]) = let sides = [a*b, b*c, a*c] in sum ((*2) <$> sides) + minimum sides

doR (P []) = 0
doR (P xs@[a,b,c]) = product xs + (sum $ (*2) <$> smallsides)
  where smallsides = take 2 $ sort xs

fromRight (Right x) = x
