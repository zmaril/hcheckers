-- | Main entry point to the application 

module Main where

import Data.List
import Data.Maybe

third (_,_,x) = x

data Placement = Same | Elsewhere | Nowhere deriving(Show,Eq)
type Point = (Int,Int)
type Board = [Int]

criticalRow :: Point -> Board -> (Placement,Maybe Int)
criticalRow (x2,y2) reds = 
    case y2 `elemIndex` drop x2 reds of
      Just 0 -> (Same, Just x2)
      Just i -> (Elsewhere, Just (x2+i))
      Nothing -> (Nowhere,Nothing)

criticalDiagonal :: Point -> Board -> (Placement,Maybe Int)
criticalDiagonal (x1,y1) reds = 
    case map third $ filter (\(x,y,z) -> x == y) $ zip3 (drop x1 reds) [y1,y1-1..] [x1..] of
      []   -> (Nowhere, Nothing)
      x:xs -> (if x == x1 then Same else Elsewhere, Just x)

rtest:: Point -> Point -> Board -> (Board, Bool)
rtest p1@(x1,y1) p2@(x2,y2) reds = 
    case (greek,roman) of
       (Same,Same) -> (reds, False)
      where n = length reds
            (greek, cr) = criticalRow      p2 reds
            (roman, cd) = criticalDiagonal p1 reds

main :: IO()
main = do 
  print((rtest(0,1) (1,0) [1,0]), ([0,1],False))



