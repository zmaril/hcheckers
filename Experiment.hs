module Experiment (bubbledPairs) where 

{-# LANGUAGE UnicodeSyntax #-}


import Math.Combinat.Partitions as P
import Prelude hiding (Left)
import Debug.Trace as T

-- -- data Problem = Problem {
-- --       shapes::[(Int,Shape)]
-- -- [(Int,Int,Shape)]-- (# shapes in problem,# of boxes in each shape, outline of shape)

-- problemsIn Gr {k=ki, n=ni} = 10
-- rule ((x1,y1),(x2,y2)) reds n =  (reds,True)

-- iterateGame :: Int -> Int -> Int -> Game -> (Int,Int,Int, Maybe Game)
-- iterateGame i n splitCount reds 
--     | (i == (length $ bubbledPairs n)) = (succ i, n, splitCount ,Nothing)
--     | (splitQ == True)  = (succ i, n, splitCount, Just newReds) 
--     | (splitQ == False) = iterateGame (succ i) n splitCount newReds
--     where (newReds, splitQ) = rule ((bubbledPairs n) !! i) reds n
--           splitCount = if splitQ then succ splitCount else splitCount


data Gr = Gr {k::Int, n::Int} deriving (Show)
data Game = Game {i :: Int, reds :: [[Bool]]} deriving(Show)

data Direction = Left | Down
type Directions = [Direction]
instance Show Direction where
    show Left = "←"
    show Down = "↓"



bubbledPairs :: Int -> [((Int,Int),(Int,Int))]
bubbledPairs n =  [((i,y1),(1+x2,y1-1)) 
                   | x2 <- [0..n-1]
                   ,  i <- [0..x2]
                   ,  let y1=n+i-x2 ]


createGame :: Gr -> P.Partition -> P.Partition -> Game
createGame Gr{k=ki, n=ni} a b = Game 0 []

partitionsFor Gr{k=ki, n=ni} =  filter (\x -> x /= P.partitions 0)  $ P.allPartitions' (ki,ni-ki)

_toDirections 0 h [] dirs = 
   T.trace ("0 "++ show h ++ " [] " ++ show dirs) $ 
    dirs
_toDirections i h [] dirs = 
   T.trace (show i ++ " "++ show h ++ " [] " ++ show dirs) $ 
    _toDirections (pred i) h [] ([Down] ++ dirs)
_toDirections i h xss@(x:xs) dirs = 
   T.trace (show i ++ " "++ show h ++ " " ++ show xss ++ " " ++ show dirs) $ 
    if x > h
    then _toDirections (pred i) x xs ([Down] ++ dirs)
    else _toDirections (pred i) h xs ([Left] ++ dirs)
        
toDirections :: Gr -> P.Partition -> Directions 
toDirections Gr{k=ki, n=ni} p = 
    T.trace (show p) $ 
    let ps = P.fromPartition p
        ls = ps ++ (take (ni-ki-1) $ repeat 0) 
    in  _toDirections ni 0 (reverse ls) []

gr = Gr 2 4 
main = do print $ foldr (++) [] $ partitionsFor gr
          print $ map (toDirections gr) $  foldr (++) [] $ partitionsFor gr
--main = do print $ createGame (Gr 2 4) (P.toPartition []) (P.toPartition [1])

-- If we are working with partitions of the same length can we just
-- have them reverse themselves and copy the positions down.
