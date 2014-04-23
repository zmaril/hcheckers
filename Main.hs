module Main 
    (Gr, pad,main)
where

{-# LANGUAGE UnicodeSyntax #-}

import Math.Combinat.Partitions   as P
import Math.Combinat.Sets         as S
import Math.Combinat.Permutations as PM
import Debug.Trace as T
import Data.List
import Text.PrettyPrint.Boxes

-- Small useful function
pad xs m el = xs ++ replicate (m - length xs) el

-- Grassmanians and functions
data Gr = Gr {k::Int, n::Int} deriving (Show)

partitionsFor Gr{k=ki, n=ni} =  
    let ps = P.allPartitions' (ki,ni-ki) in
    map (map P.fromPartition) $ filter (/= P.partitions 0) ps

problemFor conds partition =  --There may be a more efficent way to do this
    nub $ map sort $ S.listTensor $ map (conds !!) $ map pred partition

problemsFor g@Gr{k=ki, n=ni} = 
      foldr (++) [] $ map (problemFor $ partitionsFor g) 
                $  P._partitions $ ki*(ni-ki)


-- Game

data Game = Game {i :: Int, reds :: [[Bool]]} deriving(Show)

gameIndices xs = reverse $ filter (/= 0) $ zipWith (*) xs [length xs,length xs - 1..1]
walkP h []     prev xs = xs ++ (replicate (h - prev) 1) 
walkP h (p:ps) prev xs = 
    T.trace (show h ++ " " ++ show p ++ " " ++ show ps ++ " " ++ show prev ++ " " ++ show xs) $
     walkP h ps p (xs ++ (replicate (p - prev) 1) ++ [0])
findCoords h l xs = 
    let prepped = reverse (pad xs l 0)
    in
    gameIndices $ walkP h prepped 0 []



--createGame :: Gr -> P.Partition -> P.Partition -> Game
createGame Gr{k=ki, n=ni} a b = 
    let ac = findCoords ki (ni-ki) a 
        bc = findCoords ki (ni-ki) b
        coords = zip ac (reverse bc) 
    in
      coords


bubbledPairs :: Int -> [((Int,Int),(Int,Int))]
bubbledPairs n =  [((i,y1),(1+x2,y1-1)) 
                   | x2 <- [0..n-1]
                   ,  i <- [0..x2]
                   ,  let y1=n+i-x2 ]


-- Pretty Boxes for printing things 
mark  = char '#'
blank = char ' '
toCol k i = vcat left $ pad (replicate i mark) k blank
condToBox Gr{k=ki,n=ni} cond = hcat left $ map (toCol ki) (pad cond (ni-ki) 0) 
condsToBox g conds = hsep 1 center1 $ map (condToBox gr) conds

gr = Gr 3 6
--main = do printBox $ condsToBox gr $ foldr (++) [] $ problemsFor gr 
main = do printBox $ vsep 1 left $ map (condsToBox gr) $ problemsFor gr
