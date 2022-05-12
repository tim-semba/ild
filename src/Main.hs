module Main where

import Data.List
import Data.Ratio
import Linear.Vector
import qualified Data.Vector as V

ild :: Int -> Int -> [V.Vector Rational]
ild m h = map (V.map (% fromIntegral (m * h))) ilds
  where
    seeds :: [V.Vector Integer]
    seeds = [V.replicate m 0]

    ilds :: [V.Vector Integer]
    ilds = let (centers, edges) = head . drop h $ iterate nextGen (seeds, seeds)
      in nub $ centers ++ edges

    nextGen :: ([V.Vector Integer], [V.Vector Integer]) -> ([V.Vector Integer], [V.Vector Integer])
    nextGen (centers, edges) = (newCenters, newEdges)
      where
        newEdges = nextEdges edges
        newCenters = nub $ newEdges ++ nextCenters centers

    nextEdges, nextCenters :: [V.Vector Integer] -> [V.Vector Integer]
    nextEdges edges = nub [s ^+^ u | s <- edges, u <- modUnitVectors]
    nextCenters = map (^+^ V.replicate m 1)

    modUnitVectors, unitVectors :: [V.Vector Integer]
    modUnitVectors = map (^* fromIntegral m) unitVectors
    unitVectors = [V.generate m (f i) | i <- [0..(m - 1)]]
      where f i n | i == n = 1 | otherwise = 0

main :: IO ()
main = do
  print $ length results
  mapM_ print results
  mapM_ (print . V.map fromRational) results
  where
    results = ild 3 2
