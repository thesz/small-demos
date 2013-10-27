-- |SFGen.hs
--
-- Scalefree graph generation.
--
-- Copyright (C) 2013 Serguey Zefirov

module SFGen where

import Control.Monad
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

import Debug.Trace

genSFG :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
genSFG a b c d pow edges = map randomize $ map snd $ take nEdges $ tail $ iterate (uncurry gen) (map ((`mod` abcd)) randoms,error "not an edge")
	where
		startTranspose = Map.fromAscList [(i,i) | i <- [0..nVertices-1]]
		swap i j' transposition = r
			where
				j = j' `mod` (i+1)
				r = Map.insert i y $ Map.insert j x transposition
				x = Map.findWithDefault undefined i transposition
				y = Map.findWithDefault undefined j transposition
		bigPrime = 2^30 - 35
		swapped = foldr (uncurry swap) startTranspose $ zip [0..nVertices-1] randoms2
		randomize (a,b) = (transp a, transp b)
			where
				transp a = maybe (error "no index?") id $ Map.lookup a swapped
		abcd = a+b+c+d
		aLow = b+c+d
		bLow = c+d
		cLow = d
		dLow = 0
		edge x y 0 rs = (rs,(x,y))
		edge x y m (r:rs)
			| r >= aLow = edge x y m' rs
			| r >= bLow = edge (x+m) y m' rs
			| r >= cLow = edge x (y+m) m' rs
			| otherwise = edge (x+m) (y+m) m' rs
			where
				m' = div m 2
		gen :: [Int] -> (Int, Int) -> ([Int], (Int, Int))
		gen rs _ = edge 0 0 (div nVertices 2) rs
		tuple (a:b:xs) = (a,b) : tuple xs
		rnd :: Int -> Int
		rnd = (`mod` bigPrime) . (+1) . (*0x8088405)
		randoms = drop 10 $ iterate rnd 24101971
		randoms2 = drop 10 $ iterate rnd 11223344
		nVertices = 2^pow
		nEdges = edges * nVertices
