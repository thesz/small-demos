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

genSFG :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
genSFG a b c d pow edges = map snd $ take nEdges $ tail $ iterate (uncurry gen) (map ((`mod` abcd) . (`mod` (abcd*12+1))) randoms,error "not an edge")
	where
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
		gen rs _ = edge 0 0 (div nVertices 2) rs
		tuple (a:b:xs) = (a,b) : tuple xs
		randoms = drop 10 $ iterate ((+1) . (*0x8088405)) 24101971
		nVertices = 2^pow
		nEdges = edges * nVertices
