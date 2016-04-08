{-# OPTIONS -fno-warn-tabs #-}
{-# LANGUAGE GADTs, TypeOperators, TypeFamilies, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternGuards, UndecidableInstances, StandaloneDeriving #-}

module Tree where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Monoid

import qualified Data.Vector as V

import Test.QuickCheck

data Map k a = Map (Map.Map k a)
	deriving (Eq, Ord, Show)

mapEmpty :: Map k a
mapEmpty = Map Map.empty

mapDifferenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
mapDifferenceWith f (Map a) (Map b) = Map $ Map.differenceWith f a b

mapDifference :: Ord k => Map k a -> Map k b -> Map k a
mapDifference (Map a) (Map b) = Map $ Map.difference a b

mapUnion :: Ord k => Map k a -> Map k a -> Map k a
mapUnion (Map a) (Map b) = Map $ Map.union a b

mapUnionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnionWith f (Map a) (Map b) = Map $ Map.unionWith f a b

class Monoid (Delta a) => Trans a where
	data Delta a

	trans :: a -> Delta a -> a

instance (Ord k, Trans a) => Trans (Map k a) where
	data Delta (Map k a) =
		-- insert, delete and change.
		-- all are disjoint.
		DMap	(Map k a)	(Map k ())	(Map k (Delta a))
	trans map (DMap ins del change) =
		mapDifferenceWith (curry $ Just . uncurry trans ) (mapDifference (mapUnion map ins) del) change

instance (Ord k, Trans a) => Monoid (Delta (Map k a)) where
	mempty = DMap mapEmpty mapEmpty mapEmpty

	mappend (DMap ins1 del1 chg1) (DMap ins2 del2 chg2) =
		DMap ins1del2chg2ins2 del1ins2del2 chg1chg2
		where
			ins1del2 = mapDifference ins1 del2
			ins1del2chg2 = mapDifferenceWith (curry $ Just . uncurry trans) ins1del2 chg2
			ins1del2chg2ins2 = mapUnion ins1del2chg2 ins2
			del1ins2 = mapDifference del1 ins2
			del1ins2del2 = mapUnion del1ins2 del2
			ins1del2ins2 = mapUnion ins1del2
			ins = mapUnion (mapDifference ins1 del2) ins2
			chg1del2 = mapDifference chg1 del2
			chg1del2ins2 = mapDifference chg1del2 ins2
			chg1chg2 = mapUnionWith mappend chg1del2ins2 chg2

data ListOp a = Ins a | Del | Chg (Delta a)

deriving instance (Show a, Show (Delta a)) => Show (ListOp a)

deriving instance (Show a, Show (Delta a)) => Show (Delta [a])

instance (Trans a) => Trans [a] where
	data Delta [a] =
		DList	[(Int, ListOp a)]
	trans list (DList ops) = go ops list
		where
			-- no elements to process - performing remaining inserts
			go [] list = list
			go [(d,Ins x)] []
				| d == 0 = [x]
				| otherwise = error $ "insert past end: "++show d
			go ((d,Ins x):(d',op):ds) []
				| d == 0 = x : go ((d',op):ds) []
			go _ [] = error "only insertions are allowed past the end."
			go ((d,op):ds) xs = skip ++ case op of
				Ins x -> x : go ds list
				Del -> go ds es
				Chg y -> trans e y : go ds es
				where
					(skip, list) = splitAt d xs
					(e:es) = list

instance Trans a => Monoid (Delta [a]) where
	mempty = DList []

	mappend (DList opsa) (DList opsb) = DList $ go opsa opsb
		where
			{-# OPTIONS -fwarn-incomplete-patterns #-}
			go [] opsb = opsb
			go opsa [] = opsa
			go ((da,opa):opsa) ((db,opb):opsb)
				| da < db = (da,opa) : go opsa ((db-da, opb) : opsb)
				| da > db = (db,opb) : go ((da-db, opa) : opsa) opsb
				| otherwise = case (opa, opb) of
					(Ins a, Ins b) -> (da, opb) : go ((0,opa) : opsa) opsb
					(Ins a, Del) -> go opsa opsb
					(Ins a, Chg d) -> (da, Ins (trans a d)) : go opsa opsb
					(Del, _) -> (da, Del) : go opsa ((0, opb) : opsb)
					(Chg x, Ins b) -> (da, opb) : go ((0, opa) : opsa) opsb
					(Chg x, Del) -> go opsa opsb
					(Chg x, Chg y) -> (da, Chg $ mappend x y) : go opsa opsb

data C = C Char
	deriving (Eq, Ord, Show)

deriving instance Show (Delta C)

instance Trans C where
	data Delta C = CChange Char Char

	trans (C x) (CChange a b)
		| a == x = C b
		| otherwise = error $ "incorrect delta dummy: "++show (x,a,b)

instance Monoid (Delta C) where
	mempty = error "delta dummy mempty"
	mappend (CChange a b) (CChange c d)
		| b == c = CChange a d
		| otherwise = error $ "incorrect chain of deltas: "++show (a,b,c,d)


-------------------------------------------------------------------------------
-- properties.

singlify :: Trans a => Delta [a] -> [Delta [a]]
singlify (DList ds) = go 0 ds
	where
		go _ [] = []
		go i ((j,op):ds) = o : case op of
			Ins x -> go (ij+1) ds
			Del -> go (ij-1) ds
			Chg a -> go ij ds
			where
				ij = i+j
				o = DList [(ij, op)]

instance Arbitrary C where
	arbitrary = fmap C arbitrary

inserts :: [C] -> [Delta [C]]
inserts = map (\c -> DList [(0, Ins c)])

prop_inserts0 :: [C] -> Bool
prop_inserts0 list = trans [] (mconcat $ inserts list) == reverse list

prop_del :: Int -> [C] -> Bool
prop_del n l
	| n < 0 || n >= length l = True
	| otherwise = trans l (DList [(n, Del)]) == (take n l ++ tail (drop n l))

ti0 = verboseCheck prop_inserts0
t = verboseCheck prop_del