import Control.Monad
import Control.Monad.State

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.Environment
import System.Exit

import SFGen

import Debug.Trace

_trace =
	-- trace
	flip const

-------------------------------------------------------------------------------
-- State of the graph for betweenness centrality computation.

type Index = Int

data G = G {
	-- |Successors and predecessors for each vertex are sets of vertices.
	  gAdjacency		:: !(Map.Map Index (Set.Set Index))
	}
	deriving (Show)

startG :: G
startG = G {
	  gAdjacency		= Map.empty
	}

type GM a = State G a

runG :: GM a -> a
runG = flip evalState startG

edge a b = do
	modify $ \g -> g {
		  gAdjacency = Map.insertWith Set.union a (Set.singleton b) $! gAdjacency g
		}

getCentrBatch :: Int -> GM (Map.Map Index Double)
getCentrBatch batchSize = do
	return $ Map.empty

getCentr :: GM (Map.Map Index Double)
getCentr = do
	adj <- liftM gAdjacency get
	let vertices =
		-- Set.singleton 1
		Map.foldl (Set.union) (Map.keysSet adj) adj
	return $
		foldr (Map.unionWith (+)) Map.empty $
		centralities vertices $ Map.map (\s -> Map.fromAscList $ map (flip (,) 1) $ Set.toList s) adj
	where
		centralities vertices adj = case mbMin of
			Nothing -> []
			Just (v,vertices') -> betwCentr v adj : centralities vertices' adj
			where
				mbMin = Set.minView vertices
		betwCentr v adj = _trace ("adj "++show adj++", v "++show v) $ bfs adj 0 Map.empty (Map.singleton v 1) fFromAdj
			where
				fFromAdj = Map.findWithDefault Map.empty v adj
		bfs adj d s p f
			| Map.null f = _trace ("\nbfs (d: "++show d ++", s: "++show s++", p: "++show p++", f: "++show f++")") $
				gather adj d s p (Map.empty)
			| otherwise = _trace ("\nbfs (d: "++show d ++", s: "++show s++", p: "++show p++", f: "++show f++") -> "++show (d', s', p', f')) $
				bfs adj d' s' p' f'
			where
				d' = d+1
				s' = Map.insert d' (Map.map (const 1) f) s
				p' = Map.unionWith (+) p f
				fA = Map.filter (/=0) $ mulvm f adj
				f' = Map.difference fA p'
		gather adj d s p u
			| d >= 2 = _trace ("\ngather: sd "++show sd++" 1+u "++show uPlus1++", sd.*(1+u) "++show sduPlus1++", w1 "++show w1++", w2 "++show w2++", w3 "++show w3++", u' "++show u') $
				gather adj (d-1) s p u'
			| otherwise = _trace ("------\nu: "++show u) $ u
			where
				sd = Map.findWithDefault Map.empty d s
				uPlus1 = Map.unionWith (+) u $ Map.map (const 1) sd
				sduPlus1 = Map.intersectionWith (*) sd uPlus1
				w1 = Map.intersectionWith (/)
					sduPlus1
					p
				w2 = mulmv adj w1
				sd1 = Map.findWithDefault Map.empty (d-1) s
				w3 = Map.intersectionWith (*) (Map.intersectionWith (*) w2 sd1) p
				u' = Map.unionWith (+) u $ Map.filter (/=0) w3

mulmv :: Map.Map Index (Map.Map Index Double) -> Map.Map Index Double -> Map.Map Index Double
mulmv m v = Map.filter (/=0) $ Map.map (\a -> Map.foldl (+) 0 $ Map.intersectionWith (*) v a) m

mulvm :: Map.Map Index Double -> Map.Map Index (Map.Map Index Double) -> Map.Map Index Double
mulvm v m = Map.filter (/=0) $ Map.foldl (Map.unionWith (+)) Map.empty $
	Map.intersectionWith (\c r -> Map.map (*c) r) v m

-------------------------------------------------------------------------------
-- tests.

-- graph algorithms in linear algebra, page 71.
g1 = [
	  (1, 2)
	, (1, 3)
	, (1, 4)
	, (2, 3)
	, (4, 3)
	, (2, 5)
	, (3, 5)
	, (4, 5)
	, (5, 6)
	, (5, 7)
	, (6, 8)
	, (7, 8)
	]

t1 = runG $ do
	mapM_ (uncurry edge) g1
	getCentr

t = t1

-------------------------------------------------------------------------------
-- Main program.

main = do
	args <- getArgs
	let parse n = case reads n of
		[(a,"")] -> Just (a :: Int)
		[] -> Nothing
	case mapM parse args of
		Just [a,b,c,d,p,e] -> do
			let edges = genSFG a b c d p e
			print $ runG $ mapM_ (uncurry edge) edges >> getCentr
		_ -> do
			putStrLn "usage: ./a a b c d p e\n\na, b, c and d are integers for generating scale-free graphs.\np determines number of vertices = 2^p.\ne is average number of edges per vertex.\nAll parameters are integers. Choose something about tens of thousands for a, b, c and d."
			exitFailure
