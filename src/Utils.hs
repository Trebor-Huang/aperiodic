module Utils where
import Data.List
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M

bfs :: Eq b
  => (a -> Bool) -- Prune if evaluates to False
  -> (a -> b)    -- Nub on function
  -> a           -- Starting point of search
  -> (a -> [(a, i)])  -- Branches at a point, with labels
  -> [(a, [i])]
bfs prune key start branch = searchspace
  where
    searchspace = concat $ takeWhile (not.null) epochs
    epochs = [(start, [])] :
      map (\as -> nubBy ((==) `on` key . fst) $
        [ (a', i:is) |
          (a, is) <- last as,
          prune a,
          (a', i) <- branch a,
          not $ any (elem (key a') . map (key . fst)) as])
      (tail $ inits epochs)
    -- go = nubBy ((==) `on` key) $ start : filter prune (branch =<< go)

type UnionFind s = Map s s
lookupUF :: Ord s => UnionFind s -> s -> s
lookupUF u s = case M.lookup s u of
  Just s' -> lookupUF u s'
  Nothing -> s
addUF :: Ord s => UnionFind s -> s -> s -> UnionFind s
addUF u sdrop skeep = let
    s1 = lookupUF u sdrop
    s2 = lookupUF u skeep
  in if s1 == s2 then u else M.insert s1 s2 u
addUFs :: Ord s => UnionFind s -> [(s, s)] -> UnionFind s
addUFs = foldr (\ (s1, s2) u -> addUF u s1 s2)

withUF :: a -> (a, UnionFind b)
withUF a = (a, M.empty)

fromUF :: (Ord b) => b -> b -> (a, UnionFind b) -> (a, b, b)
fromUF s1 s2 (a, u) = (a, lookupUF u s1, lookupUF u s2)
