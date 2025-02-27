{-# LANGUAGE TupleSections #-}
module Automata where
import Data.List (nub)
import Data.Function ((&))
import Control.Monad (guard)
import Data.Maybe ( fromJust, mapMaybe, isJust, listToMaybe )
import Utils
import Substitution
import qualified Data.Set as S
import qualified Data.Map as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS


-- No epsilon-transition is allowed
data FST input output state = FST {
  states :: S.Set state,
  transitions :: ![(state, input, output, state)]
} deriving Show

productFST :: (Eq i) => FST i o1 s1 -> FST i o2 s2 -> FST i (o1, o2) (s1, s2)
productFST a1 a2 = FST {
  states = S.cartesianProduct (states a1) (states a2),
  transitions = do
    (s1, i1, o1, s1') <- transitions a1
    (s2, i2, o2, s2') <- transitions a2
    guard (i1 == i2)
    return ((s1, s2), i1, (o1, o2), (s1', s2'))
}

mapMachine :: (Ord s', Eq i, Eq o) => (s -> s') -> FST i o s -> FST i o s'
mapMachine f machine = FST {
  states = S.map f (states machine),
  transitions = nub $ map (\(s, i, o, s') -> (f s, i, o, f s')) $ transitions machine
}

mapMonotonic :: (s -> s') -> FST i o s -> FST i o s'
mapMonotonic f machine = FST {
  states = S.mapMonotonic f (states machine),
  transitions = map (\(s, i, o, s') -> (f s, i, o, f s')) $ transitions machine
}

-- | Output a machine that has epsilon transitions
-- but has no read fan-outs
normalizeMachine :: (Ord s, Ord i, Eq o)
  => (FST i o s, s) -> (FST (Maybe i) (Maybe o) (Either s (s, i, s)), Either s (s, i, s))
normalizeMachine (machine, start) =
  let
    st = S.disjointUnion (states machine) $ S.fromList $
      map (\(s, i, _, s') -> (s, i, s')) $ transitions machine
  in
    (FST {
      states = st,
      transitions = nub $ do
        (s, i, o, s') <- transitions machine
        [(Left s, Nothing, Nothing, Right (s, i, s')),
          (Right (s, i, s'), Just i, Just o, Left s')]
    }, Left start)

data FSTInt input output = FSTInt {
  stateInt :: !Int,
  transitionsInt :: ![(Int, input, output, Int)]
} deriving Show

numberStates :: Eq state
  => (FST input output state, state)
  -> (FSTInt input output, Int)
numberStates (automata, start) =
  let
    table = fromJust . (`lookup` zip (S.toAscList $ states automata) [0..])
    automata' = mapMonotonic table automata
  in
    (FSTInt {
      stateInt = S.size (states automata),
      transitionsInt = transitions automata'
    }, table start)

unnumberStates :: FSTInt input output -> FST input output Int
unnumberStates automata = FST {
  states = S.fromDistinctAscList [0..stateInt automata-1],
  transitions = transitionsInt automata
}

-- | Finds accessible states, output the paths that reaches
accessible :: (Eq input, Eq state)
  => FST input output state
  -> state
  -> [(state, [input])]
accessible automata start = bfs
  (const True) id start
  (\st -> [(s2, i) | (s1, i, _, s2) <- transitions automata, s1 == st])

type FSA input state = FST input () state

forgetOutput :: FST input output state -> FSA input state
forgetOutput automata = FST {
  states = states automata,
  transitions = map (\(s, i, _, s') -> (s, i, (), s')) $ transitions automata
}

transpose :: FST input output state -> FST output input state
transpose automata = FST {
  states = states automata,
  transitions = map (\(s, o, i, s') -> (s, i, o, s')) $ transitions automata
}

determinize :: (Ord input)
  => (FSTInt input (), Int)
  -> (FSA input IntSet, IntSet)
determinize (nfa, start) = let
    start' = IS.singleton start
    states = S.fromList $ map fst $ bfs (not . IS.null) id start'
      (map ((,()) . snd) . getTransitions)
    transitions = [(s, i, (), s') | s <- S.toList states, (i, s') <- getTransitions s]
  in
    (FST {..}, start')
  where
    -- getTransitions :: IntSet -> [(input, IntSet)]
    getTransitions ss =
      M.toList $ M.fromListWith IS.union [(i, IS.singleton s2) |
          s <- IS.toList ss,
          (s1, i, _, s2) <- transitionsInt nfa,
          s == s1
        ]

naiveSimulate :: (Eq input, Eq state)
  => FST input output state
  -> [input] -> state -> [([output], state)]
naiveSimulate _ [] s = return ([], s)
naiveSimulate automata (i:is) s = do
  (s1, i', o', s2) <- transitions automata
  guard (s1 == s && i' == i)
  (os, sf) <- naiveSimulate automata is s2
  return (o':os, sf)

naiveGenerate :: (Eq output, Eq state)
  => FSA output state
  -> state -> (state -> Bool)
  -> [([output], state)]
naiveGenerate automata start acc = do
  n <- [0..]
  (os, st) <- naiveSimulate (transpose automata) (replicate n ()) start
  guard (acc st)
  return (os, st)

addStates :: (Ord state)
  => S.Set state
  -> FST input output state
  -> FST input output state
addStates s transducer = if any (`elem` states transducer) s then
    error "Duplicate state added"
  else
    FST {
      states = S.union s $ states transducer,
      transitions = transitions transducer
    }

removeState :: (Ord state)
  => state
  -> FST input output state
  -> FST input output state
removeState s transducer = FST {
  states = S.delete s (states transducer),
  transitions = filter
    (\(s1, _, _, s2) -> s1 /= s && s2 /= s)
    (transitions transducer)
}

irreversible :: (Eq input, Eq state)
  => FSA input state -> [(state, state)]
irreversible machine = do
  (s1, i, _, s2) <- transitions machine
  (s1', i', _, s2') <- transitions machine
  guard (i == i')
  case (s1 == s1', s2 == s2') of
    (True, True) -> mempty
    (True, False) -> return (s2, s2')
    (False, True) -> return (s1, s1')
    (False, False) -> mempty

mergeStates :: (Eq input, Ord state)
  => (FSA input state, UnionFind state)
  -> (FSA input state, UnionFind state)
mergeStates (machine, uf) =
  case irreversible machine of
    [] -> (machine, uf)
    rs -> let uf' = addUFs uf rs in
      mergeStates (mapMachine (lookupUF uf') machine, uf')

addTransitions :: (Eq input, Ord state)
  => [(state, input, state)]
  -> FSA input state
  -> (FSA input state, UnionFind state)
addTransitions trans machine =
  mergeStates $ withUF FST {
    states = states machine,
    transitions = map (\(x,y,z) -> (x,y,(),z)) trans ++ transitions machine
  }

induceExample :: (Eq input, Ord state)
  => [input]
  -> (Int -> [input] -> state) -- fresh state generator
  -> (FSA input state, state, state)
  -> (FSA input state, state, state)
induceExample ex fresh (automata, start, end) =
  let new = map (`fresh` ex) [1..length ex - 1] in
  automata &
  addStates (S.fromDistinctAscList new) &
  addTransitions (zip3 (start : new) ex (new ++ [end])) &
  fromUF start end

-- | Given a (deterministic) transducer T and acceptor A, test whether
-- the language of A is contained in the domain of T
missing :: (Eq input, Eq state, Eq state')
  => FSA input state
  -> FSA input state'
  -> state -> state'
  -> [[input]]
missing fun acc start start' = do
  let outreach = accessible (productFST (forgetOutput fun) acc) (start, start')
  ((st, st'), inputs) <- outreach
  (st0', i, _, _) <- transitions acc
  guard (st' == st0')
  guard $ not $ any (\(st1, i1, _, _) -> i1 == i && st1 == st)
    (transitions fun)
  return $ reverse $ i : inputs

data FreshStates input
  = FreshST !Int [input]
  | StartST | EndST
  deriving (Show, Eq, Ord, Functor)

fsaToFst
  :: FSA (AugAlphabet tile subtile edge) state
  -> FST (Alphabet tile subtile edge) (Alphabet tile subtile edge) state
fsaToFst automata = FST {
  states = states automata,
  transitions = mapMaybe adjoint $ transitions automata
}
  where
    adjoint (s1, Begin2 t1 e1 t2 e2, _, s2) =
      Just (s1, Begin t1 e1, Begin t2 e2, s2)
    adjoint (s1, Inflate2 t1 ts1 t2 ts2, _, s2) =
      Just (s1, Inflate t1 ts1, Inflate t2 ts2, s2)
    adjoint (_, Accept _, _, _) = Nothing
    -- Maybe we can add a constructor for this, but it won't be actually used anyway


induceFromFunction :: (Ord edge, Ord tile, Ord subtile)
  -- The recursive recognizer
  => (Signature tile subtile -> edge -> Maybe (Signature tile subtile, edge))
  -- The valid string recognizer
  -> FSA (Alphabet tile subtile edge) (Maybe tile)
  -> (FST (Alphabet tile subtile edge) (Alphabet tile subtile edge)
    (FreshStates (AugAlphabet tile subtile edge)), FreshStates (AugAlphabet tile subtile edge))
induceFromFunction baseline valid = go (FST {
    states = S.fromDistinctAscList [StartST, EndST],
    transitions = []
  }, StartST, EndST)
  where
    -- Given the prefix, find a full input/output pair
    getExample prefix = do
      (_, t) <- naiveSimulate valid prefix Nothing
      (suffix, _) <- naiveGenerate valid t isJust
      let input = prefix ++ suffix
      -- check if this actually works
      Just sig <- [uncurry baseline (alphabetToSig input)]
      return (zipSigs input $ sigToAlphabet sig)

    -- Find missing prefixes for the transducer, and product an example
    getMissing (machine, start) =
      listToMaybe $ getExample =<< missing machine valid start Nothing

    -- Given an automata, improve it if it can
    improve a@(automata, start, _) = do
      example <- getMissing $
        determinize $
        numberStates (forgetOutput $ fsaToFst automata, start)
      return $ induceExample example FreshST a

    -- Iterate the improvement
    go a@(automata, start, _) = maybe (fsaToFst automata, start) go (improve a)

-- | Naively simulates the transducer. It is not streaming.
adjFST :: (Eq edge, Eq tile, Eq subtile, Eq state)
  => (FST (Alphabet tile subtile edge) (Alphabet tile subtile edge) state, state)
  -> Signature tile subtile -> edge -> Maybe (Signature tile subtile, edge)
adjFST (machine, start) sig e = listToMaybe $ do
  (word, _) <- naiveSimulate machine (sigToAlphabet (sig, e)) start
  return (alphabetToSig word)

