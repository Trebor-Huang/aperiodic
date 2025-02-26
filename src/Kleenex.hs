{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Kleenex where

import KMC.SymbolicFST (FST(..))
import qualified KMC.SymbolicFST as K.FST
import qualified KMC.SymbolicFST.Functionalization as K.FST
import qualified KMC.SymbolicSST as K.SST
import qualified KMC.Determinization as K
import qualified KMC.Theories as K
import Data.Set (Set)
import qualified Data.Set as S
import Automata
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Substitution

data SingFunc a b = Sing a b deriving (Eq, Show, Ord, Functor)
instance Eq a => K.Function (SingFunc a b) where
  type Dom (SingFunc a b) = a
  type Rng (SingFunc a b) = [b]
  eval (Sing _ b) = const [b]
  isConst (Sing _ b) = Just [b]
  inDom a' (Sing a _) = a == a'
  domain (Sing a _) = [a]

instance Ord a => K.PartialOrder (Set a) where
  lte = S.isSubsetOf

instance (Enum a, Bounded a, Ord a) => K.Boolean (Set a) where
  top = S.fromDistinctAscList [minBound..maxBound]
  bot = S.empty
  neg x = K.top S.\\ x
  conj = S.intersection
  disj = S.union

instance Ord a => K.SetLike (Set a) a where
  member = S.member

instance Ord a => K.Enumerable (Set a) a where
  indexOf x s = fromJust (elemIndex x (S.toAscList s))
  lookupIndex i s = S.toAscList s !! i
  size = S.size

toKMC :: (Enum input, Ord input, Bounded input, Eq output)
  => (FSTInt (Maybe input) (Maybe output), Int)
  -> K.SST.SST Int (Set input) (K.SSTFunc (SingFunc input output)) Int
toKMC (machine, start) =
  let
    sts = S.fromDistinctAscList [0..stateInt machine-1]
    machine' = K.FST.FST {
      fstS = sts,
      fstE =
        K.FST.edgesFromList $
        map (\case
          (s, Just i, Just o, s') -> (s, Left (S.singleton i, Sing i o), s')
          (s, Nothing, Nothing, s') -> (s, Right [], s')
          _ -> error "Impossible"
        ) $
        transitionsInt machine,
      fstI = start,
      fstF = sts
    }
  in
  K.SST.enumerateVariables $
  K.SST.enumerateStates $
  fst $ K.SST.optimize 3 $
  K.sstFromFST (
    K.FST.functionalize
    machine'
  ) True

flattenStreamUnsafe :: K.SST.Stream [m] -> [m]
flattenStreamUnsafe (K.SST.Fail e) = error e
flattenStreamUnsafe K.SST.Done = []
flattenStreamUnsafe (K.SST.Chunk x xs) = x ++ flattenStreamUnsafe xs

adjSST :: (Bounded edge, Bounded tile, Bounded subtile, Ord edge, Ord tile, Ord subtile, Enum tile, Enum subtile, Enum edge)
  => K.SST.SST Int (Set (Alphabet tile subtile edge)) (K.SSTFunc (SingFunc (Alphabet tile subtile edge) (Alphabet tile subtile edge))) Int
  -> Signature tile subtile -> edge -> Maybe (Signature tile subtile, edge)
adjSST machine sig e = Just $
  alphabetToSig $
  flattenStreamUnsafe $
  K.SST.run machine $
  sigToAlphabet (sig, e)

