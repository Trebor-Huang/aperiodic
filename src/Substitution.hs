{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Substitution where

import Data.Tuple (swap)

-- | The type of perfect pairings on the type, i.e.
-- an involution with no fixpoints.
type Pairing a = a -> a

-- | Auxiliary function to help construct pairs
fromPairs :: (Show a, Eq a) => [(a, a)] -> Pairing a
fromPairs p a = case a `lookup` p of
  Just b -> b
  Nothing -> case a `lookup` map swap p of
    Just b -> b
    Nothing -> error $ "Bad pairing: " <> show a <> " within " <> show p

data SubstSystem tile subtile edge subedge stage
  = SubstSystem {
    -- | Records the tile type of each subtile
    subtile :: tile -> subtile -> tile,

    -- | What stage does a subtile label lie in
    stageMap :: subtile -> stage,

    -- | In a substitution rule, the subedges of the parent
    -- and all the edges of the children are perfectly paired.
    substMap :: stage -> tile -> Pairing (Either (edge, subedge) (subtile, edge))
  }

-- | Conway signature for tiles.
-- Notice it goes from small to large:
--
-- > Signature t0 [(t1,s1),(t2,s2),...]
--
-- represents tile type `t0` that is the `s1`-th subtile of
-- the tile `t1`, which is the `s2`-th subtile of...
data Signature tile subtile = tile :< [(tile, subtile)] deriving (Eq, Show)
infix 4 :<

data Alphabet tile subtile edge
  = Begin !tile !edge
  | Inflate !tile !subtile
  deriving (Show, Eq, Ord)

instance (Bounded tile, Bounded subtile, Bounded edge)
  => Bounded (Alphabet tile subtile edge) where
  minBound = Begin minBound minBound
  maxBound = Inflate maxBound maxBound

instance forall tile subtile edge. (Bounded tile, Bounded subtile, Bounded edge, Enum tile, Enum subtile, Enum edge)
  => Enum (Alphabet tile subtile edge) where
  toEnum n =
    let
      tm = (1 + fromEnum (maxBound :: tile))
      em = (1 + fromEnum (maxBound :: edge))
      sm = (1 + fromEnum (maxBound :: subtile))
    in
      if n < tm * em then
        Begin (toEnum (n `div` em)) (toEnum (n `mod` em))
      else let n' = n - tm * em in
        Inflate (toEnum (n' `div` sm)) (toEnum (n' `mod` sm))
  fromEnum (Begin t e) = fromEnum e +
    fromEnum t * (1 + fromEnum (maxBound :: edge))
  fromEnum (Inflate t s) = fromEnum s +
    fromEnum t * (1 + fromEnum (maxBound :: subtile)) +
    (1 + fromEnum (maxBound :: tile)) * (1 + fromEnum (maxBound :: edge))

sigToAlphabet
  :: (Signature tile subtile, edge)
  -> [Alphabet tile subtile edge]
sigToAlphabet (t :< ts, e) = Begin t e : map (uncurry Inflate) ts

alphabetToSig
  :: [Alphabet tile subtile edge]
  -> (Signature tile subtile, edge)
alphabetToSig (Begin t e : word) = (t :< map fromInflate word, e)
  where
    fromInflate (Inflate t0 s0) = (t0, s0)
    fromInflate _ = error "Invalid letter"
alphabetToSig _ = error "Invalid string"

data AugAlphabet tile subtile edge
  = Begin2 !tile !edge !tile !edge
  | Inflate2 !tile !subtile !tile !subtile
  | Accept !tile
  deriving (Show, Eq, Ord)

zipSigs :: Eq tile
  => [Alphabet tile subtile edge]
  -> [Alphabet tile subtile edge]
  -> [AugAlphabet tile subtile edge]
zipSigs [] _ = error "Empty string"
zipSigs _ [] = error "Empty string"
zipSigs [Inflate t1 e1] [Inflate t2 e2] =
  if t1 == t2 then
    [Inflate2 t1 e1 t2 e2, Accept t1]
  else
    error "Tile does not match"
zipSigs (Inflate t1 e1: ts1) (Inflate t2 e2: ts2)
  = Inflate2 t1 e1 t2 e2 : zipSigs ts1 ts2
zipSigs (Begin t1 e1: ts1) (Begin t2 e2: ts2)
  = Begin2 t1 e1 t2 e2 : zipSigs ts1 ts2
zipSigs _ _ = error "Invalid signatures"

-- | The recursive function for calculating neighborhood
adjRec
  :: SubstSystem tile subtile edge subedge stage
  -> Signature tile subtile -> edge
  -> Maybe (Signature tile subtile, edge)
  -- ^ Outputs which edge it is entering from
adjRec _ (_ :< []) _ = Nothing
adjRec sys (_ :< (t1,s1):sigtail) e =
  let stage = stageMap sys s1 in
  case substMap sys stage t1 (Right (s1, e)) of
    Right (s1', e') -> do
      -- We are inside the original parent tile
      let t0' = subtile sys t1 s1'
      return (t0' :< (t1,s1'):sigtail, e')
    Left (eo, se') -> do
      -- We step out of the parent tile, on the subedge (e', se')
      (sig, eo') <- adjRec sys (t1 :< sigtail) eo
      enterRec sys stage sig (eo', se')

-- | Recursively calculate the result of entering from an external subedge
enterRec
  :: SubstSystem tile subtile edge subedge stage
  -> stage -> Signature tile subtile
  -> (edge, subedge)
  -> Maybe (Signature tile subtile, edge)
enterRec sys stage (t1 :< sigtail) (e, se) =
  case substMap sys stage t1 (Left (e, se)) of
    Right (s1', e') -> do
      -- We step into a subtile
      let t0' = subtile sys t1 s1'
      return (t0' :< (t1,s1'):sigtail, e')
    Left (eo, se') -> do
      -- We're stepping out again!!
      (sig, eo') <- adjRec sys (t1 :< sigtail) eo
      enterRec sys stage sig (eo', se')
