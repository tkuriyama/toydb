{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Database.BTreeV2 where

-- Invariants (order m)
-- 1. every non-Null node has min (ceil $ m `div` 2) children
-- 1.a. the root has min 2 children (unless it's a Null)
-- 2. every node has max m children
-- 3. every Null node is on the same level
-- 4. every (non-Null) node with n children has n - 1 keys

import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

data BTree k v where
  BTree :: Order -> Tree h k v -> BTree k v

type Order = Int

data Height = HZero | HSucc Height

data Tree :: Height -> * -> * -> * where
  Empty :: Tree 'HZero k v
  Node :: Tree h k v -> NE.NonEmpty (k, v, Tree h k v) -> Tree ('HSucc h) k v

data InsertResult h k v
  = NoPop (Tree h k v)
  | Pop (Tree h k v) k v (Tree h k v)


insertBT :: Ord k => k -> v -> BTree k v -> BTree k v
insertBT k v (BTree ord Empty) = BTree ord $ Node Empty [(k, v, Empty)]
insertBT k v (BTree ord t@(Node _ _)) =
  case insertTree ord k v t of
    NoPop t' -> BTree ord t'
    Pop t1' k' v' t2' -> BTree ord $ Node t1' [(k', v', t2')]


insertTree
  :: Ord k
  => Order -> k -> v -> Tree ('HSucc h) k v -> InsertResult ('HSucc h) k v
insertTree ord k v node@(Node nt nts) = case k `elem` keys of
  True -> NoPop node  -- no-op
  False -> case nt of
    Empty -> insertLeaf ord k v node
    Node _ _ -> insertSubT ord k v node
  where
    keys = NE.map fst' nts


insertLeaf
  :: Ord k
  => Order
  -> k
  -> v
  -> Tree ('HSucc 'HZero) k v
  -> InsertResult ('HSucc 'HZero) k v
insertLeaf ord k v (Node _ nts) =
  case hasRoom of
    True -> NoPop $ Node Empty nts'
    False -> splitNode ord $ Node Empty nts'
  where
    hasRoom = NE.length nts < ord - 1
    nts' = insertTriple nts (k, v, Empty)


insertSubT
  :: Ord k
  => Order
  -> k
  -> v
  -> Tree ('HSucc ('HSucc h)) k v
  -> InsertResult ('HSucc ('HSucc h)) k v
insertSubT ord k v node@(Node _ nts) =
  case insertTree ord k v (indexT i node) of
    NoPop t' -> NoPop $ replaceIndexT i t' node
    Pop t1' k' v' t2' ->
      let node' = mergeNoPop i node t1' k' v' t2'
      in if hasRoom then NoPop node' else splitNode ord node'
  where
    i = findIndexT k node
    hasRoom = NE.length nts < ord - 1


mergeNoPop
  :: Ord k
  => Int
  -> Tree ('HSucc h) k v
  -> Tree h k v
  -> k
  -> v
  -> Tree h k v
  -> Tree ('HSucc h) k v
mergeNoPop i (Node nt nts) t1 k v t2 = case i of
  0 -> Node t1 $ (k, v, t2) NE.<| nts
  _ -> replaceIndexT i t1 $ Node nt $ insertTriple nts (k, v, t2)


insertTriple
  :: Ord k
  => NE.NonEmpty (k, v, Tree h k v)
  -> (k, v, Tree h k v)
  -> NE.NonEmpty (k, v, Tree h k v)
insertTriple xs x =
  NE.fromList $ L.insertBy (compare `on` fst') x $ NE.toList xs


splitNode
  :: Ord k => Order -> Tree ('HSucc h) k v -> InsertResult ('HSucc h) k v
splitNode ord (Node nt nts) =
  let (nts1, nts2) = NE.splitAt (ord `div` 2) nts
      ((k', v', nt'), nts2') = (head nts2, tail nts2) -- safe!
  in Pop (Node nt $ NE.fromList nts1) k' v' (Node nt' $ NE.fromList nts2')


------------------------ Helpers ------------------------

-- Find index i where search key < tree key, or otherwise index of last
findIndexT :: Ord k => k -> Tree ('HSucc h) k v -> Int
findIndexT k (Node _ nts) = foldl f 0 $ NE.map fst' nts
  where f i k' = if k < k' then i else i + 1

-- Return Tree at index i
indexT :: Ord k => Int -> Tree ('HSucc h) k v -> Tree h k v
indexT i (Node nt nts) = case i of
  0 -> nt
  _ -> trd' $ (NE.!!) nts (i-1)

-- -- Return list of Trees not at index i
-- notIndexT :: Ord k => Int -> Tree k v -> [Tree k v]
-- notIndexT _ Empty = error "Empty tree has no index"
-- notIndexT i (Node nt nts) = case i of
--   0 -> map trd' nts'
--   _ -> nt : (map trd' $ take (i-1) nts') ++ (map trd' $ drop i nts')
--   where
--     nts' = NE.toList nts

-- Replace T at index i
replaceIndexT
  :: Ord k
  => Int -> Tree h k v -> Tree ('HSucc h) k v -> Tree ('HSucc h) k v
replaceIndexT i t (Node nt nts) = case i of
  0 -> Node t nts
  _ -> Node nt $ NE.fromList $ NE.tail $
       NE.map fst $ NE.scanl f (NE.head nts, 1) nts
    where f (_, j) (k', v', t') = let nextT = if j == i then t else t'
                                  in ((k', v', nextT), j+1)

-- toList :: Ord k => Tree k v -> [(k, v)]
-- toList Empty = []
-- toList (Node nt nts) = case nt of
--   Empty -> pairs
--   t -> toList t ++ pairs ++ concatMap toList ts
--   where nts' = NE.toList nts
--         (keys, vals, ts) = (map fst' nts', map snd' nts', map trd' nts')
--         pairs = zip keys vals

-- fromList :: Ord k => Order -> [(k, v)] -> Tree b k v
-- fromList ord = tree . L.foldr f (emptyBT ord)
--   where f (k, v) t = insertBT k v t

maxT :: Ord k => Tree ('HSucc h) k v -> k
maxT (Node _ nts) = maximum $ NE.map fst' nts

heightT :: Ord k => Tree h k v -> Int
heightT Empty = 0
heightT (Node nt _) = 1 + heightT nt


fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd' :: (a, b, c) -> c
trd' (_, _, z) = z
