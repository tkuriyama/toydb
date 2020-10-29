{-# LANGUAGE OverloadedLists #-}
module Database.BTree where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

data BTree k v = BTree
  { order :: Order
  , tree :: Tree k v
  }
  deriving (Show, Eq)

type Order = Int

data Tree k v
  = Empty
  | Node (Tree k v) (NE.NonEmpty (k, v, Tree k v))
  deriving (Show, Eq)
  
data InsertResult k v
  = NoPop (Tree k v)
  | Pop (Tree k v) k v (Tree k v)
  deriving (Show)

------------------------ Insert ------------------------ 

insertT :: Ord k => k -> v -> BTree k v -> BTree k v
insertT k v (BTree order tree) = BTree order $
  case insertTree order k v tree of
    NoPop t -> t
    Pop t1 k v t2 -> Node t1 [(k, v, t2)]

insertTree :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertTree _     k v Empty = NoPop $ Node Empty [(k, v, Empty)]
insertTree order k v node@(Node nt nts) = case k `elem` keys of
  True -> NoPop node  -- no-op
  False -> case nt of  
    Empty -> insertToLeaf order k v node
    subTree@(Node _ _) -> insertToSubT order k v node
  where    
    keys = NE.map fst' nts

insertToLeaf :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertToLeaf order k v (Node _ nts) =
  case NE.length nts < order - 1 of
    True -> NoPop $ Node Empty nts'
    False -> let (nts1, nts2) = NE.splitAt (order `div` 2) nts'
                 ((k, v, _), nts2') = (head nts2, tail nts2)
             in Pop (toNode nts1) k v (toNode nts2')
  where   
    nts' = NE.fromList $
           L.insertBy (compare `on` fst') (k, v, Empty) $ NE.toList nts
    toNode xs = Node Empty $ NE.fromList xs
    
insertToSubT :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertToSubT order  k v node@(Node nt nts) =
  if k < k1 then
      case insertTree order k v node of
        NoPop t' -> NoPop $ Node t' nts
        Pop t1' k' v' t2' ->
          if NE.length nts < order - 1 then
            NoPop $ Node t1' ((k', v', t2') NE.<| nts)
          else
            undefined
  else 
    undefined
  where
    (k1, v1, t1) = NE.head nts

------------------------ Helpers ------------------------

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd' :: (a, b, c) -> c
trd' (_, _, z) = z

empty :: Order -> BTree k v
empty n = BTree n Empty

showT :: (Show k, Show v) => BTree k v -> String
showT (BTree order tree) = "Order: " ++ show order ++ "\n" ++ showTree 0 tree

showTree :: (Show k, Show v) => Int -> Tree k v -> String
showTree n Empty = "" -- (concat $ replicate n "--") ++ " E\n"
showTree n (Node nt nts) = 
  (concat $ replicate n "    ") ++
  show (NE.toList $ NE.zip keys vals) ++
  "\n" ++ L.concatMap (showTree (n+1)) (NE.toList $ nt NE.<| nts')
  where (keys, vals, nts') = ( NE.map fst' nts
                             , NE.map snd' nts
                             ,NE.map trd' nts )
