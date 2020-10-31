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
insertT k v (BTree ord t) = BTree ord $
  case insertTree ord k v t of
    NoPop t' -> t'
    Pop t1' k' v' t2' -> Node t1' [(k', v', t2')]

insertTree :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertTree _     k v Empty = NoPop $ Node Empty [(k, v, Empty)]
insertTree ord k v node@(Node nt nts) = case k `elem` keys of
  True -> NoPop node  -- no-op
  False -> case nt of  
    Empty -> insertLeaf ord k v node
    (Node _ _) -> insertSubT ord k v node
  where    
    keys = NE.map fst' nts

insertLeaf :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertLeaf ord k v (Node _ nts) =
  case hasRoom of
    True -> NoPop $ Node Empty nts'
    False -> let (nts1, nts2) = NE.splitAt (ord `div` 2) nts'
                 ((k', v', _), nts2') = (head nts2, tail nts2) -- safe!
             in Pop (toNode nts1) k' v' (toNode nts2')
  where
    hasRoom = NE.length nts < ord - 1
    nts' = insertTriple nts (k, v, Empty)
    toNode xs = Node Empty $ NE.fromList xs
    
insertSubT :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertSubT ord k v node@(Node nt nts) =
  case insertTree ord k v (indexT i node) of
    NoPop t' -> NoPop $ replaceIndexT i t' node
    Pop t1' k' v' t2' ->
      if hasRoom then
        mergeNoPop i node t1' k' v' t2' 
      else
        mergePop node t1' k' v' t2' 
  where
    i = findIndexT k node
    hasRoom = NE.length nts < ord - 1

mergeNoPop :: Ord k => Int -> Tree k v -> Tree k v -> k -> v -> Tree k v ->
            InsertResult k v
mergeNoPop i (Node nt nts) t1 k v t2 = case i of
  0 ->  NoPop $ Node t1 $ (k, v, t2) NE.<| nts
  _ -> NoPop $ replaceIndexT i t1 $ Node nt $ insertTriple nts (k, v, t2)

mergePop :: Ord k => Tree k v -> Tree k v -> k -> v -> Tree k v ->
            InsertResult k v
mergePop = undefined

insertTriple :: Ord k => NE.NonEmpty (k, v, Tree k v) -> (k, v, Tree k v) ->
                NE.NonEmpty (k, v, Tree k v)
insertTriple xs x = NE.fromList $
                    L.insertBy (compare `on` fst') x $ NE.toList xs

------------------------ Helpers ------------------------

-- Find index i where search key < tree key, or otherwise index of last
findIndexT :: Ord k => k -> Tree k v -> Int
findIndexT k (Node _ nts) = L.foldl f 0 $ NE.toList $ NE.map fst' nts
  where f i k' = if k < k' then i else i + 1

-- Return Tree at index i
indexT :: Ord k => Int -> Tree k v -> Tree k v
indexT i (Node nt nts) = case i of
  0 -> nt
  _ -> trd' $ (NE.!!) nts (i-1)

-- Replace T at index i
replaceIndexT :: Ord k => Int -> Tree k v -> Tree k v -> Tree k v
replaceIndexT i t (Node nt nts) = case i of
  0 -> Node t nts
  _ -> Node nt $ NE.fromList $ NE.tail $
       NE.map fst $ NE.scanl f (NE.head nts, 1) nts
    where f (_, j) (k', v', t') = let nextT = if j == i then t else t'
                                  in ((k', v', nextT), j+1)
    
fst' :: (a, b, c) -> a
fst' (x, _, _) = x

snd' :: (a, b, c) -> b
snd' (_, y, _) = y

trd' :: (a, b, c) -> c
trd' (_, _, z) = z

empty :: Order -> BTree k v
empty n = BTree n Empty

showT :: (Show k, Show v) => BTree k v -> String
showT (BTree ord t) = "Order: " ++ show ord ++ "\n" ++ showTree 0 t

showTree :: (Show k, Show v) => Int -> Tree k v -> String
showTree _ Empty = "" 
showTree n (Node nt nts) = 
  (concat $ replicate n "    ") ++
  show (NE.toList $ NE.zip keys vals) ++
  "\n" ++ L.concatMap (showTree (n+1)) (NE.toList $ nt NE.<| nts')
  where (keys, vals, nts') = ( NE.map fst' nts
                             , NE.map snd' nts
                             ,NE.map trd' nts )
