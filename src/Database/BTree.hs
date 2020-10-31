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

------------------------ Seach ------------------------ 

findT :: Ord k => k -> BTree k v -> Maybe v
findT k (BTree ord t) = case t of
  Empty -> Nothing
  (Node _ nts) ->
    if k `elem` keys then
      Just $ snd' $ head $ NE.dropWhile (\(k', _, _) -> k' /= k) nts
    else
      findT k $ BTree ord $ indexT (findIndexT k t) t
    where keys = NE.map fst' nts

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
    False -> splitNode ord $ Node Empty nts'
  where
    hasRoom = NE.length nts < ord - 1
    nts' = insertTriple nts (k, v, Empty)
    
insertSubT :: Ord k => Order -> k -> v -> Tree k v -> InsertResult k v
insertSubT ord k v node@(Node _ nts) =
  case insertTree ord k v (indexT i node) of
    NoPop t' -> NoPop $ replaceIndexT i t' node
    Pop t1' k' v' t2' ->
      let (NoPop node') = mergeNoPop i node t1' k' v' t2'
      in if hasRoom then NoPop node' else splitNode ord node'
  where
    i = findIndexT k node
    hasRoom = NE.length nts < ord - 1

mergeNoPop :: Ord k => Int -> Tree k v -> Tree k v -> k -> v -> Tree k v ->
            InsertResult k v
mergeNoPop _ Empty _ _ _ _ = error "Cannot mergeNoPop empty tree"            
mergeNoPop i (Node nt nts) t1 k v t2 = case i of
  0 -> NoPop $ Node t1 $ (k, v, t2) NE.<| nts
  _ -> NoPop $ replaceIndexT i t1 $ Node nt $ insertTriple nts (k, v, t2)
                                        
insertTriple :: Ord k => NE.NonEmpty (k, v, Tree k v) -> (k, v, Tree k v) ->
                NE.NonEmpty (k, v, Tree k v)
insertTriple xs x = NE.fromList $
                    L.insertBy (compare `on` fst') x $ NE.toList xs

splitNode :: Ord k => Order -> Tree k v -> InsertResult k v
splitNode _ Empty = error "Cannot split empty list"
splitNode ord (Node nt nts) = 
  let (nts1, nts2) = NE.splitAt (ord `div` 2) nts
      ((k', v', nt'), nts2') = (head nts2, tail nts2) -- safe!
  in Pop (Node nt $ NE.fromList nts1) k' v' (Node nt' $ NE.fromList nts2')

------------------------ Delete ------------------------ 

------------------------ Helpers ------------------------

-- Find index i where search key < tree key, or otherwise index of last
findIndexT :: Ord k => k -> Tree k v -> Int
findIndexT _ Empty = error "Empty tree cannot be indexed"
findIndexT k (Node _ nts) = L.foldl f 0 $ NE.toList $ NE.map fst' nts
  where f i k' = if k < k' then i else i + 1

-- Return Tree at index i
indexT :: Ord k => Int -> Tree k v -> Tree k v
indexT _ Empty = error "Empty tree has no index"
indexT i (Node nt nts) = case i of
  0 -> nt
  _ -> trd' $ (NE.!!) nts (i-1)

-- Replace T at index i
replaceIndexT :: Ord k => Int -> Tree k v -> Tree k v -> Tree k v
replaceIndexT _ _ Empty = error "Empty tree cannot be indexed"
replaceIndexT i t (Node nt nts) = case i of
  0 -> Node t nts
  _ -> Node nt $ NE.fromList $ NE.tail $
       NE.map fst $ NE.scanl f (NE.head nts, 1) nts
    where f (_, j) (k', v', t') = let nextT = if j == i then t else t'
                                  in ((k', v', nextT), j+1)

maxT :: Ord k => Tree k v -> k
maxT Empty = error "Empty tree has no max"
maxT (Node _ nts) = maximum $ NE.map fst' nts
          
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
