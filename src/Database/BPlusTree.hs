-- This module compiles and one can run, for example,
-- the function 'makeTree' as illustrated below.
-- Still more thinking and testing to do on this
-- to see if it is doing what it is supposed to
-- be doing.
--
--
-- *BPlusTree> makeTree 2
-- Leaf 2 [1,2] [1,2]
--
-- *BPlusTree> makeTree 4
-- NNode 2 [3] [Leaf 2 [1,2] [1,2],Leaf 2 [3,4] [3,4]]


module Database.BPlusTree where

data BPTree k v = Nil Int | Leaf Int [k] [v] | Node Int [k] [BPTree k v] deriving Show


empty :: BPTree Int Int
empty = Nil 2

makeTree :: Int -> BPTree Int Int
makeTree n = fromList [1..n] [1..n] empty

-- CONSTRUCT TREE FROM LIST
-- > fromList [1..99] (Nil 2) |> depth
--   5
fromList :: (Ord k) => [k] -> [v] -> BPTree k v -> BPTree k v  
fromList as bs t =
  let 
      ps = zip as bs
  in
   foldr (\p acc -> insert acc (fst p) (snd p)) t ps


insert :: (Ord k, Eq k) => BPTree k v -> k -> v -> BPTree k v
insert t x y = if is_full t then insert_non_full (split t) x y
                          else insert_non_full t x y


-- HELPERS


insert_non_full :: (Ord k, Eq k) => BPTree k v -> k -> v -> BPTree k v
insert_non_full (Nil m) x y = Leaf m [x] [y]
insert_non_full (Leaf m [] [] ) x y = Leaf m [x] [y]
insert_non_full l@(Leaf m keys@(k:ks)  values@(v:vs)) x y
  | x == k = l
  | x < k  = Leaf m (x:keys) (y:values)
  | x > k  = Leaf m (k:new_ks) (v:new_vs)
    where Leaf _ new_ks new_vs = insert_non_full (Leaf m ks vs) x y
insert_non_full (Node m [] (t:ts)) x y = if is_full t then insert_non_full (split t) x y
                                                    else Node m [] [(insert_non_full t x y)]
insert_non_full n@(Node m keys@(k:ks)  trees@(t:ts)) x y
  | x == k = n
  | x < k  = if is_full t then insert_non_full (Node m (newK:k:ks) (newT1:newT2:ts)) x y
                          else Node m keys ((insert_non_full t x y):ts)
  | x > k  = Node m (k:new_ks) (t:new_ts)
    where Node _ new_ks new_ts = insert_non_full (Node m ks ts) x y
          Node _ [newK] [newT1, newT2] = split t

split :: (Ord k, Eq k) => BPTree k v -> BPTree k v
split (Leaf m keys values) = Node m [k] [Leaf m k1 v1, Leaf m (k:k2) (v:v2)]
  where k1 = first_half keys
        k:k2 = last_half keys
        v1 = first_half values
        v:v2 = last_half values
split (Node m keys trees) = Node m [k] [Node m k1 t1, Node m (k:k2) t2]
  where k1 = first_half keys
        k:k2 = last_half keys
        t1 = first_half trees
        t2 = last_half trees

first_half :: [a] -> [a]
first_half xs = take (div (length xs) 2) xs

last_half :: [a] -> [a]
last_half xs = drop (div (length xs) 2) xs

is_full :: (Ord k, Eq k) => BPTree k v -> Bool
is_full (Nil m) = False
is_full (Leaf m ks vs)
  | length ks == (2 * m - 1) = True
  | otherwise = False
is_full (Node m ks _)
  | length ks == (2 * m - 1) = True
  | otherwise = False
