module BPTree where

import qualified Data.List as L

type Keys k = [k]
type Values v = [v]
type Kids k v = [BPTree k v]

-- A B+ Tree structure. Int m controls the branching factor. Nodes are either:
--
-- Empty
--
-- Leaf with keys, values, and a parent pointer
-- Invariant m <= #keys = #vals <= 2*m-1
--
-- Internal node with keys, children, and a parent pointer
-- Invariant m-1 <= #keys = (#children)-1 <= 2*m-1

data BPTree k v = Nil Int 
                 | Leaf Int (Keys k) (Values v) (Maybe (BPTree k v)) 
                 | Node Int (Keys k) (Kids k v) (Maybe (BPTree k v))
                 deriving Show

-- Implementing basic algorithms following the outline of http://www.cburch.com/cs/340/reading/btree/index.html

-- LOOKUP FUNCTIONS

-- Given key, descend to the leaf node that could contain it
search :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
search t@(Nil _) _ = error "Searching empty tree" -- Don't do this
search l@(Leaf _ _ _ _) _ = l
search (Node _ ks ts _) x = search child x
  where idx = L.findIndex (< x) ks
        shiftIdx mi = case mi of Nothing -> 0
                                 Just i -> i+1
        child = ts !! (shiftIdx idx)

-- Given key, look up the associated value
locate :: (Ord k, Eq k) => BPTree k v -> k -> Maybe v
locate t x = (vals !!) <$> idx
  where (Leaf _ keys vals _) = search t x
        idx = L.elemIndex x keys

-- INSERTION FUNCTIONS

-- Inserts key-value pair, may upset the invariant.
insert :: (Ord k, Eq k) => BPTree k v -> k -> v -> BPTree k v
insert (Nil m) x y = Leaf m [x] [y] Nothing
insert (Node m ks ts par) x y = Node m ks (ls ++ [insert r x y] ++ rs) par
  where idx = L.findIndex (< x) ks 
        shiftIdx mi = case mi of Nothing -> 0
                                 Just i -> i+1
        splitpt = shiftIdx idx
        (ls,r:rs) = splitAt splitpt ts
insert (Leaf m keys vals par) x y = let (ks,vs) = intoAssoc x y (keys,vals) in Leaf m ks vs par

-- Does the work of splitting nodes/moving up keys to restore invariant. After (insert t x y), call this on (search t x)  
fixBranchingPlus :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
fixBranchingPlus t@(Nil _) _ = t
fixBranchingPlus l@(Leaf m ks vs par) x
  | (length ks) <= (2*m-1) = maybe l (`fixBranchingPlus` x) par
  | otherwise = fixBranchingPlus (place p x (split l)) x --split this node, (re)place altered kids & key in parent, call fixBranching there
  where p = maybe (Nil m) id par
fixBranchingPlus n@(Node m ks ts par) x
  | (length ts) <= (2*m-1) = maybe n (`fixBranchingPlus` x) par
  | otherwise = fixBranchingPlus (place p x (split n)) x --split this node, (re)place altered kids & key in parent, call fixBranching there
  where p = maybe (Nil m) id par

-- Split a node into two (only done when capacity exactly hits 2*m)
split :: (Ord k, Eq k) => BPTree k v -> (BPTree k v, BPTree k v)
split (Nil m) = error "Splitting empty tree"
split (Leaf m ks vs par) = ((Leaf m (take m ks) (take m vs) p), (Leaf m (drop m ks) (drop m vs) p))
  where p = Just (maybe (Nil m) id par)
split (Node m ks ts par) = ((Node m (take (m-1) ks) (take (m-1) ts) p), (Node m (drop m ks) (drop m ts) p))
  where p = Just (maybe (Nil m) id par)

-- Places a new key and tree pair into a node
place :: (Ord k, Eq k) => BPTree k v -> k -> (BPTree k v, BPTree k v) -> BPTree k v
place (Nil m) x (t0, t1) = Node m [x] [t0, t1] Nothing
place (Leaf _ _ _ _) _ _ = error "Placing into leaf"
place (Node m ks ts par) x (t0, t1) = Node m (L.insert x ks) (ls ++ [t0, t1] ++ rs) par
  where idx = L.findIndex (< x) ks 
        shiftIdx mi = case mi of Nothing -> 0
                                 Just i -> i+1
        splitpt = shiftIdx idx
        (ls,r:rs) = splitAt splitpt ts
        
-- Puts key + value into proper positions in a pair of lists (used for inserting into leaf node)
intoAssoc :: (Ord k, Eq k) => k -> v -> ([k],[v]) -> ([k],[v])
intoAssoc x y ([],vs) = ([x],y:vs) -- should only occur with vs = []
intoAssoc x y (keys@(k:ks),vals@(v:vs))
  | x < k = (x:keys,y:vals)
  | x == k = (x:ks,y:vs)
  | otherwise = tupleCons (k,v) (intoAssoc x y (ks,vs))
    where tupleCons (a,b) (as,bs) = (a:as, b:bs)

-- DELETION FUNCTIONS (INCOMPLETE)
{-
-- Deletes key (and associated value), may upset invariant 
delete :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
delete (Nil _) _ = error "Deleting from an empty tree"
delete (Node m ks ts par) x = Node m ks (ls ++ [delete r x] ++ rs) par
  where idx = findIndex (< x) ks 
        shiftIdx mi = case mi of Empty = 0
                                 Full i = i+1
        splitpt = shiftIdx idx
        (ls,r:rs) = splitAt splitpt ts
delete (Leaf m keys vals par) x = let (ks,vs) = outOfAssoc x (keys,vals) in Leaf m ks vs par

-- Does the work of shifting keys/merging nodes to restore invariant. After (delete t x), call this on (search t x)  
fixBranchingMinus :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
fixBranchingMinus t@(Nil _) _ = t
fixBranchingMinus l@(Leaf m ks vs par) x
  | (length ks) >= m = maybe l fixBranchingMinus par
  | (length nks) > m = undefined -- Extra keys to the right, shift one over
  | (length pks) > m = undefined -- Extra keys to the left, shift one over
  | otherwise = undefined -- Both sides at minimum, do a merge
  where (Leaf _ nks nvs npar) = nextNode l
        (Leaf _ pks pvs ppar) = prevNode l
fixBranchingPlus n@(Node ks ts par) x
  | (length ts) >= m = maybe n fixBranchingMinus par
  | (length nts) > m = undefined -- Extra keys to the right, shift one over
  | (length pts) > m = undefined -- Extra keys to the left, shift one over
  | otherwise = undefined -- Both sides at minimum, do a merge
  where (Node _ nks nts npar) = nextNode n
        (Leaf _ pks pts ppar) = prevNode n

-- Gets the next node in the same level (to be used for shifting/merging purposes)
--nextLeaf ::
nextNode = undefined

-- Gets the previous node in the same level (to be used for shifting/merging purposes)
--prevLeaf ::
prevNode = undefined

-- Moves about keys among nodes
--shift ::
shift = undefined

-- Merges two nodes into one
--merge ::
merge = undefined
                                     
-- Takes key + corresponding value out of associated pair of lists (used for deleting from leaf node)
outOfAssoc :: (Ord k, Eq k) => k -> ([k],[v]) -> ([k],[v])
outOfAssoc _ pair@([],_) = pair
outOfAssoc x pair@(keys@(k:ks),vals@(v:vs))
  | x < k = pair
  | x == k = (ks,vs)
  | otherwise = (<$>) $ (:) $ (k,v) (outOfAssoc x (ks,vs))
-}
  
-- TESTING FUNCTIONS

empty :: BPTree Int Int
empty = Nil 2

makeTree :: Int -> BPTree Int Int
makeTree n = fromList (zip [1..n] [1..n]) empty

fromList :: (Ord k, Eq k) => [(k,v)] -> BPTree k v -> BPTree k v  
fromList kvs t =
   foldr (\(x,y) acc -> (`fixBranchingPlus` x) $ (`search` x) $ insert acc x y) t kvs

t10 :: BPTree Int Int
t10 = makeTree 10

-- TESTS
main :: IO ()
main = print t10

--print ((`fixBranchingMinus` x) $ (`search` x) $ delete t10 2)
