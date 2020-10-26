module Database.BPlusTree where

-- TO DO:
-- Implement deletion methods
-- Change storage of Leaf nodes into a hashmap also for ease of access
-- Massively clean up and refactor - this is all a mess currently

import qualified Data.List as L
import qualified Data.Map as M

-- A B+ Tree structure. 

-- Every node has two ints: 
-- m controls the branching factor
-- h is the height (root is at height 0) 

-- Nodes are either:
--
-- Empty
--
-- Leaf with keys, values, and a parent pointer
-- Invariant m <= #keys = #vals <= 2*m-1
--
-- Internal node with keys, children, and a parent pointer
-- Invariant m-1 <= #keys = (#children)-1 <= 2*m-1
--
-- Also have a nested hashmap containing these nodes
-- Outer keys = height (root is 0)
-- Inner keys = key intervals

data BPTree k v = BPTree {root :: Node k v, heightmap :: HeightMap k v}
                  deriving Show

data Node k v =
  Nil BranchFactor
  | Leaf Height BranchFactor (Keys k) (Values v) (Maybe (TreePtr k))
  | Internal Height BranchFactor (Keys k) [TreePtr k] (Maybe (TreePtr k))
  deriving Show

type HeightMap k v = (M.Map Height (M.Map (KeyInterval k) (Node k v)))

type Height = Int
type BranchFactor = Int

type Keys k = [k]
type Values v = [v]
type KeyInterval k = (k, k)

type TreePtr k = (Height, KeyInterval k) -- indices for nested hash: height, key interval.


-- Implementing basic algorithms following the outline of http://www.cburch.com/cs/340/reading/btree/index.html

-- GENERAL HELPERS/ACCESSORS

-- Restores root component to point to root after having moved through the tree elsewhere
-- Will cause error on empty tree
recoverRoot :: (Ord k, Eq k) => BPTree k v -> Node k v
recoverRoot bt = (M.elems (hm M.! 0)) !! 0
  where hm = heightmap bt

-- Used for going from key index to ptr index in an Internal node  
shiftIdx :: Maybe Int -> Int
shiftIdx =  maybe 0 succ

-- Extracts height parameter
getHeight :: Node k v -> Height
getHeight (Nil _) = 0
getHeight (Leaf h _ _ _ _) = h
getHeight (Internal h _ _ _ _) = h

-- Extracts list of keys
getKeys :: (Ord k, Eq k) => Node k v -> Keys k
getKeys (Nil _) = error "No keys"
getKeys (Leaf _ _ ks _ _) = ks
getKeys (Internal _ _ ks _ _) = ks

-- Extracts kids (only from internal)
getKids :: (Ord k, Eq k) => Node k v -> [TreePtr k]
getKids (Nil _) = error "No kids"
getKids (Leaf _ _ _ _ _) = error "No kids"
getKids (Internal _ _ _ ts _) = ts

-- Extracts interval of keys represented in this node 
getKeyIntvl :: (Ord k, Eq k) => Node k v -> (k, k)
getKeyIntvl (Nil _) = error "No keys"
getKeyIntvl (Leaf _ _ ks _ _) = (minimum ks, maximum ks)
getKeyIntvl (Internal _ _ ks _ _) = (minimum ks, maximum ks)

-- Extracts parent pointer
getParent :: Node k v -> Maybe (TreePtr k)
getParent (Nil _) = Nothing
getParent (Leaf _ _ _ _ par) = par
getParent (Internal _ _ _ _ par) = par

-- Node from HeightMap and TreePtr
getNodeMap :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v
getNodeMap hm tptr = (hm M.! (fst tptr)) M.! (snd tptr)

-- Node from BPTree and TreePtr
getNode :: (Ord k, Eq k) => BPTree k v -> TreePtr k -> Node k v
getNode = getNodeMap . heightmap

-- shifts node height parameter (and heights in ptrs if appropriate)
nodeHeightShift :: Int -> Node k v -> Node k v
nodeHeightShift shift node = case node of
                               n@(Nil m) -> n -- never used
                               (Internal h m ks ts par) -> Internal (h+shift) m ks (map shiftPtr ts) (fmap shiftPtr par)
                               (Leaf h m ks vs par) -> Leaf (h+shift) m ks vs (fmap shiftPtr par)
                             where shiftPtr (i,ki) = (i+shift, ki)
                             
-- Increments all node heights and height keys in hash map, used when height of tree changes due to root spliting or merging
mapHeightShift:: Int -> HeightMap k v -> HeightMap k v
mapHeightShift shift = (M.map (M.map (nodeHeightShift shift))) . (M.mapKeysMonotonic (+shift))

-- Peel off a Just wrapper
fromJust :: Maybe a -> a
fromJust Nothing = error "Nothing"
fromJust (Just a) = a

-- LOOKUP FUNCTIONS

-- Given key, descend to the leaf node that could contain it
search :: (Ord k, Eq k) => BPTree k v -> k -> Node k v
search bt x = case node of
                (Nil _) -> error "Searching empty tree" -- Don't do this
                l@(Leaf _ _ _ _ _) -> l
                n@(Internal _ _ ks ts _) -> let hm = heightmap bt
                                                idx = L.findIndex (<= x) ks
                                                cptr = ts !! (shiftIdx idx)
                                                child = getNodeMap hm cptr
                                            in search (BPTree child hm) x
           where node = root bt

-- Given key, look up the associated value
locate :: (Ord k, Eq k) => BPTree k v -> k -> Maybe v
locate t x = (vals !!) <$> idx
  where (Leaf _ _ keys vals _) = search t x
        idx = L.elemIndex x keys

-- INSERTION FUNCTIONS

-- Inserts key-value pair into tree and rebalances to maintain invariant
insert :: (Ord k, Eq k) => BPTree k v -> k -> v -> BPTree k v
insert bt x y = fixBranchingPlus $ insertImproper bt x y

-- Inserts key-value pair into correct leaf
-- erases (reference to) child pointers as it descends (to be fixed on way up)
-- may upset the invariant (fixed on way up) 
-- in the output, the 'root' of of the tree remains at insertion leaf
insertImproper :: (Ord k, Eq k) => BPTree k v -> k -> v -> BPTree k v
insertImproper bt x y = case node of
                  -- Empty node: create singleton root leaf node
                  (Nil m) -> let l = Leaf 0 m [x] [y] Nothing
                             in BPTree l (M.singleton 0 (M.singleton (x,x) l))
                  -- Internal node: delete child reference and go down
                  (Internal h m ks ts par) -> let idx = L.findIndex (<= x) ks 
                                                  splitpt = shiftIdx idx
                                                  (ls,r:rs) = splitAt splitpt ts
                                                  ts' = ls ++ rs
                                                  n = Internal h m ks ts' par
                                                  f  = M.adjust (const n) ki
                                                  hm' = M.adjust f h hm 
                                              in insertImproper (BPTree (getNode bt r) hm') x y
                  -- Leaf node: insert into lists, update hm with new version of this leaf
                  (Leaf h m ks vs par) -> let lk' = min lk x
                                              rk' = max rk x
                                              (ks',vs') = intoAssoc x y (ks,vs)
                                              l = Leaf h m ks' vs' par
                                              f hmInner = M.insert (lk',rk') l (M.delete ki hmInner)
                                              hm' = M.adjust f h hm
                                          in BPTree l hm'
                where node = root bt
                      hm = heightmap bt
                      ki@(lk, rk) = getKeyIntvl node

-- Does the work of splitting nodes/moving up keys to restore invariant
-- Also fixes pointers to children as it ascends, and to parents whenever there's an internal split
fixBranchingPlus :: (Ord k, Eq k) => BPTree k v -> BPTree k v
fixBranchingPlus bt = case node of
                  l@(Leaf h m ks vs par)
                    -- splitting root: empty parent, so need to make new root and adjust all heights in final answer HeightMap
                    | h == 0 && (length ks) == 2*m -> let newroot = (Internal 0 m [x] [(1,ki0), (1,ki1)] Nothing)
                                                          roothm = M.insert 0 (M.singleton (x,x) newroot) (mapHeightShift 1 hm')
                                                      in BPTree newroot roothm
                    -- unfull root: do nothing
                    | h == 0 -> bt
                    -- unfull proper leaf: fix parent from here then recurse up
                    | (length ks) <= (2*m-1) -> let truepar = fromJust par
                                                    pnode = getNodeMap hm truepar
                                                    (phm,pnode') = placePtr hm (h,ki) pnode
                                                in fixBranchingPlus (BPTree pnode' phm)
                    -- full: split this node, update hm to reflect split, (re)place altered kids & key in parent, call fixBranching there
                    | otherwise -> let truepar = fromJust par
                                       pnode = getNodeMap hm' truepar
                                       (phm', pnode') = placeKey hm' x pnode
                                       ki' = getKeyIntvl pnode'
                                       ts' = getKids pnode'
                                       phmfold = foldr (\ptr hash -> fst (placeParent hash (h-1,ki') (getNodeMap hash ptr))) phm' ts'
                                       (phm'', pnode'') = placePtr phmfold (h, ki0) pnode'
                                       (phm''', pnode''') = placePtr phm'' (h, ki1) pnode''
                                   in fixBranchingPlus (BPTree pnode''' phm''')
                    where x = ks !! m 
                          (l0,l1) = split l
                          ki0 = getKeyIntvl l0
                          ki1 = getKeyIntvl l1
                          hm' = (M.adjust ((M.insert ki1 l1) . (M.insert ki0 l0) . (M.delete ki)) h hm)
                          
                  -- the internal cases are all analogous to the leaf cases        
                  n@(Internal h m ks ts par)
                    | h == 0 && (length ts) == 2*m -> let newroot = (Internal 0 m [x] [(1,ki0), (1,ki1)] Nothing)
                                                          roothm = M.insert 0 (M.singleton (x,x) newroot) (mapHeightShift 1 hmfold') 
                                                      in BPTree newroot roothm
                    | h == 0 -> bt
                    | (length ts) <= (2*m-1) -> let truepar = fromJust par
                                                    pnode = getNodeMap hm truepar
                                                    (phm,pnode') = placePtr hm (h,ki) pnode
                                                in fixBranchingPlus (BPTree pnode' phm)
                    | otherwise -> let truepar = fromJust par
                                       pnode = getNodeMap hmfold' truepar
                                       (phm', pnode') = placeKey hmfold' x pnode
                                       ki' = getKeyIntvl pnode'
                                       ts' = getKids pnode'
                                       phmfold = foldr (\ptr hash -> fst (placeParent hash (h-1,ki') (getNodeMap hash ptr))) phm' ts'
                                       (phm'', pnode'') = placePtr phmfold (h, ki0) pnode'
                                       (phm''', pnode''') = placePtr phm'' (h, ki1) pnode''
                                   in fixBranchingPlus (BPTree pnode''' phm''')
                    where x = ks !! (m-1)
                          (n0,n1) = split n
                          ki0 = getKeyIntvl n0
                          ki1 = getKeyIntvl n1
                          ts0 = take m ts
                          ts1 = drop m ts
                          hm' = (M.adjust ((M.insert ki1 n1) . (M.insert ki0 n0) . (M.delete ki)) h hm)
                          hmfold = foldr (\ptr hash -> fst (placeParent hash (h,ki0) (getNodeMap hash ptr))) hm' ts0
                          hmfold' = foldr (\ptr hash -> fst (placeParent hash (h,ki1) (getNodeMap hash ptr))) hmfold ts1
                where node = root bt
                      hm = heightmap bt
                      ki = getKeyIntvl node
                      h = getHeight node
                      par = getParent node
          
-- Split a node into two (only done when capacity exactly hits 2*m), also if root is split put in ptr to new root
split :: (Ord k, Eq k) => Node k v -> (Node k v, Node k v)
split (Nil m) = error "Splitting empty tree"
split (Leaf h m ks vs par) = ((Leaf h m (take m ks) (take m vs) p), (Leaf h m (drop m ks) (drop m vs) p))
  where x = ks !! m
        kiAlter (h,ki) = (h,keyIntvlAlter x ki) 
        p = Just (maybe (-1,(x,x)) kiAlter par) -- height -1 for future shift purposes
split (Internal h m ks ts par) = ((Internal h m (take (m-1) ks) (take m ts) p), (Internal h m (drop m ks) (drop m ts) p))
  where x = ks !! (m-1)
        kiAlter (h,ki) = (h,keyIntvlAlter x ki)
        p = Just (maybe (-1,(x,x)) kiAlter par)

-- Changes key interval signature when a new key is added     
keyIntvlAlter :: (Ord k, Eq k) => k -> (k,k) -> (k,k)
keyIntvlAlter a (b,c) = (min a b, max a c)

-- Places a new key into a node and update HeightMap accordingly
placeKey :: (Ord k, Eq k) => HeightMap k v -> k -> Node k v -> (HeightMap k v, Node k v)
placeKey hm x (Nil m) = let n = Internal 0 m [x] [] Nothing 
                        in (M.insert 0 (M.singleton (x,x) n) hm, n)
placeKey _ _ (Leaf _ _ _ _ _) = error "Placing into leaf" -- this should never come up the way insertion works right now but it can't hurt to write it
placeKey hm x n@(Internal h m ks ts par) = let n' = Internal h m (L.insert x ks) ts par
                                               ki = getKeyIntvl n
                                               ki' = getKeyIntvl n'
                                               hm' = M.adjust ((M.insert ki' n') . (M.delete ki)) h hm
                                           in (hm',n')
                                        
-- Parent-to-child pointer updating helper function - intended to add back pointer to child updated in insertion process
-- This change is crystallized by storing it in the HeightMap
placePtr :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v -> (HeightMap k v, Node k v)
placePtr hm ptr node = case node of
                (Nil _) -> error "Parent is empty"
                (Leaf _ _ _ _ _) -> error "Parent is leaf" -- shouldn't happen, but could be implemented
                (Internal h m ks ts par) -> let x = fst $ snd $ ptr
                                                idx = L.findIndex (<= x) ks 
                                                splitpt = shiftIdx idx
                                                (ls,rs) = splitAt splitpt ts
                                                ts' = ls ++ [ptr] ++ rs
                                                n' = Internal h m ks ts' par
                                                ki = getKeyIntvl node
                                                hm' = M.adjust (M.insert ki n') h hm -- overwrites old copy of node
                                            in (hm',n')
-- Changes parent pointers (for fixing references after splits or merges)                                            
placeParent :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v -> (HeightMap k v, Node k v)
placeParent _ _ (Nil m) = error "Parent of empty node"
placeParent hm ptr l@(Leaf h m ks vs par) = let l' = Leaf h m ks vs (Just ptr)
                                                ki = getKeyIntvl l
                                                hm' = M.adjust (M.insert ki l') h hm
                                            in (hm', l')
placeParent hm ptr n@(Internal h m ks ts par) = let n' = Internal h m ks ts (Just ptr)
                                                    ki = getKeyIntvl n
                                                    hm' = M.adjust (M.insert ki n') h hm
                                                in (hm',n')
        
-- Puts key + value into proper positions in a pair of lists (used for inserting into leaf node)
-- The need for this function would be obviated by using hashmaps in the leaves (see ToDo)
intoAssoc :: (Ord k, Eq k) => k -> v -> (Keys k, Values v) -> (Keys k, Values v)
intoAssoc x y ([],vs) = ([x],y:vs) -- should only occur with vs = []
intoAssoc x y (keys@(k:ks),vals@(v:vs))
  | x < k = (x:keys,y:vals)
  | x == k = (x:ks,y:vs)
  | otherwise = tupleCons (k,v) (intoAssoc x y (ks,vs))
    where tupleCons (a,b) (as,bs) = (a:as, b:bs)

-- DELETION FUNCTIONS (Incomplete, a random mess of variations from older forms)
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
  
-- Helpers

empty :: BPTree Int Int
empty = BPTree (Nil 2) M.empty

emptyS :: BPTree Int String
emptyS = BPTree (Nil 2) M.empty


makeTree :: Int -> BPTree Int Int
makeTree n = fromList (zip [1..n] [1..]) empty

makeTree' :: Int -> BPTree Int Int
makeTree' n = fromList (zip [n,(n-1)..1] [n,(n-1)..1]) empty


fromList :: (Ord k, Eq k) => [(k,v)] -> BPTree k v -> BPTree k v  
fromList kvs t =
   foldr (\(x,y) acc -> insert acc x y) t kvs

-- TESTS
-- main :: IO ()
-- main = print t

--print ((`fixBranchingMinus` x) $ (`search` x) $ delete t10 2)
