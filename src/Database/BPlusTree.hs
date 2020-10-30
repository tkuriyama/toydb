module Database.BPlusTree where

-- TO DO:
-- Change storage of Leaf nodes into a hashmap also for ease of access
-- Massively clean up and refactor - this is all a mess currently
-- Add bulk loading method for better initialization of large trees
-- Replace the lists for keys and values with sequences? Would allow for easier manipulation, but not a big time savings for small m

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

-- A B+ Tree structure. Consists of a node (the "current location" in the tree), heightmap storing the whole tree, and branching factor m
-- HeightMap is a nested hashmap where outer keys = height (root is 0), inner keys = key intervals, values = nodes

-- Every node has int h, the height (root is at height 0)
-- Nodes are either:
--
-- Empty
--
-- Leaf with keys, values, and a parent pointer
-- Invariant m <= #keys = #vals <= 2*m-1
--
-- Internal node with keys, children, and a parent pointer
-- Invariant m-1 <= #keys = (#children)-1 <= 2*m-1

-- A note: after having debugged the deletion algos, I think in retrospect that there are still more bugs that would come up if we allow m=1
-- But this should never arise in practice hopefully since m=1 is a very bad storage structure

data BPTree k v = BPTree {root :: Node k v
                         , heightmap :: HeightMap k v
                         , branchfactor :: BranchFactor}
                  deriving (Show, Eq)

data Node k v =
  Nil
  | Leaf Height (Keys k) (Values v) (Maybe (TreePtr k))
  | Internal Height (Keys k) [TreePtr k] (Maybe (TreePtr k))
  deriving (Show, Eq)

type HeightMap k v = (M.Map Height (M.Map (KeyInterval k) (Node k v)))

type Height = Int
type BranchFactor = Int

type Direction = Bool -- for redistributing/merging after deletions. False indicates left, True indicates right

leftDir :: Direction
leftDir = False

rightDir :: Direction
rightDir = True

type Keys k = [k]
type Values v = [v]
type KeyInterval k = (k, k)

type TreePtr k = (Height, KeyInterval k) -- indices for nested hash: height, key interval.


-- Implementing basic algorithms following the outline of http://www.cburch.com/cs/340/reading/btree/index.html

-- GENERAL HELPERS/ACCESSORS

-- Restores root component to point to root after having moved through the tree elsewhere
-- Will cause error on empty tree
recoverRootFromMap :: (Ord k, Eq k) => HeightMap k v -> Node k v
recoverRootFromMap hm = (M.elems (hm M.! 0)) !! 0

-- Used for going from key index to ptr index in an Internal node  
shiftIdx :: Maybe Int -> Int
shiftIdx =  maybe 0 succ

-- Extracts height parameter
getHeight :: Node k v -> Height
getHeight Nil = 0
getHeight (Leaf h _ _ _) = h
getHeight (Internal h _ _ _) = h

-- Extracts list of keys
getKeys :: (Ord k, Eq k) => Node k v -> Keys k
getKeys Nil = []
getKeys (Leaf _ ks _ _) = ks
getKeys (Internal _ ks _ _) = ks

-- Extracts interval of keys represented in this node 
getKeyIntvl :: (Ord k, Eq k) => Node k v -> KeyInterval k
getKeyIntvl Nil = error "No keys"
getKeyIntvl (Leaf _ ks _ _) = (minimum ks, maximum ks)
getKeyIntvl (Internal _ [] ts _) = (fst $ snd $ head ts, snd $ snd $ last ts) -- Special case (only can occur when m = 2): if ks if empty, descend to kids to build ki
getKeyIntvl (Internal _ ks _ _) = (minimum ks, maximum ks)

-- Extracts kids (only from internal). This should really return a Maybe
getKids :: (Ord k, Eq k) => Node k v -> [TreePtr k]
getKids Nil = error "No kids"
getKids (Leaf _ _ _ _) = error "No kids"
getKids (Internal _ _ ts _) = ts

-- Extracts values (only from leaf). This should really return a Maybe
getVals :: (Ord k, Eq k) => Node k v -> Values v
getVals Nil = error "No values"
getVals (Leaf _ _ vs _) = vs
getVals (Internal _ _ _ _) = error "No values"

-- Extracts parent pointer
getParent :: Node k v -> Maybe (TreePtr k)
getParent Nil = Nothing
getParent (Leaf _ _ _ par) = par
getParent (Internal _ _ _ par) = par

-- Node from HeightMap and TreePtr
getNodeMap :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v
getNodeMap hm tptr = (hm M.! (fst tptr)) M.! (snd tptr)

-- Node from BPTree and TreePtr
getNode :: (Ord k, Eq k) => BPTree k v -> TreePtr k -> Node k v
getNode = getNodeMap . heightmap

-- shifts node height parameter (and heights in ptrs if appropriate)
nodeHeightShift :: Height -> Node k v -> Node k v
nodeHeightShift shift node = case node of
                               Nil -> node -- never used
                               (Internal h ks ts par) -> Internal (h+shift) ks (map shiftPtr ts) (fmap shiftPtr par)
                               (Leaf h ks vs par) -> Leaf (h+shift) ks vs (fmap shiftPtr par)
                             where shiftPtr (i,ki) = (i+shift, ki)
                             
-- Increments all node heights and height keys in hash map, used when height of tree changes due to root spliting or merging
mapHeightShift:: Height -> HeightMap k v -> HeightMap k v
mapHeightShift shift = (M.map (M.map (nodeHeightShift shift))) . (M.mapKeysMonotonic (+shift))

-- Peel off a Just wrapper
fromJust :: Maybe a -> a
fromJust Nothing = error "Nothing"
fromJust (Just a) = a

-- LOOKUP FUNCTIONS

findIndex :: (Ord k, Eq k) => [k] -> k -> Int
findIndex ks k = case L.findIndex (> k) ks of
                   (Just i) -> i
                   Nothing -> length ks

-- Given key, descend to the leaf node that could contain it
search :: (Ord k, Eq k) => BPTree k v -> k -> Maybe (Node k v)
search bt x =
  case root bt of
    Nil -> Nothing
    n@(Leaf _ ks _ _) -> if x `elem` ks then (Just n) else Nothing
    n@(Internal _ ks ts _) -> let hm = heightmap bt
                                  m = branchfactor bt
                                  idx = findIndex ks x
                                  cptr = ts !! idx
                                  child = getNodeMap hm cptr
                              in search (BPTree child hm m) x

-- Given key, look up the associated value
locate :: (Ord k, Eq k) => BPTree k v -> k -> Maybe v
locate t k =
  case search t k of
    (Just (Leaf _ keys vals _)) -> (vals !!) <$> (L.elemIndex k keys)
    _ -> Nothing

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
                  Nil -> let l = Leaf 0 [x] [y] Nothing
                         in BPTree l (M.singleton 0 (M.singleton (x,x) l)) m
                  -- Internal node: delete child reference and go down
                  (Internal h ks ts par) -> let splitpt = findIndex ks x
                                                (ls,r:rs) = splitAt splitpt ts
                                                ts' = ls ++ rs
                                                n = Internal h ks ts' par
                                                f  = M.adjust (const n) ki
                                                hm' = M.adjust f h hm 
                                            in insertImproper (BPTree (getNode bt r) hm' m) x y
                  -- Leaf node: insert into lists, update hm with new version of this leaf
                  (Leaf h ks vs par) -> let lk' = min lk x
                                            rk' = max rk x
                                            (ks',vs') = intoAssoc x y (ks,vs)
                                            l = Leaf h ks' vs' par
                                            f hmInner = M.insert (lk',rk') l (M.delete ki hmInner)
                                            hm' = M.adjust f h hm
                                          in BPTree l hm' m
                where node = root bt
                      hm = heightmap bt
                      m = branchfactor bt
                      ki@(lk, rk) = getKeyIntvl node

-- Does the work of splitting nodes/moving up keys to restore invariant
-- Also fixes pointers to children as it ascends, and to parents whenever there's an internal split
fixBranchingPlus :: (Ord k, Eq k) => BPTree k v -> BPTree k v
fixBranchingPlus bt = case node of
                  l@(Leaf h ks vs par)
                    -- splitting root: empty parent, so need to make new root and adjust all heights in final answer HeightMap
                    | h == 0 && (length ks) == 2*m -> let newroot = (Internal 0 [x] [(1,ki0), (1,ki1)] Nothing)
                                                          roothm = M.insert 0 (M.singleton (x,x) newroot) (mapHeightShift 1 hm')
                                                      in BPTree newroot roothm m
                    -- unfull root: do nothing
                    | h == 0 -> bt
                    -- unfull proper leaf: fix parent from here then recurse up
                    | (length ks) <= (2*m-1) -> let truePar = fromJust par
                                                    pnode = getNodeMap hm truePar
                                                    (phm,pnode') = placePtr hm (h,ki) pnode
                                                in fixBranchingPlus (BPTree pnode' phm m)
                    -- full: split this node, update hm to reflect split, (re)place altered kids & key in parent, call fixBranching there
                    | otherwise -> let truePar = fromJust par
                                       pnode = getNodeMap hm' truePar
                                       (phm', pnode') = placeKey hm' x pnode
                                       ki' = getKeyIntvl pnode'
                                       ts' = getKids pnode'
                                       phmfold = foldr (\ptr hash -> fst (placeParent hash (h-1,ki') (getNodeMap hash ptr))) phm' ts'
                                       (phm'', pnode'') = placePtr phmfold (h, ki0) pnode'
                                       (phm''', pnode''') = placePtr phm'' (h, ki1) pnode''
                                   in fixBranchingPlus (BPTree pnode''' phm''' m)
                    where x = ks !! m 
                          (l0,l1) = split l m
                          ki0 = getKeyIntvl l0
                          ki1 = getKeyIntvl l1
                          hm' = (M.adjust ((M.insert ki1 l1) . (M.insert ki0 l0) . (M.delete ki)) h hm)
                          
                  -- the internal cases are all analogous to the leaf cases        
                  n@(Internal h ks ts par)
                    | h == 0 && (length ts) == 2*m -> let newroot = (Internal 0 [x] [(1,ki0), (1,ki1)] Nothing)
                                                          roothm = M.insert 0 (M.singleton (x,x) newroot) (mapHeightShift 1 hmfold') 
                                                      in BPTree newroot roothm m
                    | h == 0 -> bt
                    | (length ts) <= (2*m-1) -> let truePar = fromJust par
                                                    pnode = getNodeMap hm truePar
                                                    (phm,pnode') = placePtr hm (h,ki) pnode
                                                in fixBranchingPlus (BPTree pnode' phm m)
                    | otherwise -> let truePar = fromJust par
                                       pnode = getNodeMap hmfold' truePar
                                       (phm', pnode') = placeKey hmfold' x pnode
                                       ki' = getKeyIntvl pnode'
                                       ts' = getKids pnode'
                                       phmfold = foldr (\ptr hash -> fst (placeParent hash (h-1,ki') (getNodeMap hash ptr))) phm' ts'
                                       (phm'', pnode'') = placePtr phmfold (h, ki0) pnode'
                                       (phm''', pnode''') = placePtr phm'' (h, ki1) pnode''
                                   in fixBranchingPlus (BPTree pnode''' phm''' m)
                    where x = ks !! (m-1)
                          (n0,n1) = split n m
                          ki0 = getKeyIntvl n0
                          ki1 = getKeyIntvl n1
                          ts0 = take m ts
                          ts1 = drop m ts
                          hm' = (M.adjust ((M.insert ki1 n1) . (M.insert ki0 n0) . (M.delete ki)) h hm)
                          hmfold = foldr (\ptr hash -> fst (placeParent hash (h,ki0) (getNodeMap hash ptr))) hm' ts0
                          hmfold' = foldr (\ptr hash -> fst (placeParent hash (h,ki1) (getNodeMap hash ptr))) hmfold ts1
                where node = root bt
                      hm = heightmap bt
                      m = branchfactor bt
                      ki = getKeyIntvl node
                      h = getHeight node
                      par = getParent node
          
-- Split a node into two (only done when capacity exactly hits 2*m), also if root is split put in ptr to new root
split :: (Ord k, Eq k) => Node k v -> BranchFactor -> (Node k v, Node k v)
split Nil _ = error "Splitting empty tree"
split (Leaf h ks vs par) m = ((Leaf h (take m ks) (take m vs) p), (Leaf h (drop m ks) (drop m vs) p))
  where x = ks !! m
        kiAlter (h,ki) = (h,keyIntvlAlter x ki) 
        p = Just (maybe (-1,(x,x)) kiAlter par) -- height -1 for future shift purposes
split (Internal h ks ts par) m = ((Internal h (take (m-1) ks) (take m ts) p), (Internal h (drop m ks) (drop m ts) p))
  where x = ks !! (m-1)
        kiAlter (h,ki) = (h,keyIntvlAlter x ki)
        p = Just (maybe (-1,(x,x)) kiAlter par)

-- Changes key interval signature when a new key is added     
keyIntvlAlter :: (Ord k, Eq k) => k -> KeyInterval k -> KeyInterval k
keyIntvlAlter a (b,c) = (min a b, max a c)

-- Places a new key into a node and updates HeightMap accordingly
placeKey :: (Ord k, Eq k) => HeightMap k v -> k -> Node k v -> (HeightMap k v, Node k v)
placeKey hm x Nil = let n = Internal 0 [x] [] Nothing 
                        in (M.insert 0 (M.singleton (x,x) n) hm, n)
placeKey _ _ (Leaf _ _ _ _) = error "Placing into leaf" -- this should never come up the way insertion works right now but it can't hurt to write it
placeKey hm x n@(Internal h ks ts par) = let n' = Internal h (L.insert x ks) ts par
                                             ki = getKeyIntvl n
                                             ki' = getKeyIntvl n'
                                             hm' = M.adjust ((M.insert ki' n') . (M.delete ki)) h hm
                                         in (hm',n')
                                        
-- Parent-to-child pointer updating helper function - intended to add back pointer to child updated in insertion/deletion process
-- This change is crystallized by storing it in the HeightMap
placePtr :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v -> (HeightMap k v, Node k v)
placePtr hm ptr node = case node of
                Nil -> error "Parent is empty"
                (Leaf _ _ _ _) -> error "Parent is leaf"
                (Internal h ks ts par) -> let x = fst $ snd $ ptr
                                              splitpt = findIndex ks x
                                              (ls,rs) = splitAt splitpt ts
                                              ts' = ls ++ [ptr] ++ rs
                                              n' = Internal h ks ts' par
                                              ki = getKeyIntvl node
                                              hm' = M.adjust (M.insert ki n') h hm -- overwrites old copy of node
                                          in (hm',n')
                                          
-- Changes parent pointers (for fixing references after splits or merges)                                            
placeParent :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v -> (HeightMap k v, Node k v)
placeParent _ _ Nil = error "Parent of empty node"
placeParent hm ptr l@(Leaf h ks vs par) = let l' = Leaf h ks vs (Just ptr)
                                              ki = getKeyIntvl l
                                              hm' = M.adjust (M.insert ki l') h hm
                                          in (hm', l')
placeParent hm ptr n@(Internal h ks ts par) = let n' = Internal h ks ts (Just ptr)
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

-- DELETION FUNCTIONS (UNDER CONSTRUCTION)
-- Here the previous site doesn't provide sufficient explanation, see here for further detail https://www.programiz.com/dsa/deletion-from-a-b-plus-tree
-- These slides are also helpful http://avid.cs.umass.edu/courses/445/f2008/17-tree-indexes-2.pdf

-- Deletes key from tree and rebalances to maintain invariant
delete :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
delete bt x = fixBranchingMinus $ deleteImproper bt x

-- Deletes key-value pair from (leaf of) tree
-- erases (reference to) child pointers as it descends (to be fixed on way up)
-- may upset the invariant (fixed on way up) 
-- in the output, the 'root' of of the tree remains at deletion leaf
deleteImproper :: (Ord k, Eq k) => BPTree k v -> k -> BPTree k v
deleteImproper bt x = case node of
                  -- Empty node: do nothing
                  Nil -> bt
                  -- Internal node: delete child reference and go down
                  (Internal h ks ts par) -> let splitpt = findIndex ks x
                                                (ls,r:rs) = splitAt splitpt ts
                                                ts' = ls ++ rs
                                                n = Internal h ks ts' par
                                                f  = M.adjust (const n) ki
                                                hm' = M.adjust f h hm 
                                              in deleteImproper (BPTree (getNode bt r) hm' m) x
                  -- Leaf node: delete key and corresponding value from lists, update hm with new version of this leaf
                  (Leaf h ks vs par) -> let (ks',vs') = outOfAssoc x (ks,vs)
                                            l = Leaf h ks' vs' par
                                            ki' = getKeyIntvl l
                                            f hmInner = M.insert ki' l (M.delete ki hmInner)
                                            hm' = M.adjust f h hm
                                        in BPTree l hm' m
                where node = root bt
                      hm = heightmap bt
                      m = branchfactor bt
                      ki@(lk, rk) = getKeyIntvl node
                      
-- Removes key + value from associated pair of lists (used for deleting into leaf node)
-- The need for this function would be obviated by using hashmaps in the leaves (see ToDo)
outOfAssoc :: (Ord k, Eq k) => k -> (Keys k, Values v) -> (Keys k, Values v)
outOfAssoc _ pair@([],_) = pair
outOfAssoc x (keys@(k:ks),vals@(v:vs))
  | x < k = (keys, vals)
  | x == k = (ks,vs)
  | otherwise = tupleCons (k,v) (outOfAssoc x (ks,vs))
    where tupleCons (a,b) (as,bs) = (a:as, b:bs)
    
-- Does the work of moving around keys/merging nodes/correcting keys in parents to restore invariants
-- Also fixes pointers to children as it ascends, and to parents whenever there's a merge
fixBranchingMinus :: (Ord k, Eq k) => BPTree k v -> BPTree k v
fixBranchingMinus bt = case node of
                  l@(Leaf h ks vs par)
                    -- removing the final key from leaf root: tree is now empty
                    | h == 0 && (length ks) == 0 -> BPTree Nil M.empty m
                    -- root & still has keys: do nothing
                    | h == 0 -> bt
                    -- proper leaf, still enough keys: fix parent from here then recurse up
                    | (length ks) >= m -> let truePar = fromJust par
                                              pnode = getNodeMap hm truePar
                                              (phm,pnode') = placePtr hm (h,ki) pnode
                                          in fixBranchingMinus (BPTree pnode' phm m)
                    -- underflow: first try to take keys from neighbors if they have any to spare
                    | isJust $ attemptRedistribute bt -> fixBranchingMinus . fromJust $ attemptRedistribute bt -- This is bad but I don't know how to do "if let" in Haskell                              
                    -- otherwise merge this with one of its neighbors under the same parent (on right by default).
                    | otherwise -> let leftNeighbor = snd <$> M.lookupLT ki (hm M.! h)
                                       rightNeighbor = snd <$> M.lookupGT ki (hm M.! h)
                                       pright = join $ getParent <$> rightNeighbor
                                       dir = (isJust rightNeighbor) && (par == pright) -- tells us which direction to go to merge
                                       neighbor = fromJust $ if dir then rightNeighbor else leftNeighbor
                                       (ml@(Leaf _ mks mvs _), mki, delk, _) = if dir then (mergeNodes node neighbor) else (mergeNodes neighbor node) -- as mentioned in merge, delk is always the smallest key on the right, which is guaranteed to exist here
                                       nki = getKeyIntvl neighbor
                                       truePar = fromJust par
                                       pnode = getNodeMap hm truePar
                                       pks = getKeys pnode
                                       pks' = L.delete delk pks --special case: if parent has only one key, this empties it, so need to allow for Maybes below
                                       pki' = kiFromKeys pks'
                                       pptr' = ((\he keyint -> (he, keyint)) (h-1)) <$> pki'
                                       ml' = Leaf h mks mvs pptr'
                                       hm' = M.adjust ((M.insert mki ml') . (M.delete nki) . (M.delete ki)) h hm
                                       (phm,pnode') = delPtr hm' (h,nki) pnode
                                       (phm',pnode'')= placePtr phm (h,mki) pnode'
                                       (phm'',pnode''') = delKey phm' delk pnode''
                                   in fixBranchingMinus $ BPTree pnode''' phm'' m
                  -- the internal cases are mostly analogous to the leaf cases, with exceptions as noted      
                  n@(Internal h ks ts par)
                    -- removing the final key from internal root: the merged node one level below is the new root, heights all go down one
                    | h == 0 && (length ks) == 0 -> let roothm = mapHeightShift (-1) (M.delete 0 hm)
                                                        newroot = recoverRootFromMap roothm  
                                                    in BPTree newroot roothm m
                    | h == 0 -> bt
                    | (length ks) >= (m-1) -> let truePar = fromJust par
                                                  pnode = getNodeMap hm truePar
                                                  (phm,pnode') = placePtr hm (h,ki) pnode
                                              in fixBranchingPlus (BPTree pnode' phm m)
                    | isJust (attemptRedistribute bt) -> fixBranchingMinus . fromJust $ attemptRedistribute bt
                    -- here the separating key in the parent node is pulled into the merged node instead of being discarded. NEED TO MAKE SURE THAT PARENTS MATCH FOR MERGE
                    | otherwise -> let leftNeighbor = snd <$> M.lookupLT ki (hm M.! h)
                                       rightNeighbor = snd <$> M.lookupGT ki (hm M.! h)
                                       pright = join $ getParent <$> rightNeighbor
                                       dir = (isJust rightNeighbor) && (par == pright)
                                       neighbor = fromJust $ if dir then rightNeighbor else leftNeighbor
                                       (mn@(Internal _ mks mts _), mki, bordk, kdir) = if dir then (mergeNodes node neighbor) else (mergeNodes neighbor node)
                                       nki = getKeyIntvl neighbor
                                       truePar = fromJust par
                                       pnode = getNodeMap hm truePar
                                       pks = getKeys pnode
                                       splitk = (if kdir then (last . (takeWhile (< bordk))) else (head . (dropWhile (> bordk)))) pks -- want the key separating node & nbhr = greatest key < bordk on right or least key > bordk on left
                                       pks' = L.delete splitk pks
                                       pki' = kiFromKeys pks'
                                       pptr' = ((\he keyint -> (he, keyint)) (h-1)) <$> pki'
                                       mks' = L.insert splitk mks
                                       mki' = (head mks', last mks')
                                       mn' = Internal h mks' mts pptr'
                                       hmfold = foldr (\ptr hash -> fst (placeParent hash (h,mki') (getNodeMap hash ptr))) hm mts -- fold over kids to correct parent pointers below
                                       hm' = M.adjust ((M.insert mki' mn') . (M.delete nki) . (M.delete ki)) h hmfold
                                       (phm,pnode') = delPtr hm' (h,nki) pnode
                                       (phm',pnode'') = delKey phm splitk pnode'
                                       (phm'',pnode''')= placePtr phm' (h,mki) pnode''
                                   in fixBranchingMinus $ BPTree pnode''' phm'' m
                where node = root bt
                      hm = heightmap bt
                      m = branchfactor bt
                      ki = getKeyIntvl node 

-- Checks if a node has a neighbor (under the same parent) with keys to spare
-- If so, redistributes a key and updates the HeightMap accordingly for all affected nodes (the two on this level, all kids, common parent)
-- Gives back whole BPTree to allow for upwards recursion in FixBranchingMinus
attemptRedistribute :: (Ord k, Eq k) => BPTree k v -> Maybe (BPTree k v)
attemptRedistribute bt = case node of
                 (Leaf _ ks vs par)
                   | par == pleft && leftExtra == Just True -> let trueNeighbor = fromJust leftNeighbor
                                                                   nks = getKeys trueNeighbor
                                                                   nvs = getVals trueNeighbor
                                                                   nki@(nlk,nrk) = (head nks, last nks) 
                                                                   v' = last nvs
                                                                   lk' = nrk
                                                                   nks' = init nks
                                                                   nvs' = init nvs
                                                                   nki' = (nlk, last nks')
                                                                   ks' = lk':ks
                                                                   vs' = v':vs
                                                                   ki' = (lk',rk)
                                                                   pnode = getNodeMap hm (fromJust par)
                                                                   pks = getKeys pnode
                                                                   pts = getKids pnode
                                                                   pts' = (L.insert (h,nki')) . (L.insert (h,ki')) . (L.delete (h,nki)) $ pts
                                                                   pks' = (L.insert lk') . (L.delete lk) $ pks
                                                                   pki' = (head pks', last pks')
                                                                   truepar' = ((h-1), pki')
                                                                   l = Leaf h ks' vs' (Just truepar')
                                                                   nl = Leaf h nks' nvs' (Just truepar')
                                                                   hm' = M.adjust ((M.insert nki' nl) . (M.insert ki' l) . (M.delete nki) . (M.delete ki)) h hm
                                                                   hmfold = foldr (\ptr hash -> fst (placeParent hash truepar' (getNodeMap hash ptr))) hm' pts'
                                                                   (phm,pnode') = delPtr hmfold (h,nki) pnode
                                                                   (phm',pnode'')= placeKey phm lk' pnode'
                                                                   (phm'',pnode''')= placePtr phm' (h,nki') pnode''
                                                                   (phm''',pnode'''') = placePtr phm'' (h,ki') pnode'''
                                                                   (phm'''',pnode''''') = delKey phm''' lk pnode''''
                                                               in Just (BPTree pnode''''' phm'''' m)
                   | par == pright && rightExtra == Just True -> let trueNeighbor = fromJust rightNeighbor
                                                                     nks = getKeys trueNeighbor
                                                                     nvs = getVals trueNeighbor
                                                                     nki@(nlk,nrk) = (head nks, last nks) 
                                                                     v' = head nvs
                                                                     rk' = nlk
                                                                     nks' = tail nks
                                                                     nvs' = tail nvs
                                                                     nlk' = head nks'
                                                                     nki' = (nlk',nrk)
                                                                     ks' = ks ++ [rk']
                                                                     vs' = vs ++ [v']
                                                                     ki' = (lk,rk')
                                                                     pnode = getNodeMap hm (fromJust par)
                                                                     pks = getKeys pnode
                                                                     pts = getKids pnode
                                                                     pts' = (L.insert (h,nki')) . (L.insert (h,ki')) . (L.delete (h,nki)) $ pts
                                                                     pks' = (L.insert rk') . (L.delete rk) $ pks
                                                                     pki' = (head pks', last pks')
                                                                     truepar' = ((h-1), pki')
                                                                     l = Leaf h ks' vs' (Just truepar')
                                                                     nl = Leaf h nks' nvs' (Just truepar')
                                                                     hm' = M.adjust ((M.insert nki' nl) . (M.insert ki' l) . (M.delete nki) . (M.delete ki)) h hm
                                                                     hmfold = foldr (\ptr hash -> fst (placeParent hash truepar' (getNodeMap hash ptr))) hm' pts'
                                                                     (phm,pnode') = delPtr hmfold (h,nki) pnode
                                                                     (phm',pnode'')= placeKey phm nlk' pnode'
                                                                     (phm'',pnode''')= placePtr phm' (h,ki') pnode''
                                                                     (phm''',pnode'''') = placePtr phm'' (h,nki') pnode'''
                                                                     (phm'''',pnode''''') = delKey phm''' nlk pnode'''''
                                                                 in Just (BPTree pnode''''' phm'''' m)
                   | otherwise -> Nothing
                 -- Here there are longer lets that do most of the key & ptr manipulation in-house, arguably looks somewhat nicer than above
                 (Internal _ ks ts par)
                   | par == pleft && leftExtra == Just True -> let trueNeighbor = fromJust leftNeighbor
                                                                   pnode = getNodeMap hm (fromJust par)
                                                                   pks = getKeys pnode
                                                                   pki = (head pks, last pks)
                                                                   pts = getKids pnode
                                                                   ppar = getParent pnode
                                                                   nks = getKeys trueNeighbor
                                                                   nts = getKids trueNeighbor
                                                                   nki@(nlk,nrk) = (head nks, last nks) 
                                                                   t' = last nts
                                                                   pk = fromJust (L.find (>nrk) pks) -- Redistribute by "pushing through" parent
                                                                   pk' = nrk
                                                                   pks' = L.insert pk' (L.delete pk pks)
                                                                   pki' = (head pks', last pks')
                                                                   nks' = init nks
                                                                   nts' = init nts
                                                                   nki' = (nlk, last nks')
                                                                   lk' = pk
                                                                   ks' = lk':ks
                                                                   ts' = t':ts
                                                                   ki' = (head ks', last ks')
                                                                   pts' =  (L.insert (h,nki')) . (L.insert (h,ki')) . (L.delete (h,nki)) $ pts
                                                                   par' = ((h-1), pki')
                                                                   p = Internal (h-1) pks' pts' ppar
                                                                   n = Internal h ks' ts' (Just par')
                                                                   nn = Internal h nks' nts' (Just par')
                                                                   hm' = M.adjust ((M.insert nki' nn) . (M.insert ki' n) . (M.delete nki) . (M.delete ki)) h hm -- safe even if ks was empty since deleting a nonexistent key has no effect
                                                                   hm'' = M.adjust ((M.insert pki' p) . (M.delete pki)) h hm'
                                                                   hmfold = foldr (\ptr hash -> fst (placeParent hash par' (getNodeMap hash ptr))) hm'' pts'
                                                               in Just (BPTree p hmfold m)
                   | par == pright && rightExtra == Just True -> let trueNeighbor = fromJust rightNeighbor
                                                                     pnode = getNodeMap hm (fromJust par)
                                                                     pks = getKeys pnode
                                                                     pki = (head pks, last pks)
                                                                     pts = getKids pnode
                                                                     ppar = getParent pnode
                                                                     nks = getKeys trueNeighbor
                                                                     nts = getKids trueNeighbor
                                                                     nki@(nlk,nrk) = (head nks, last nks) 
                                                                     t' = head nts
                                                                     pk = fromJust (L.find (>rk) pks) -- Redistribute by "pushing through" parent
                                                                     nks' = tail nks
                                                                     nts' = tail nts
                                                                     nki' = (head nks', nrk)
                                                                     pk' = nlk
                                                                     pks' = L.insert pk' (L.delete pk pks)
                                                                     pki' = (head pks', last pks')
                                                                     rk' = pk
                                                                     ks' = ks ++ [rk']
                                                                     ts' = ts ++ [t']
                                                                     ki' = (head ks', last ks')
                                                                     pts' = (L.insert (h,nki')) . (L.insert (h,ki')) . (L.delete (h,nki)) $ pts
                                                                     par' = ((h-1), pki')
                                                                     p = Internal (h-1) pks' pts' ppar
                                                                     n = Internal h ks' ts' (Just par')
                                                                     nn = Internal h nks' nts' (Just par')
                                                                     hm' = M.adjust ((M.insert nki' nn) . (M.insert ki' n) . (M.delete nki) . (M.delete ki)) h hm
                                                                     hm'' = M.adjust ((M.insert pki' p) . (M.delete pki)) h hm'
                                                                     hmfold = foldr (\ptr hash -> fst (placeParent hash par' (getNodeMap hash ptr))) hm'' pts'
                                                                 in Just (BPTree p hmfold m)
                   | otherwise -> Nothing
               where node = root bt
                     hm = heightmap bt
                     m = branchfactor bt
                     ki@(lk,rk) = getKeyIntvl node
                     h = getHeight node
                     leftNeighbor = snd <$> M.lookupLT ki (hm M.! h)
                     pleft = join $ getParent <$> leftNeighbor
                     leftExtra = (hasExtraKey m) <$> leftNeighbor
                     rightNeighbor = snd <$> M.lookupGT ki (hm M.! h)
                     pright = join $ getParent <$> rightNeighbor
                     rightExtra = (hasExtraKey m) <$> rightNeighbor

-- Checks if a neighbor has a spare key
hasExtraKey :: (Ord k, Eq k) => BranchFactor -> Node k v -> Bool
hasExtraKey m node = case node of
            (Leaf _ ks _ _)
              | (length ks) == m -> False
              | otherwise -> True
            (Internal _ ks _ _)
              | (length ks) == (m-1) -> False
              | otherwise -> True

-- Maybe to Bool
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- Merges nodes (assumes first argument node is on the left and that heights & parents match, alternatively these could be implemented as checks)
-- Gives back merged node, its key interval, and a key together with direction (right by default)
-- This is the key that is deleted from the parent in the case of leaves (in which case there will always be a key on the right)
-- For internal nodes, one or the other key lists may be empty so we need key + direction to ensure we can always find the splitting key in the parent
mergeNodes :: (Ord k, Eq k) => Node k v -> Node k v -> (Node k v, KeyInterval k, k, Direction)
mergeNodes (Leaf h ks vs par) (Leaf _ ks' vs' _) = let mks = ks++ks'
                                                       mvs = vs++vs'
                                                       mki = (head mks, last mks)
                                                       delk = head ks'
                                                   in (Leaf h mks mvs par, mki, delk, True)
mergeNodes (Internal h ks kids par) (Internal _ ks' kids' _) = let mks = ks++ks'
                                                                   mkids = kids++kids'
                                                                   mki = (head mks, last mks)
                                                                   kdir = not $ null ks'
                                                                   bordk = if kdir then (head ks') else (last ks)
                                                               in (Internal h mks mkids par, mki, bordk, kdir)
mergeNodes _ _ = error "Mismatched nodes" -- If node types don't match

-- Deletes a key from a node and updates HeightMap accordingly
-- Note that this does not reinsert the node into the height map if it has no keys remaining!
-- Therefore, it's important that it happens last in any hm modification chain
delKey :: (Ord k, Eq k) => HeightMap k v -> k -> Node k v -> (HeightMap k v, Node k v)
delKey hm x Nil = (hm, Nil)
delKey _ _ (Leaf _ _ _ _) = error "Deleting from leaf" -- this should never come up the way deletion works right now but it can't hurt to write it
delKey hm x n@(Internal h ks ts par) = let ks' = (L.delete x ks)
                                           n' = Internal h ks' ts par
                                           ki = getKeyIntvl n
                                           ki' = getKeyIntvl n'
                                           hm' = M.adjust (M.delete ki) h hm
                                           hm'' = if (null ks') then hm' else M.adjust (M.insert ki' n') h hm'
                                         in (hm'',n')
                                        
-- Parent-to-child pointer updating helper function - intended to add back pointer to child updated in insertion/deletion process
-- This change is crystallized by storing it in the HeightMap
delPtr :: (Ord k, Eq k) => HeightMap k v -> TreePtr k -> Node k v -> (HeightMap k v, Node k v)
delPtr hm ptr node = case node of
                Nil -> error "Parent is empty"
                (Leaf _ _ _ _) -> error "Parent is leaf"
                (Internal h ks ts par) -> let ts' = L.delete ptr ts
                                              n' = Internal h ks ts' par
                                              ki = getKeyIntvl node
                                              hm' = M.adjust (M.insert ki n') h hm -- overwrites old copy of node
                                          in (hm',n')
                                          
-- Helper function for getting KeyIntervals that can handle the case of an empty key list
-- (Comes up when merging erases the last key from the root node)
kiFromKeys :: (Ord k, Eq k) => Keys k -> Maybe (KeyInterval k)
kiFromKeys [] = Nothing
kiFromKeys ks = Just (head ks, last ks)
  
-- Helpers

empty :: Int -> BPTree Int Int
empty = BPTree Nil M.empty

emptyS :: Int -> BPTree Int String
emptyS = BPTree Nil M.empty

makeTree :: Int -> Int -> BPTree Int Int
makeTree n b = fromList (zip [1..n] [1..]) (empty b)

makeTree' :: Int -> Int -> BPTree Int Int
makeTree' n b = fromList (zip [n,(n-1)..1] [n,(n-1)..1]) (empty b)

fromList :: (Ord k, Eq k) => [(k,v)] -> BPTree k v -> BPTree k v  
fromList kvs t =
   foldr (\(x,y) acc -> insert acc x y) t kvs
   
t :: BPTree Int Int
t = makeTree 19 3

-- TESTS

main :: IO ()
main = mapM_ print [t, delete t 6, delete (delete t 6) 1] 
