--Much simpler version of BPTree based on Taro's BTree
--2DO
--misc. improvements: 
  --use Maps for the leaf storage
  --extra utility functions
--extra features:
  --bulk loading
  --more robust searching

module Database.BPlusTreeV2 where
 
import qualified Data.List as L

type BranchFactor = Int
type Keys k = [k]
type Values v = [v]
type Trees k v = [Tree k v]

data BPTree k v = BPTree {branchfactor :: BranchFactor
                          , tree :: Tree k v}
                  deriving Eq

data Tree k v = 
  Nil
  | Leaf (Keys k) (Values v)
  | Internal (Keys k) (Trees k v)
  deriving Eq


-- LOOKUP FUNCTIONS

-- This finds the first index greater than k
-- E.g., th. index at which k should be inserted to maintain order
findIndex :: Ord k => Keys k -> k -> Int
findIndex ks k = case L.findIndex (> k) ks of
                   (Just i) -> i
                   Nothing -> length ks

-- Given key, descend to the leaf node that could contain it
searchTree :: Ord k => Tree k v -> k -> Maybe (Tree k v)
searchTree t x =
  case t of
    Nil -> Nothing
    (Leaf ks _) -> if x `elem` ks then (Just t) else Nothing
    (Internal ks ts) -> let idx = findIndex ks x
                            child = ts !! idx
                        in searchTree child x

-- Given key, look up the associated value
locate :: Ord k => BPTree k v -> k -> Maybe v
locate bt x =
  case searchTree (tree bt) x of
    (Just (Leaf ks vs)) -> (vs !!) <$> (L.elemIndex x ks)
    _ -> Nothing


-- INSERTION FUNCTIONS

-- Inserts key-value pair into BPtree
-- Wraps insertTree, also creates new root when appropriate
insert :: Ord k => BPTree k v -> k -> v -> BPTree k v
insert bt x y = case insertTree t m x y of
       ([],[t']) -> BPTree m t'
       (ks,ts) -> BPTree m (Internal ks ts)
  where t = tree bt
        m = branchfactor bt

-- Inserts key-value pair into Tree node
-- Maintains the invariant by splitting nodes as necessary
insertTree :: Ord k => Tree k v -> BranchFactor -> k -> v 
                       -> (Keys k, Trees k v)
insertTree t m x y = 
      case t of
      --empty tree becomes singleton leaf
        Nil -> ([],[Leaf [x] [y]])
      --for leaf, insert key directly here and split if necessary
      --pass up ([],newtree) if no split
      --([separatingkey],[splitleft,splitright]) if split
        (Leaf ks vs) -> 
              case () of _
                          --no overwrite
                          | x `elem` ks -> ([], [t])
                          --over invariant
                          | isOver -> let (tl,tr,sepk) = split t' m
                                      in ([sepk],[tl,tr])
                          | otherwise -> ([],[t'])
                where idx = findIndex ks x
                      (ksl,ksr) = splitAt idx ks
                      (vsl,vsr) = splitAt idx vs
                      ks' = ksl ++ [x] ++ ksr
                      vs' = vsl ++ [y] ++ vsr
                      t' = Leaf ks' vs'
                      isOver = (length ks') == (2*m)      
       --for internal, first figure out the proper subtree to recurse to
       --once we have the return result from there:
       --insert the passed-up key here if there is one, may cause split
        (Internal ks ts) -> case isOver of
                              False -> ([], [t'])
                              True -> let (tl,tr,sepk) = split t' m
                                      in ([sepk],[tl,tr])
           where idx = findIndex ks x
                 (ksl,ksr) = splitAt idx ks
                 (tsl,child:tsr) = splitAt idx ts
                 (newks,newts) = insertTree child m x y
                 ks' = ksl ++ newks ++ ksr
                 ts' = tsl ++ newts ++ tsr
                 t' = Internal ks' ts'
                 isOver = (length ts') == (2*m)

-- Splits an overflowing node into two, also yields the separating key
split :: Ord k => Tree k v -> BranchFactor -> (Tree k v, Tree k v, k)
split Nil _ = error "Can't split an empty tree"
split (Leaf ks vs) m = let splitk = ks !! m
                           (ksl,ksr) = splitAt m ks
                           (vsl,vsr) = splitAt m vs
                           leafl = Leaf ksl vsl
                           leafr = Leaf ksr vsr
                       in (leafl, leafr, splitk)
split (Internal ks ts) m = let (ksl,splitk:ksr) = splitAt (m-1) ks
                               (tsl,tsr) = splitAt m ts
                               nodel = Internal ksl tsl
                               noder = Internal ksr tsr
                           in (nodel, noder, splitk)
                           
 
-- DELETION FUNCTIONS

-- Removes key-value pair from BPtree
-- Wraps deleteTree, reduces root to Nil when there are no more keys
delete :: Ord k => BPTree k v -> k -> BPTree k v
delete bt x = case deleteTree [t] [] m x of
       ([],[Leaf [] []]) -> BPTree m Nil
       ([],[t']) -> BPTree m t'
  where t = tree bt
        m = branchfactor bt

-- Removes key-value pair from Tree node
-- Maintains the invariant by stealing keys/merging nodes as necessary
deleteTree :: Ord k => Trees k v -> Keys k -> BranchFactor -> k 
                       -> (Keys k, Trees k v)
deleteTree nbhrts sks m x = 
      case t of
      --empty tree remains empty
        Nil -> ([],[Nil])
      --for leaf, remove key directly here
      --if a neighbor has a spare key, move it over and report up
      --otherwise, merge nodes
        (Leaf ks vs) -> 
              case () of _
                          --root leaf
                          | null sks -> ([],[t'])
                          --leaf with still enough keys
                          | not isUnder -> (sks,maybelt++[t']++maybert)
                          --right neighbor has an extra key
                          | foldr (const . hasExtra m) False maybert -> 
                              let (Leaf nks nvs) = maybert !! 0
                                  ks'' = ks' ++ [head nks]
                                  vs'' = vs' ++ [head nvs]
                                  nks' = tail nks
                                  nvs' = tail nvs
                                  t'' = Leaf ks'' vs''
                                  rt' = Leaf nks' nvs'
                              in (maybelk++[head nks'],
                                  maybelt++[t'',rt'])
                           --left neighbor has an extra key
                           | foldr (const . hasExtra m) False maybelt -> 
                              let (Leaf nks nvs) = maybelt !! 0
                                  ks'' = (last nks):ks'
                                  vs'' = (last nvs):vs'
                                  nks' = init nks
                                  nvs' = init nvs
                                  t'' = Leaf ks'' vs''
                                  lt' = Leaf nks' nvs'
                              in ([last nks]++mayberk,
                                  [lt',t'']++maybert)
                           --merge with left neighbor
                           | null maybert -> let lt = maybelt !! 0
                                                 mt = merge lt t' []
                                             in (mayberk,mt:maybert)
                           --merge with right neighbor
                           | otherwise -> let rt = maybert !! 0
                                              mt = merge t' rt []
                                          in (maybelk,maybelt++[mt])                                     
          where delIndex i xs = (take i xs) ++ (drop (i+1) xs)
                (ks',vs') = maybe (ks,vs) 
                           (\i -> (delIndex i ks, delIndex i vs)) 
                           (L.elemIndex x ks)
                t' = Leaf ks' vs'
                isUnder = (length ks') < m               
       --for internal, first figure out the proper subtree to recurse to
       --once we have the return result from there: shift/merge as necc.
       --(doing so will use the appropriate seperating key)
       --either way pass up lists of modified keys and nodes
        (Internal ks ts) -> 
              case () of _
                          --root node
                          | null sks -> ([],[t'])
                          --node with still enough children
                          | not isUnder -> (sks,maybelt++[t']++maybert)
                          --right neighbor has an extra key
                          | foldr (const . hasExtra m) False maybert -> 
                              let (Internal nks nts) = maybert !! 0
                                  rk = mayberk !! 0
                                  ks'' = ks' ++ [rk]
                                  ts'' = ts' ++ [head nts]
                                  nks' = tail nks
                                  nts' = tail nts
                                  t'' = Internal ks'' ts''
                                  rt' = Internal nks' nts'
                              in (maybelk++[head nks],
                                  maybelt++[t'',rt'])
                           --left neighbor has an extra key
                           | foldr (const . hasExtra m) False maybelt -> 
                              let (Internal nks nts) = maybelt !! 0
                                  lk = maybelk !! 0
                                  ks'' = lk:ks'
                                  ts'' = (last nts):ts'
                                  nks' = init nks
                                  nts' = init nts
                                  t'' = Internal ks'' ts''
                                  lt' = Internal nks' nts'
                              in ([last nks]++mayberk,
                                  [lt',t'']++maybert)
                           --merge with left neighbor
                           | null maybert -> let lt = maybelt !! 0
                                                 lk = maybelk !! 0
                                                 mt = merge lt t' [lk]
                                             in (mayberk,mt:maybert)
                           --merge with right neighbor
                           | otherwise -> let rt = maybert !! 0
                                              rk = mayberk !! 0
                                              mt = merge t' rt [rk]
                                          in (maybelk,maybelt++[mt])                                     
          where idx' = findIndex ks x
                ksl = take (idx'-1) ks
                ksr = drop (idx'+1) ks
                sks' = L.intersect (drop (idx'-1) ks) (take (idx'+1) ks)
                tsl = take (idx'-1) ts
                tsr = drop (idx'+2) ts
                nnei = length sks'
                nbhrts' = if nnei == 3 then take 3 (drop (idx'-1) ts)
                          else (if idx' == 0 then take 2 ts
                                else drop (idx'-1) ts)
                (newks,newts) = deleteTree nbhrts' sks' m x
                ks' = ksl++newks++ksr
                ts' = tsl++newts++tsr
                t' = Internal ks' ts' 
                isUnder = (length ks') < (m-1)
      where idx = findIndex sks x
            t = nbhrts !! idx
            maybelt = take idx nbhrts
            maybert = drop (idx+1) nbhrts
            maybelk = take idx sks
            mayberk = drop idx sks
            
-- Determines if a node has a spare key
hasExtra :: Ord k => BranchFactor -> Tree k v -> Bool
hasExtra _ Nil = False
hasExtra m (Leaf ks _) = (length ks) > m
hasExtra m (Internal ks _) = (length ks) > (m-1)

-- Merges two nodes into one
-- The extra Keys arguments is for adding the parent's separating key
-- This is done only in the Internal case
merge :: Ord k => Tree k v -> Tree k v -> Keys k -> Tree k v
merge (Leaf ks vs) (Leaf ks' vs') _ = Leaf (ks++ks') (vs++vs')
merge (Internal ks ts) (Internal ks' ts') sk = 
                                     Internal (ks++sk++ks') (ts++ts')
merge _ _ _ = error "Mismatched node types in merge"


-- SHOW FUNCTIONS

showBPT :: (Show k, Show v) => BPTree k v -> String
showBPT bt = "Branching factor: " ++ show m ++ "\n" ++ showTree 0 t
  where t = tree bt
        m = branchfactor bt

showTree :: (Show k, Show v) => Int -> Tree k v -> String
showTree _ Nil = "" 
showTree n (Leaf ks vs) = 
  (concat $ replicate n "    ") ++ show (zip ks vs) ++ "\n"
showTree n (Internal ks ts) =
  (concat $ replicate n "    ") ++ show ks 
  ++ "\n" ++ L.concatMap (showTree (n+1)) ts


-- CLASS INSTANTIATIONS

--Could maybe also make into a Semigroup/Monoid under union
--But doesn't seem natural to do so

instance (Show k, Show v) => Show (Tree k v) where
  show = showTree 0
                    
instance (Show k, Show v) => Show (BPTree k v) where
  show = showBPT
  
instance Functor (Tree k) where
  fmap _ Nil = Nil
  fmap f (Internal ks ts) = Internal ks (map (fmap f) ts)
  fmap f (Leaf ks vs) = Leaf ks (map f vs)

instance Functor (BPTree k) where
  fmap f bt = BPTree (branchfactor bt) (fmap f $ tree bt)

instance Foldable (Tree k) where
  foldMap _ Nil = mempty
  foldMap f (Internal _ ts) = foldMap (foldMap f) ts
  foldMap f (Leaf _ vs) = foldMap f vs

instance Foldable (BPTree k) where
  foldMap f = (foldMap f) . tree

instance Traversable (Tree k) where
  traverse _ Nil = pure Nil
  traverse f (Internal ks ts) = 
    Internal ks <$> (traverse (traverse f) ts)
  traverse f (Leaf ks vs) = Leaf ks <$> (traverse f vs)

instance Traversable (BPTree k) where
  traverse f bt = BPTree (branchfactor bt) <$> (traverse f $ tree bt)


-- EXAMPLES
empty :: BranchFactor -> BPTree k v
empty m = BPTree m Nil

makeTree :: Int -> Int -> BPTree Int Int
makeTree n m = fromList (zip [n,(n-1)..1] [n,(n-1)..1]) (empty m)

fromList :: Ord k => [(k,v)] -> BPTree k v -> BPTree k v  
fromList kvs t =
   foldr (\(x,y) acc -> insert acc x y) t kvs

t :: BPTree Int Int
t = makeTree 7 2


-- TESTS

main :: IO ()
main = mapM_ print [t, delete t 6, delete (delete t 6) 1]
