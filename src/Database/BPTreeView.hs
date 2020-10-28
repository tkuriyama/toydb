module Database.BPTreeView where 

import Database.BPlusTree 


-- data Node k v =
--   Nil
--   | Leaf Height (Keys k) (Values v) (Maybe (TreePtr k))
--   | Internal Height (Keys k) [Tree Ptr k] (Maybe (TreePtr k))
--   deriving (Show, Eq)


-- The usual approach to processing a tree will not work as far
-- as I can see.  This is because `Node k v` is not a recursive type:
-- it does not refer to itself.  In particular, the children of
-- node are not themselves nodes.  More precisely, neither
-- `keys` nor `children` is a list of nodes.
--
-- The code below is incorrrect 
-- 
-- view :: (Show k, Show v) => Node k v -> String
-- view (Nil)= "Nil"
-- view (Leaf _ keys values _) = show keys ++ ": " ++ show values
-- view (Internal _ keys children _) = show keys ++ "--" ++ show (map view children)


tt1 :: BPTree Int Int
tt1 = makeTree 10 2