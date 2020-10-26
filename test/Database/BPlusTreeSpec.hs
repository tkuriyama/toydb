module Database.BPlusTreeSpec where

import qualified Database.BPlusTree as BPT
import qualified Data.Map as M
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
    shouldThrow,
    anyException
  )
  
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
   describe "empty tree tests" $ do
     it "search empty returns nil" $  
       (BPT.search (BPT.empty 2) 1) `shouldBe` BPT.Nil

   describe "Height 1 tree" $ do
     it  "3 nodes branching factor 2 == height 1" $
       (M.keys $ BPT.heightmap (BPT.makeTree 3 2)) `shouldBe` [0]
     it  "4 nodes branching factor 2 == height 2" $
       (M.keys $ BPT.heightmap (BPT.makeTree 4 2)) `shouldBe` [0, 1]
       

-- t :: BPTree Int Int
-- t = makeTree 10

-- t' :: BPTree Int Int
-- t' = makeTree' 6




-- module BTreeTest where

-- import BTree


-- -- TEST DATA


-- -- In 'Nil m', the m determines the branching of the BTree: it is 2m - 1 
-- empty :: Tree Int
-- empty = Nil 2

-- makeTree :: Int -> Tree Int
-- makeTree n = fromList [1..n] empty

-- -- CONSTRUCT TREE FROM LIST
-- -- > fromList [1..99] (Nil 2) |> depth
-- --   5
-- fromList :: (Ord a) => [a] -> Tree a -> Tree a  
-- fromList as t =
--    foldr (\a acc -> insert acc a) t as


-- t99 :: Tree Int
-- t99 = makeTree 99



-- -- BTREE TESTS

-- -- From Wikipedia:
-- -- According to Knuth's definition, a B-tree of order m is a tree which satisfies the following properties:

-- -- (A) Every node has at most m children.
-- -- (B) Every non-leaf node (except root) has at least ⌈m/2⌉ child nodes.
-- -- (C) The root has at least two children if it is not a leaf node.
-- -- (D) A non-leaf node with k children contains k − 1 keys.
-- -- (E) All leaves appear in the same level and carry no information.


-- -- (A)
-- -- > maxChildren $ makeTree 99
-- -- 4
-- maxChildren :: (Ord a) => Tree a -> Int
-- maxChildren (Nil _) = 0
-- maxChildren (Leaf _ _) = 0
-- maxChildren (Node _ _ ts) = max (length ts) (maxList $ map maxChildren ts)

    
-- -- (B)
-- -- > enoughChildNodes 2 t99
-- --   True
-- --
-- -- > enoughChildNodes 3 t99
-- --   False
-- enoughChildNodes :: (Ord a) => Int -> Tree a -> Bool
-- enoughChildNodes _ (Nil _) = True
-- enoughChildNodes _ (Leaf _ _) = True
-- enoughChildNodes k (Node _ _ ts) = (length ts >= k) && (andAll $ map (enoughChildNodes k) ts)


-- -- (C)
-- rootOK :: (Ord a) => Tree a -> Bool
-- rootOK (Nil _) = True
-- rootOK (Leaf _ _) = True
-- rootOK (Node _ _ ts) = length ts >= 2

-- --(D)
-- -- > nodesOK t99
-- --   True
-- nodesOK :: (Ord a) => Tree a -> Bool
-- nodesOK (Nil _) = True
-- nodesOK (Leaf _ _) = True
-- nodesOK node@(Node _ ks ts)  =  nodeOK node && (andAll $ map nodeOK ts)

-- -- (E)
-- wellFormed :: (Ord a) => Tree a -> Bool
-- wellFormed t = (length $ lastLevel t)  == (length $ flatten t)


-- -- HELPERS FOR A--E

-- children :: (Ord a) =>  Tree a -> [Tree a]
-- children (Nil _) = []
-- children (Leaf _ _) = []
-- children (Node _ _ ts) = ts

-- nodeOK :: (Ord a) => Tree a -> Bool
-- nodeOK (Nil _) = True
-- nodeOK (Leaf _ _) = True
-- nodeOK (Node _ ks ts) = (length ts) - 1 == length ks

-- -- > lastLevel $ tn 10
-- --   [Leaf 2 [1,2],Leaf 2 [4],Leaf 2 [6],Leaf 2 [8],Leaf 2 [10]]
-- lastLevel :: (Ord a) => Tree a -> [Tree a]
-- lastLevel t = 
--   childrenN (depth t) t

-- childrenN :: (Ord a) => Int -> Tree a -> [Tree a]
-- childrenN k t = childrenN_ (k - 1) $ children t

-- childrenN_ ::  (Ord a) => Int -> [Tree a] -> [Tree a]
-- childrenN_ 0 ts = ts
-- childrenN_ n ts = concat $ map children (childrenN_ (n - 1) ts)

-- flatten :: (Ord a) => Tree a -> [Tree a]
-- flatten (Nil k) = [(Nil k)]
-- flatten (Leaf a b) = [Leaf a b]
-- flatten (Node a b ts) = concat $ map flatten ts


-- -- TREE HELPER FUNCTIONS

-- depth :: Tree a -> Int
-- depth (Nil _ )= 0
-- depth (Leaf _ _) = 0
-- depth (Node _ _ ts) = 1 + maxList (map depth ts)


-- -- number of nodes
-- nodes :: Tree a -> Int
-- nodes (Nil _ )= 1
-- nodes (Leaf _ _) = 1
-- nodes (Node _ _ ts) = 1 + sum (map depth ts)


-- -- MATH AND LOGIC HELPERS 

-- andAll :: [Bool] -> Bool
-- andAll [] = True
-- andAll (x:[]) = x
-- andAll (x:xs) = x && (andAll xs)

-- maxList :: (Ord a) => [a] -> a
-- maxList (x:[]) = x
-- maxList (x:xs) = max x (maxList xs)


-- minList :: (Ord a) => [a] -> a
-- minList (x:[]) = x
-- minList (x:xs) = min x (minList xs)

-- -- CONVENIENCE

-- (|>) :: a -> (a -> b) -> b
-- (|>) a f = f a
