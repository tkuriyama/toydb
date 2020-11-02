module Database.BTreeSpec where

import qualified Database.BTree as BT

import qualified Data.List.NonEmpty as NE

import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===), property)

spec :: Spec
spec = do
  describe "insertBT" $ do
    
    it "singleton" $
      (BT.insertBT 1 'a' $ BT.emptyBT 4)
      `shouldBe` BT.BTree 4 (BT.Node BT.Empty $ (1, 'a', BT.Empty) NE.:| [])
      
    it "qcheck: size is always consistent" $ property $
      \ord y -> let (ord', y') = (max 3 $ abs ord, abs y)
                in (BT.sizeBT $ mkIntTree ord' y') == y'
                   
    it "qtcheck: BTree invariants 1, 2, 3" $ property $
      \ord y -> let (ord', y') = (max 3 $ abs ord, abs y)
                in (BT.validBT $ mkIntTree ord' y') == True

  describe "findBT" $ do
    
    it "qcheck: min and max keys are always present" $ property $
      \ord y -> let (ord', y') = (max 3 $ abs ord, (abs y) + 1)
                in (isJust $ BT.findBT (y'-1) $ mkIntTree ord' y') == True
                   && (isJust $ BT.findBT 0 $ mkIntTree ord' y') == True

    it "qcheck: min-1 and max+ keys are never present" $ property $
      \ord y -> let (ord', y') = (max 3 $ abs ord, (abs y) + 1)
                in (isJust $ BT.findBT y' $ mkIntTree ord' y') == False
                   && (isJust $ BT.findBT (-1) $ mkIntTree ord' y') == False

  describe "deleteBT" $ do

    it "qcheck: size is always consistent" $ property $
      \ord y i -> let (ord', y') = (max 3 $ abs ord, (abs y) + 1)
                      i' = max 0 (min i (y' - 1))
                      t = mkIntTree ord' y'
                  in (BT.sizeBT t - 1) == (BT.sizeBT $ BT.deleteBT i' t)

    it "qcheck: deleting non-existent key has no effect" $ property $
      \ord y -> let (ord', y') = (max 3 $ abs ord, (abs y) + 1)
                    t = mkIntTree ord' y'
                in t == BT.deleteBT (y'+1) t

    it "qtcheck: BTree invariants 1, 2, 3" $ property $
      \ord y i -> let (ord', y') = (max 3 $ abs ord, abs y)
                      i' = max 0 (min i y')
                  in (BT.validBT $ BT.deleteBT i' $ mkIntTree ord' y') == True

------------------------ Helpers ------------------------  
mkIntTree :: BT.Order -> Int -> BT.BTree Int String
mkIntTree ord n = foldr f (BT.emptyBT ord) pairs
  where pairs = zipWith (\k v -> (k, 'v':(show v))) [0..(n-1)] [0..]
        f (k, v) t = BT.insertBT k v t
                        
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
