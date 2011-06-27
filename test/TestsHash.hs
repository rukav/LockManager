module Main where
import qualified Control.Concurrent.THashTable as T
import Control.Concurrent.STM

import Data.List
import Data.Maybe

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = 
    [ testProperty "toFrom" prop_toFrom
    , testProperty "lookup" prop_lookup
    , testProperty "insert" prop_insert
    , testProperty "delete" prop_delete
    , testProperty "update" prop_update
    ]

prop_toFrom = monadicIO $ do
   xs <- pick (arbitrary :: Gen [(String,String)])
   tbl <- run $ atomically $ T.fromList 16 T.hashString xs
   xs' <- run $ atomically $ T.toList tbl
   assert $ sort xs == sort xs'

prop_lookup = monadicIO $ do
   ys <- pick (arbitrary :: Gen [(String,String)])
   let xs = nubBy (\x y -> fst x == fst y) ys
   tbl <- run $ atomically $ T.fromList 16 T.hashString xs
   xs' <- run $ atomically $ mapM (\(k,_) -> T.lookup k tbl) xs
   assert $ sort (catMaybes xs') == sort (map snd xs)

prop_insert = monadicIO $ do
   xs <- pick (arbitrary :: Gen [(String,String)])
   tbl <- run $ atomically $ T.empty 16 T.hashString
   run $ atomically $ mapM_ (\(k,v) -> T.insert k v tbl) xs
   xs' <- run $ atomically $ T.toList tbl
   assert $ sort xs == sort xs'

prop_delete = monadicIO $ do
   xs <- pick (arbitrary :: Gen [(String,String)])
   tbl <- run $ atomically $ T.fromList 16 T.hashString xs
   run $ atomically $ mapM_ (\(k,_) -> T.delete k tbl) xs
   xs' <- run $ atomically $ T.toList tbl
   assert $ null xs'

prop_update = monadicIO $ do
   ys <- pick (arbitrary :: Gen [(String,String)])
   let xs = nubBy (\x y -> fst x == fst y) ys
   tbl <- run $ atomically $ T.fromList 16 T.hashString xs
   run $ atomically $ mapM_ (\(k,_) -> T.update k k tbl) xs
   xs' <- run $ atomically $ T.toList tbl
   assert $ sort xs' == sort (map (\(k,_) -> (k,k)) xs)
