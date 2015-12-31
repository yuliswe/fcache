{-# LANGUAGE MultiParamTypeClasses #-}
import qualified Cache.State as S
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Cache.Internal
import Data.IORef
import Test.Hspec
import Prelude hiding (lookup)

main :: IO ()
main = hspec $ do
   describe "Cache.Internal" $ do
      it "newCache Nothing" $ do
         let cache = newCache Nothing :: Cache Int Int
         size cache `shouldBe` 0
         limit cache `shouldBe` Nothing
      it "newCache $ Just 2" $ do
         let cache = newCache $ Just 2 :: Cache Int Int
         size cache `shouldBe` 0
         limit cache `shouldBe` Just 2
      it "setLimit $ Just 3" $ do
         let cache = setLimit (Just 3) $ newCache Nothing  :: Cache Int Int
         limit cache `shouldBe` Just 3
      it "insert no limit" $ do
         let cache = insert 0 0 $ newCache Nothing :: Cache Int Int
         limit cache `shouldBe` Nothing
         size cache `shouldBe` 1
         let cache' = insert 1 1 $ cache
         limit cache' `shouldBe` Nothing
         size cache' `shouldBe` 2
      it "dumpOldest" $ do
         let cache = dumpOldest $ 
                     insert 1 1 $ 
                     insert 0 0 $ 
                     newCache Nothing :: Cache Int Int
         lookup 1 cache `shouldBe` Just 1
         size cache `shouldBe` 1
         limit cache `shouldBe` Nothing
   describe "Cache.State" $ do
      it "withCache" $ do
         let cache = newCache Nothing :: Cache Int Int
         let f = do
               modify (insert 0 0)
               modify (insert 1 1)
         evalState (f >> S.withCache (return . h) 0) cache `shouldBe` 0
         evalState (f >> S.withCache (return . h) 1) cache `shouldBe` 1
         evalState (f >> S.withCache (return . h) 2) cache `shouldBe` 2
         evalState (f >> S.withCache (return . h) 2 >> gets size) cache `shouldBe` 3
   where h 2 = 2
   