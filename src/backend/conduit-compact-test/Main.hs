{-# LANGUAGE LambdaCase #-}

module Main where

import Test.Hspec
import Data.Conduit
import Control.Monad
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE
import Pfizer.Datalake.Util.Conduit
import qualified Data.Conduit.List as CL

import Test.Hspec.QuickCheck (modifyMaxSize)

main :: IO ()
main = hspec $ do
  describe "compactSources" $ modifyMaxSize (const 10000) $ do
    it "is id if the inputs are equal" $ do
      property $ \xs -> do
        let c1 = CL.sourceList xs :: ConduitM () Int IO ()
            c2 = compactConduits2 c1 c1 :: ConduitM () Int IO ()
        expected <- runConduit $ c1 .| CL.consume
        actual <- runConduit $ c2 .| CL.consume
        actual `shouldBe` expected

    it "retains all right-elements" $ do
      let c1 = CL.sourceList []
          c2 = CL.sourceList [0,0]
          cc = compactConduits2 c1 c2

      compacted <- runConduit $ cc .| CL.consume
      compacted `shouldBe` [0,0]

    it "is prefixed with the left values" $ do
      property $ \(xs1,xs2) -> do
        let c1 = CL.sourceList xs1 :: ConduitM () Int IO ()
            c2 = CL.sourceList xs2 :: ConduitM () Int IO ()
            cc = compactConduits2 c1 c2 :: ConduitM () Int IO ()
        compacted <- runConduit $ cc .| CL.consume

        let
          sames = (\(x1,x2) -> x1 == x2) <$> zip xs1 compacted

        and sames `shouldBe` True

    modifyMaxSize (const 100) $ it "should not drop values" $ do
      property $ \xss -> do
        let
          xssIx =
            (\(idx,xs) -> CL.sourceList (zip (repeat idx) xs)) <$>
            zip [1..] xss
          cc = compactConduitsWith snd xssIx
        compacted <- runConduit $ cc .| CL.consume

        let
          isOf :: Int -> NE.NonEmpty (Int, Int) -> Bool
          isOf n xs = any (\(n',_) -> n' == n) $ NE.toList xs

        forM_ (zip [1..] xss) $ \(idx, ref) ->
          mapMaybe
            (\xs -> if isOf idx xs then Just (snd $ NE.head xs) else Nothing)
            compacted `shouldBe`
            ref

mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs
{-# NOINLINE [1] mapMaybe #-}


compactConduits2
  :: (Monad m, Eq a)
  => ConduitM () a m ()
  -> ConduitM () a m ()
  -> ConduitM () a m ()
compactConduits2 left right =
  compactConduitsWith id [left, right] .| CL.map NE.head
