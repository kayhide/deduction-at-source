{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Safe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens
import Control.Monad
import Reflex.Dom

someFunc :: IO ()
someFunc = mainWidget $ do
  calculator

numberInput :: MonadWidget t m => Double -> m (TextInput t)
numberInput d = textInput $ def
                & textInputConfig_initialValue .~ show d
                & textInputConfig_attributes .~ constDyn (Map.fromList [("size", "60")])
                & textInputConfig_inputType .~ "number"

labeledWith :: MonadWidget t m => Text -> m a -> m a
labeledWith label w = do
  x <- el "div" $ do
    el "span" $ text $ Text.unpack label
    x <- w
    return x
  return x

spanText :: MonadWidget t m => String -> m ()
spanText s = el "span" $ text s

calculator :: MonadWidget t m => m ()
calculator = do
  el "h1" $ text "源泉徴収計算機"
  (price, deductionRate, consumptionRate) <- el "dl" $ do
    el "dt" $ text "料金"
    p <- el "dd" $ do
      x <- numberInput 100
      spanText "円"
      return x
    el "dt" $ text "徴収率"
    d <- el "dd" $ do
      x <- numberInput 10.21
      spanText "%"
      return x
    el "dt" $ text "消費税率"
    c <- el "dd" $ do
      x <- numberInput 8.0
      spanText "%"
      return x
    return (p, d, c)

  el "h2" $ text "計算"
  (deductionValue, consumptionValue) <- el "dl" $ do
    -- mp <- mapDyn maybeDouble (value price)
    -- md <- mapDyn maybeDouble (value deductionRate)
    -- mc <- mapDyn maybeDouble (value consumptionRate)
    d <- combineDyn apply' (value price) (value deductionRate)
    el "dt" $ text "徴収額"
    el "dd" $ dynText =<< mapDyn (maybe "" show) d
      -- dynText $ value price
    c <- combineDyn apply' (value price) (value consumptionRate)
    el "dt" $ text "消費税額"
    el "dd" $ dynText =<< mapDyn (maybe "" show) c
    -- el "dt" $ text "支払額"
    -- el "dd" $ dynText c
    return (d, d)
  return ()
  where maybeDouble :: String -> Maybe Double
        maybeDouble = readMay
        apply' :: String -> String -> Maybe Int
        apply' x y = do
          dx <- readMay x :: Maybe Double
          dy <- readMay y :: Maybe Double
          return $ floor $ dx * dy * 0.01
