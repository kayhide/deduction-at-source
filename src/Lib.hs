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
import Numeric
import Control.Lens
import Control.Monad
import Reflex.Dom

someFunc :: IO ()
someFunc = mainWidget $ do
  calculator

numberInput :: (Show a, MonadWidget t m) => a -> m (TextInput t)
numberInput x = textInput $ def
                & textInputConfig_initialValue .~ show x
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
  (priceInput, deductionRateInput, consumptionRateInput) <- el "dl" $ do
    el "dt" $ text "料金"
    p <- el "dd" $ do
      x <- numberInput (1000000 :: Int)
      spanText "円"
      return x
    el "dt" $ text "徴収率"
    d <- el "dd" $ do
      x <- numberInput (10.21 :: Double)
      spanText "%"
      return x
    el "dt" $ text "消費税率"
    c <- el "dd" $ do
      x <- numberInput (8.0 :: Double)
      spanText "%"
      return x
    return (p, d, c)

  mp <- mapDyn readMaybeDouble $ value priceInput
  mdr <- mapDyn ((fmap (/100)) . readMaybeDouble) $ value deductionRateInput
  mcr <- mapDyn ((fmap (/100)) . readMaybeDouble) $ value consumptionRateInput
  el "h2" $ text "計算"
  el "dl" $ do
    md <- mapDyn (liftM ceiling') =<< combineDyn (liftM2 (*)) mp mdr
    el "dt" $ text "徴収額"
    el "dd" $ do
      dynText $ value priceInput
      text " x "
      dynText =<< mapDyn showDouble mdr
      text " = "
      dynText =<< mapDyn (maybe "" show) md
    mc <- mapDyn (liftM floor') =<< combineDyn (liftM2 (*)) mp mcr
    el "dt" $ text "消費税額"
    el "dd" $ do
      dynText $ value priceInput
      text " x "
      dynText =<< mapDyn showDouble mcr
      text " = "
      dynText =<< mapDyn (maybe "" show) mc
    sum' <- mapDyn (liftM floor') mp
    sum'' <- combineDyn (liftM2 (-)) sum' md
    sum <- combineDyn (liftM2 (+)) sum'' mc
    el "dt" $ text "支払額"
    el "dd" $ do
      dynText $ value priceInput
      text " - "
      dynText =<< mapDyn (maybe "" show) md
      text " + "
      dynText =<< mapDyn (maybe "" show) mc
      text " = "
      dynText =<< mapDyn (maybe "" show) sum
    return ()
  return ()
  where readMaybeDouble = readMay :: String -> Maybe Double
        floor' = floor :: (RealFrac a) => a -> Int
        ceiling' = ceiling :: (RealFrac a) => a -> Int
        showInt :: Maybe Int -> String
        showInt (Just x) = show x
        showInt Nothing = ""
        showDouble :: Maybe Double -> String
        showDouble (Just x) = Numeric.showFFloat (Just 4) x ""
        showDouble Nothing = ""
