{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Trade where

import qualified Data.CBar as CBar

import           Control.SFold
import           Control.SFold.Util
import           Data.Time.Extended

import           Control.Lens hiding (to, each)
import           Control.Monad.Trans.State.Strict (State)
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Semigroup as Semi
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Time
import           Formatting
import           GHC.Generics (Generic)

data Trade = Trade
    { _trTime :: UTCTime
    , _trPrice :: Double
    , _trVolume :: Int
    } deriving (Show, Eq, Read, Generic)

makeLenses ''Trade

instance Binary Trade

instance Semi.Semigroup Trade where
    (Trade t p v) <> (Trade t' p' v') =
        if t' > t
        then Trade t' p' (v+v')
        else Trade t p (v+v')

toTrade :: CBar.CBar -> Trade
toTrade b =
    Trade
    (b ^. CBar.cbarCloseTime)
    (b ^. CBar.cbarPrice)
    (b ^. CBar.cbarVolume)

cbar2trade :: SFold CBar.CBar Trade
cbar2trade = SFold toTrades mappend mempty release id
  where
    release x = ([],x)

    toTrades :: CBar.CBar -> [Trade]
    toTrades b = if b ^. CBar.cbarVolume == 0
            then []
            else [Trade
                 (b ^. CBar.cbarCloseTime)
                 (b ^. CBar.cbarPrice)
                 (b ^. CBar.cbarVolume)]

cbar2tradeKey :: SFold (ByteString, CBar.CBar) (ByteString, Trade)
cbar2tradeKey = keyFold cbar2trade

-- not sure if a timeFold is needed
data TradeX = TradeX
     { _xOpen    :: Maybe Trade
     , _xClosed  :: [Trade]
     } deriving (Show, Eq)

makeLenses ''TradeX

instance Monoid TradeX where
    mempty = TradeX Nothing []
    mappend (TradeX o0 c0) (TradeX o1 c1) =
        case o0 of
             Nothing -> TradeX o1 c
             Just m0 -> case o1 of
                 Nothing -> TradeX o0 c
                 Just m1 -> case compare (m0 ^. trTime) (m1 ^. trTime) of
                     EQ -> TradeX (Just (m0 Semi.<> m1)) c
                     LT -> TradeX (Just m1) (c <> [m0])
                     GT -> TradeX (Just m0) (c <> [m1])
      where
        c = c0 <> c1

release :: TradeX -> (TradeX, [Trade])
release (TradeX o c) = (TradeX o [], c)

flush :: TradeX -> TradeX
flush (TradeX o c) = case o of
    Nothing -> TradeX Nothing c
    Just o' -> TradeX Nothing (c <> [o'])

timeFold :: NominalDiffTime -> SFold CBar.CBar Trade
timeFold grain = SFold toNextClose mappend mempty release flush 
  where
    toNextClose b = if v == 0
                    then mempty
                    else TradeX (Just $ Trade (ceilingTime t grain) p v) []
      where
        t = b ^. CBar.cbarCloseTime
        p = b ^. CBar.cbarPrice
        v = b ^. CBar.cbarVolume

renderTrade :: Trade -> Text
renderTrade trade =
    fTimeMilli (utctDayTime $ _trTime trade) <>
    sformat (left 10 ' ' %. (" " % float)) (_trPrice trade) <>
    sformat (left 10 ' ' %. (" " % int))  (_trVolume trade)

renderTrade' :: (ByteString, Trade) -> Text
renderTrade' (sym, trade) =
    sformat (stext % " ") (decodeUtf8 sym) <>
    renderTrade trade

-- statistics
data StatsTrade = StatsTrade
     { _stCount :: Int
     , _stSymbolCount :: Map.Map ByteString Int
     , _stSymbolVolumeSum :: Map.Map ByteString Int
     } deriving (Show, Eq, Read, Generic)

instance Binary StatsTrade

makeLenses ''StatsTrade

initialStrade :: StatsTrade
initialStrade =  StatsTrade 0 mempty mempty

stradeUpdate :: (ByteString, Trade) -> (State StatsTrade) ()
stradeUpdate (sym,x) = do
    stCount += 1
    stSymbolCount %= Map.insertWith (+) sym 1
    stSymbolVolumeSum %= Map.insertWith (+) sym (x ^. trVolume)

stradeRender :: StatsTrade -> Text
stradeRender s =
    sformat ("Count: " % int % "\n") (s ^. stCount) <>
    "Count by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. stSymbolCount) <> "\n" <>
    "Volume by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. stSymbolVolumeSum)

sameTime :: SFold Trade Trade
sameTime = SFold toX mappend mempty release flush 
  where
    toX tr = TradeX (Just tr) []
