{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Tick where

import Data.Trade
import           Control.SFold
import           Control.SFold.Util
import           Data.Time.Extended

import           Control.Applicative
import           Control.Lens hiding (to, each)
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Time
import           Formatting
import Data.Binary (Binary)
import qualified Data.Map as Map
import GHC.Generics (Generic)
-- import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.State.Strict (State)
import Control.DeepSeq (NFData)

data Tick = Tick
    { _tickTime :: UTCTime
    , _tickdTime :: Double
    , _tickTick :: Double
    , _tickVolume :: Int
    } deriving (Show, Eq, Generic)

makeLenses ''Tick

instance Binary Tick

instance NFData Tick

data TickX = TickX { _xOpen :: Maybe Trade, _xClose :: [Tick] }

toTick :: Trade -> Trade -> Tick
toTick (Trade t p _)  (Trade t' p' v') = Tick t' (toDouble $ diffUTCTime t' t) (p' - p) v' 

instance Monoid TickX where
    mempty = TickX Nothing []
    mappend (TickX o0 c0) (TickX o1 c1) = case tod of
        Nothing     -> TickX Nothing c
        Just (o,c') -> TickX (Just o) (c<>c')
      where
        c = c0 <> c1
        tod = orMaybe (\x -> (x,[])) (\x x' -> (x', [toTick x x'])) o0 o1

toX :: Trade -> TickX
toX tr = TickX (Just tr) []

relX :: TickX -> (TickX, [Tick])
relX (TickX t c) = (TickX t [], c)

trade2tick :: SFold Trade Tick 
trade2tick = SFold toX mappend mempty relX id

trade2tickZeros :: NominalDiffTime -> SFold Trade Tick 
trade2tickZeros grain = SFold toX maZero mempty relX id
  where
    maZero (TickX t0 c0) (TickX t1 c1) =
        case tod of
            Nothing     -> TickX Nothing c
            Just (o,c') -> TickX (Just o) (c <> c')
      where
        c = c0 <> c1
        tod = orMaybe (\x -> (x,[])) (\x x' -> (x', toTickZero x x')) t0 t1
        toTickZero tr tr' = zeroFill <> [toTick tr tr']
          where
            zeroFill = (\x -> Tick (addUTCTime (grain * fromInteger x)
                                      (tr ^. trTime)) (toDouble grain) 0 0) <$> [1..n]
            n = timeSteps (tr' ^. trTime) (tr ^. trTime) grain

renderTick :: Tick -> Text
renderTick d =
    fTimeMilli (utctDayTime $ _tickTime d) <>
    sformat (left 10 ' ' %. (" " % fixed 3)) (_tickdTime d) <>
    sformat (left 10 ' ' %. (" " % float))   (_tickTick d) <>
    sformat (left 10 ' ' %. (" " % int))     (_tickVolume d)

renderTick' :: (ByteString, Tick) -> Text
renderTick' (sym, tick) =
    sformat (stext % " ") (decodeUtf8 sym) <>
    renderTick tick

-- statistics
data StatsTick = StatsTick
     { _stickCount :: Int
     , _stickSymbolCount :: Map.Map ByteString Int
     , _stickSymbolTickCount :: Map.Map ByteString (Map.Map Double Int)
     , _stickSymbolVolumeSum :: Map.Map ByteString Int
     } deriving (Show, Eq, Read, Generic)

instance Binary StatsTick

makeLenses ''StatsTick

initialStick :: StatsTick
initialStick =  StatsTick 0 mempty mempty mempty

stickUpdate :: (ByteString, Tick) -> (State StatsTick) ()
stickUpdate (sym,x) = do
    stickCount += 1
    stickSymbolCount %= Map.insertWith (+) sym 1
    stickSymbolVolumeSum %= Map.insertWith (+) sym (x ^. tickVolume)
    stickSymbolTickCount %= Map.insertWith
        (Map.unionWith (+)) sym (Map.singleton (x ^. tickTick) 1)

stickRender :: StatsTick -> Text
stickRender s =
    sformat ("Count: " % int % "\n") (s ^. stickCount) <>
    "Count by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. stickSymbolCount) <> "\n" <>
    "Volume by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. stickSymbolVolumeSum)
