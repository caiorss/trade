{-# language TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.Mark where

import           Data.Time.Extended

import           Control.Applicative
import           Control.Lens hiding (each)
import           Control.Monad.State.Strict (State)
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Semigroup as Semi
import           Data.Text (Text)
import           Data.Text.Binary()
import           Data.Text.Encoding
import           Data.Time
import           Formatting
import           GHC.Generics (Generic)
import           Pipes.Binary

{- a Mark is a reduced information set representing a market condition at a single point in time -}

data MarkAction = BidUp | BidDown | AskUp | AskDown | TickUp | TickDown | BadId | BadTime | BadTrade | MarkError ByteString deriving (Ord, Show, Eq, Read, Generic)

instance Binary MarkAction

data Mark = Mark
    { _mTime :: UTCTime
    , _mBidPrice :: Double
    , _mAskPrice :: Double
    , _mPrice    :: Double
    , _mBidVolume :: Int
    , _mAskVolume :: Int
    , _mVolume    :: Int
    , _mLastTickId :: Int
    , _mTotalVolume :: Int
    , _mNotes :: [MarkAction]
    } deriving (Show, Eq, Generic)

makeLenses ''Mark

instance Binary Mark

{- not commutative

given an existing mark and a new mark, what's the combined mark
leans to the right wrt time (which is useful to avoid time travel effects),
and bid and ask volume (which is what the iqfeed data set is like)

-}

instance Semi.Semigroup Mark where
    (Mark t bp ap p _ _ v tickid tv ns) <> (Mark t' bp' ap' p' bv' av' v' tickid' tv' ns') =
        Mark t' bp' ap' p' bv' av' (v+v') tickid' tv' (ns <> ns' <> newns)
      where
        newns =
            [BidUp | bp' > bp] <>
            [AskUp | ap' > ap] <>
            [BidDown | bp' < bp] <>
            [AskDown | ap' < ap] <>
            [TickUp | p' > p] <>
            [TickDown | p' < p] <>
            [BadId | v' >0 && tickid' <= tickid] <>
            [BadTime | t' < t] <>
            [BadTrade | tv' /= tv + v']

-- Mark accumulator
data MarkX = MarkX
             { _xOpen    :: Maybe Mark
             , _xClose   ::      [Mark]
             } deriving (Show)

instance Monoid MarkX where
  mempty = MarkX Nothing []

  mappend (MarkX o0 c0) (MarkX o1 c1) =
      case o0 of
          Nothing -> MarkX o1 c
          Just m0 -> case o1 of
              Nothing -> MarkX o0 c
              Just m1 ->
                  if t' == Just 0
                  then MarkX (Just (m0 Semi.<> m1)) c
                  else MarkX o1 (m0:c)
    where
      t0 = view mTime <$> o0
      t1 = view mTime <$> o1
      t' = diffUTCTime <$> t1 <*> t0
      c = c0 <> c1

-- rendering
renderMark :: (ByteString, Mark) -> Text
renderMark (sym,m) =
    sformat (stext % " ") (decodeUtf8 sym) <>
    fTimeMilli (utctDayTime (m ^. mTime)) <>
    sformat (left 15 ' ' %. (" " % float % ":" % float % ":" % float))  (_mBidPrice m) (_mAskPrice m) (_mPrice m) <>
    sformat (left 10 ' ' %. (" " % int % ":" % int % ":" % int))  (_mBidVolume m) (_mAskVolume m) (_mVolume m) <>
    if not (null ns)
    then sformat (" " % shown) ns
    else mempty
  where
    ns = _mNotes m

-- statistics
data StatsMark = StatsMark
     { _smarkCount :: Int
     , _smarkSymbolCount :: Map.Map ByteString Int
     , _smarkNoteCount :: Map.Map MarkAction Int
     , _smarkSymbolVolumeSum :: Map.Map ByteString Int
     } deriving (Show, Eq, Read , Generic)

instance Binary StatsMark

makeLenses ''StatsMark

initialSmark :: StatsMark
initialSmark =  StatsMark 0 mempty mempty mempty

smarkUpdate :: (ByteString, Mark) -> (State StatsMark) ()
smarkUpdate (sym,m) = do
    let notes = _mNotes m
        vol = _mVolume m
    smarkCount += 1
    smarkSymbolCount %= Map.insertWith (+) sym 1
    smarkNoteCount %= Map.unionWith (+) (Map.fromList $ (,) <$> notes <*> pure 1)
    smarkSymbolVolumeSum %= Map.insertWith (+) sym vol

smarkRender :: StatsMark -> Text
smarkRender s =
    sformat ("Count: " % int % "\n") (s ^. smarkCount) <>
    "Count by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. smarkSymbolCount) <> "\n" <>
    "Count by Note: " <>
    Map.foldWithKey (\k a b -> b <> sformat (shown % ":" % int % " ") k a)
    mempty (s ^. smarkNoteCount) <> "\n" <>
    "Volume by Symbol: " <>
    Map.foldWithKey (\k a b -> b <> sformat (stext % ":" % int % " ") (decodeUtf8 k) a)
    mempty (s ^. smarkSymbolVolumeSum)
