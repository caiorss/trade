{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Data.CBar where

import           Control.Lens hiding (to, each)
import           Control.Monad.Trans.State.Strict (State)
import           Control.SFold
import           Control.SFold.Util
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Mark
import           Data.Monoid
import qualified Data.Semigroup as Semi
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Time
import           Data.Time.Extended
import           Formatting
import           GHC.Generics (Generic)

data CBar =
  CBar {_cbarCloseTime :: UTCTime
       ,_cbarFirstTime :: UTCTime
       ,_cbarLastTime :: UTCTime
       ,_cbarBidPrice :: Double
       ,_cbarAskPrice :: Double
       ,_cbarPrice :: Double
       ,_cbarBidVolume :: Int
       ,_cbarAskVolume :: Int
       ,_cbarVolume :: Int}
  deriving (Show,Eq,Generic)

makeLenses ''CBar

instance Binary CBar

instance Semi.Semigroup CBar where
  (CBar ct ft lt _ _ _ _ _ v) <> (CBar ct' ft' lt' bp' ap' p' bv' av' v') = 
    CBar ct'' ft'' lt'' bp' ap' p' bv' av' (v + v')
    where ct'' = max ct ct'
          ft'' = min ft ft'
          lt'' = max lt lt'

data CBarX =
  CBarX {_xOpen :: Maybe CBar
        ,_xClosed :: [CBar]}
  deriving (Show,Eq)

makeLenses ''CBarX

toCbar :: Mark -> CBar
toCbar (Mark t bp ap' p bv av v _ _ _) = 
  CBar t t t bp ap' p bv av v

instance Monoid CBarX where
  mempty = CBarX Nothing []
  mappend (CBarX o0 c0) (CBarX o1 c1) = 
    case o0 of
      Nothing -> CBarX o1 c
      Just m0 -> 
        case o1 of
          Nothing -> CBarX o0 c
          Just m1 -> 
            case compare (m0 ^. cbarCloseTime)
                         (m1 ^. cbarCloseTime) of
              EQ -> 
                CBarX (Just (m0 Semi.<> m1)) c
              LT -> 
                CBarX (Just m1)
                      (c <>
                       [m0])
              GT -> 
                CBarX (Just m0)
                      (c <>
                       [m1])
    where c = c0 <> c1

release :: CBarX -> (CBarX,[CBar])
release (CBarX o c) = (CBarX o [],c)

flush :: CBarX -> CBarX
flush (CBarX o c) = 
  case o of
    Nothing -> CBarX Nothing c
    Just o' -> 
      CBarX Nothing
            (c <>
             [o'])

timeFold :: NominalDiffTime -> SFold Mark CBar
timeFold grain = 
  SFold toNextClose mappend mempty release flush
  where toNextClose (Mark t bp ap' p bv av v _ _ _) = 
          CBarX (Just $
                 CBar (ceilingTime t grain) t t bp ap' p bv av v)
                []

volumeFold :: Int -> SFold Mark CBar
volumeFold grain = 
  SFold toVol step mempty release flush
  where step x0 x1 = 
          case o0' of
            Nothing -> CBarX o1' c'
            Just m0 -> 
              case o1' of
                Nothing -> CBarX o0' c'
                Just m1 -> 
                  if m0 ^. cbarVolume + m1 ^. cbarVolume < grain
                     then CBarX (Just (m0 Semi.<> m1)) c'
                     else CBarX mrem
                                (c' <>
                                 [mfirst] <>
                                 mquot)
                  where m' = m0 Semi.<> m1
                        (_,cm') = budVol m'
                        mfirst = cbarVolume .~ grain $ head cm'
                        (mrem,mquot) = 
                          budVol (cbarVolume .~
                                  (m1 ^. cbarVolume -
                                   (grain - m0 ^. cbarVolume)) $
                                  m1)
          where (CBarX o0' c0') = budVolCBarX x0
                (CBarX o1' c1') = budVolCBarX x1
                c' = c0' <> c1'
        toVol m = 
          uncurry CBarX (budVol (toCbar m))
        budVol :: CBar -> (Maybe CBar,[CBar])
        budVol x = 
          (,) (if vrem > 0
                  then Just (cbarVolume .~ vrem $ x)
                  else Nothing)
              (replicate vquot (cbarVolume .~ grain $ x))
          where (vquot,vrem) = 
                  quotRem (x ^. cbarVolume) grain
        budVolCBarX :: CBarX -> CBarX
        budVolCBarX x@(CBarX o c) = 
          case o of
            Nothing -> x
            Just m -> CBarX o' (c <> c')
              where (o',c') = budVol m

mark2TimeBar :: NominalDiffTime -> SFold (ByteString,Mark) (ByteString,CBar)
mark2TimeBar grain = keyFold (timeFold grain)

mark2VolumeBar :: Int -> SFold (ByteString,Mark) (ByteString,CBar)
mark2VolumeBar grain = keyFold (volumeFold grain)

renderCBar :: CBar -> Text
renderCBar cbar = 
  fTimeMilli (utctDayTime $ _cbarCloseTime cbar) <>
  sformat (left 15 ' ' %.
           (" " % float % ":" % float % ":" % float))
          (_cbarBidPrice cbar)
          (_cbarAskPrice cbar)
          (_cbarPrice cbar) <>
  sformat (left 10 ' ' %.
           (" " % int % ":" % int % ":" % int))
          (_cbarBidVolume cbar)
          (_cbarAskVolume cbar)
          (_cbarVolume cbar)

renderCBar' :: (ByteString,CBar) -> Text
renderCBar' (sym,cbar) = 
  sformat (stext % " ")
          (decodeUtf8 sym) <>
  fTimeMilli (utctDayTime $ _cbarCloseTime cbar) <>
  sformat (left 15 ' ' %.
           (" " % float % ":" % float % ":" % float))
          (_cbarBidPrice cbar)
          (_cbarAskPrice cbar)
          (_cbarPrice cbar) <>
  sformat (left 10 ' ' %.
           (" " % int % ":" % int % ":" % int))
          (_cbarBidVolume cbar)
          (_cbarAskVolume cbar)
          (_cbarVolume cbar)

-- statistics
data StatsCBar =
  StatsCBar {_scbarCount :: Int
            ,_scbarSymbolCount :: Map.Map ByteString Int
            ,_scbarSymbolVolumeSum :: Map.Map ByteString Int}
  deriving (Show,Eq,Read,Generic)

instance Binary StatsCBar

makeLenses ''StatsCBar

initialScbar :: StatsCBar
initialScbar = StatsCBar 0 mempty mempty

scbarUpdate :: (ByteString,CBar) -> (State StatsCBar) ()
scbarUpdate (sym,c) = 
  do scbarCount += 1
     scbarSymbolCount %=
       Map.insertWith (+) sym 1
     scbarSymbolVolumeSum %=
       Map.insertWith (+)
                      sym
                      (c ^. cbarVolume)

scbarRender :: StatsCBar -> Text
scbarRender s = 
  sformat ("Count: " % int % "\n")
          (s ^. scbarCount) <>
  "Count by Symbol: " <>
  Map.foldWithKey 
    (\k a b -> 
       b <>
       sformat (stext % ":" % int % " ")
               (decodeUtf8 k)
               a)
    mempty
    (s ^. scbarSymbolCount) <>
  "\n" <>
  "Volume by Symbol: " <>
  Map.foldWithKey 
    (\k a b -> 
       b <>
       sformat (stext % ":" % int % " ")
               (decodeUtf8 k)
               a)
    mempty
    (s ^. scbarSymbolVolumeSum)
