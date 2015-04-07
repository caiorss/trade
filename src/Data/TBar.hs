{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- TBar is trade bar (no bid/ask)
module Data.TBar where

import           Control.Applicative
import           Control.Category ((>>>))
import qualified Control.Foldl as L
import           Control.Foldl.Incremental
import           Control.Foldl.Incremental.Histogram
import           Control.Foldl.Extended
import           Control.Lens hiding (to, each)
import           Control.Monad
import           Control.SFold
import           Control.SFold.Util
import           Data.Binary (Binary)
import           Data.ByteString (ByteString)
import           Data.Histogram
import           Data.Monoid
import qualified Data.Semigroup as Semi
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Extended
import           Data.Trade (Trade(..))
import           Formatting
import           GHC.Generics (Generic)
import           MVC
import           MVC.Extended
import qualified Pipes.Prelude as Pipes
import           Pipes.Extended hiding (ma)

data TBar = TBar
    { _tbarCloseTime :: UTCTime
    , _tbarFirstTime :: UTCTime
    , _tbarLastTime  :: UTCTime
    , _tbarPrice     :: Double
    , _tbarVolume    :: Int
    } deriving (Show, Eq, Generic)

makeLenses ''TBar

instance Binary TBar

instance Semi.Semigroup TBar where
    (TBar ct ft lt _ v) <> (TBar ct' ft' lt' p' v') =
        TBar ct'' ft'' lt'' p' (v+v')
      where
        ct'' = Prelude.max ct ct'
        ft'' = Prelude.min ft ft'
        lt'' = Prelude.max lt lt'

data TBarX = TBarX
     { _xOpen    :: Maybe TBar
     , _xClosed  :: [TBar]
     } deriving (Show, Eq)

makeLenses ''TBarX

toTBar :: Trade -> TBar
toTBar (Trade t p v) =
    TBar t t t p v

instance Monoid TBarX where
    mempty = TBarX Nothing []
    mappend (TBarX o0 c0) (TBarX o1 c1) =
        case o0 of
             Nothing -> TBarX o1 c
             Just m0 -> case o1 of
                 Nothing -> TBarX o0 c
                 Just m1 -> case compare (m0 ^. tbarCloseTime) (m1 ^. tbarCloseTime) of
                     EQ -> TBarX (Just (m0 Semi.<> m1)) c
                     LT -> TBarX (Just m1) (c <> [m0])
                     GT -> TBarX (Just m0) (c <> [m1])
      where
        c = c0 <> c1

release :: TBarX -> (TBarX, [TBar])
release (TBarX o c) = (TBarX o [], c)

flush :: TBarX -> TBarX
flush (TBarX o c) = case o of
    Nothing -> TBarX Nothing c
    Just o' -> TBarX Nothing (c <> [o'])

timeFold :: NominalDiffTime -> SFold Trade TBar
timeFold grain = SFold toNextClose mappend mempty release flush 
  where
    toNextClose (Trade t p v) =
        TBarX (Just $ TBar (ceilingTime t grain) t t p v) []

volumeFold :: Int -> SFold Trade TBar
volumeFold grain = SFold toVol step mempty release flush
  where
    step x0 x1 = case o0' of
        Nothing -> TBarX o1' c'
        Just m0 -> case o1' of
            Nothing -> TBarX o0' c'
            Just m1 -> if m0 ^. tbarVolume + m1 ^. tbarVolume < grain
                       then TBarX (Just (m0 Semi.<> m1)) c'
                       else TBarX mrem (c' <> [mfirst] <> mquot)
              where
                m' = m0 Semi.<> m1
                (_, cm') = budVol m'
                mfirst = tbarVolume .~ grain $ Prelude.head cm'
                (mrem, mquot) = budVol (tbarVolume .~
                                        (m1 ^. tbarVolume - (grain - m0 ^. tbarVolume))
                                        $ m1)
      where
        (TBarX o0' c0') = budVolTBarX x0
        (TBarX o1' c1') = budVolTBarX x1
        c' = c0' <> c1'

    toVol m = uncurry TBarX (budVol (toTBar m))

    budVol :: TBar -> (Maybe TBar, [TBar])
    budVol x = (,)  
               (if vrem >0
                then Just (tbarVolume .~ vrem $ x)
                else Nothing)
               (replicate vquot (tbarVolume .~ grain $ x))
      where
        (vquot, vrem) = quotRem (x ^. tbarVolume) grain

    budVolTBarX :: TBarX -> TBarX
    budVolTBarX x@(TBarX o c) = case o of
        Nothing -> x
        Just m -> TBarX o' (c <> c')
          where
            (o',c') = budVol m

renderTBar :: TBar -> Text
renderTBar tbar =
    fTimeMilli (utctDayTime $ _tbarCloseTime tbar) <>
    fTimeMilli (utctDayTime $ _tbarFirstTime tbar) <>
    fTimeMilli (utctDayTime $ _tbarLastTime tbar) <>
    sformat (Formatting.left 15 ' ' %. (" " % Formatting.float))  (_tbarPrice tbar) <>
    sformat (Formatting.left 10 ' ' %. (" " % int))  (_tbarVolume tbar)


testTrade :: FilePath
testTrade = "data/test-trade.binary"

testFile :: String
testFile = "data/test-tbar.binary"
    
getTBarFile :: FilePath -> FilePath -> ByteString -> Int -> IO ()
getTBarFile file fileout sym n = runMVC ()
                 (asPipe (Pipes.take n >->
                          Pipes.filter ((==sym) . fst) >->
                          Pipes.map snd) >>>
                  asGetter (sfold (timeFold 1)) >>>
                  asPipe Pipes.Extended.flatten)

                 ((,) <$>
                  outBinary fileout <*>
                  (inBinary file :: Managed (Controller (ByteString, Trade))))

lookTBarFile :: FilePath -> IO [TBar]
lookTBarFile file = fromController 100000000 (inBinary file)

lookTradeFile :: FilePath -> IO [(ByteString,Trade)]
lookTradeFile file = fromController 100000000 (inBinary file)

tbarStats :: FilePath -> IO (Double, Double, Double)
tbarStats file =
    L.purely Pipes.fold ((,,) <$> ma 1.0 <*> L.premap abs (ma 1.0) <*> std 1.0) $
        readBinaryFileUnsafe file >->
        Pipes.map (view tbarPrice) >->
        L.purely Pipes.scan delta >->
        justP >->
        Pipes.map (/0.25)

tbars :: Producer TBar IO ()
tbars = readBinaryFileUnsafe testFile

tbarUnconditionalHist :: (Monad m) => Producer TBar m () -> m (Histogram BinD Double)
tbarUnconditionalHist p =
    L.purely Pipes.fold (incHist (binDn (-4.5) 1.0 4.5) 1.0) $
        p >->
        Pipes.map (view tbarPrice) >->
        L.purely Pipes.scan delta >->
        justP >->
        Pipes.map (/0.25) >->
        truncate' (-4) 4
  where
    truncate' mini maxi = forever $ do
        a <- await
        if | a < mini -> yield mini
           | a > maxi -> yield maxi
           | otherwise -> yield a
