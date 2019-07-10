{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Frames.Visualization.VegaLite.Correlation
  (
    correlationCircles
  , correlationCirclesFromFrame
  , toCorrelationRows
  )
where

import qualified Frames.Visualization.VegaLite.Data
                                               as D

import           Graphics.Vega.VegaLite.Configuration
                                                ( ViewConfig(..)
                                                , configuredVegaLite
                                                )

import qualified Control.Foldl                 as FL
import           Control.Monad                  ( when )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Frames                        as F
import qualified Graphics.Vega.VegaLite        as GV



-- | Correlation charts
toCorrelationRows
  :: (b -> T.Text)
  -> S.Set b
  -> (b -> b -> a)
  -> [F.Record ['("L1", T.Text), '("L2", T.Text), '("C", a)]]
toCorrelationRows toText parameters getCorr =
  let allPairs = [ (x, y) | x <- S.toList parameters, y <- S.toList parameters ]
      addCorr (x, y) = (x, y, getCorr x y)
      makeRec (x, y, c) = toText x F.&: toText y F.&: c F.&: V.RNil
  in  fmap (makeRec . addCorr) allPairs

correlationCircles
  :: forall a b 
  . (D.ToVLDataValue (F.ElField '("C", a))
    , RealFrac a
    , Show a
    , Ord a
    )
  => (b -> T.Text)
  -> S.Set b
  -> (b -> b -> a)
  -> Bool
  -> T.Text
  -> ViewConfig
  -> Either T.Text GV.VegaLite
correlationCircles toText parameters corr removeDiag title vc =
  let rows = toCorrelationRows toText parameters corr
  in correlationCirclesFromFrame @'("L1",T.Text) @'("L2",T.Text) @'("C", a) removeDiag title vc rows

correlationCirclesFromFrame
  :: forall p q c rs f
   . ( D.DataFieldOf rs p
     , D.DataFieldOf rs q
     , D.DataFieldOf rs c
     , RealFrac (V.Snd c)
     , Show (V.Snd c)
     , Ord (V.Snd c)
     , V.Snd p ~ T.Text
     , V.Snd q ~ T.Text
     , Foldable f
     )
  => Bool
  -> Text
  -> ViewConfig
  -> f (D.Row rs)
  -> Either T.Text GV.VegaLite
correlationCirclesFromFrame removeDiag title vc rows = do
  let (pValues, qValues, cValues, cMin, cMax) = FL.fold
        (   (,,,,)
        <$> FL.premap (F.rgetField @p) FL.set
        <*> FL.premap (F.rgetField @q) FL.set
        <*> FL.premap (F.rgetField @c) FL.set
        <*> FL.premap (F.rgetField @c) FL.minimum
        <*> FL.premap (F.rgetField @c) FL.maximum
        )
        rows
  when (pValues /= qValues)
    $  Left
    $  "mismatched labels in rows given to correlationBoxes. {p}="
    <> (T.pack $ show pValues)
    <> " and {p}="
    <> (T.pack $ show qValues)
  case cMin of
    Nothing -> Left "No data!"
    Just x  -> when (x < -1.01) $ Left $ "correlation value < -1 in data ({c}=" <> T.pack (show cValues) <> ")!"
  case cMax of
    Nothing -> Left "No data!"
    Just x  -> when (x > 1.01) $ Left $ "correlation value > 1 in data ({c}=" <> T.pack (show cValues) <> ")!"

  let dat      = D.recordsToVLData (F.rcast @'[p, q, c]) D.defaultParse rows
      trans    = GV.transform
                 . GV.calculateAs ("abs(datum." <> D.colName @c <> ")") "absCorr"
                 . if removeDiag
                   then
                     GV.filter (GV.FCompose $ GV.Not (GV.Expr $ "datum." <> D.colName @p <> " == datum." <> D.colName @q))
                   else
                     id
      cEnc     = GV.color [D.mName @c
                          , GV.MmType GV.Quantitative
                          , GV.MLegend []
                          , GV.MScale [GV.SScheme "redyellowgreen" []
                                      ]
                          ]
                 . GV.size [GV.MName "absCorr", GV.MmType GV.Quantitative, GV.MLegend []]
      posEnc     = GV.position GV.X [D.pName @p, GV.PmType GV.Nominal, GV.PAxis [GV.AxTitle ""]]
                   . GV.position GV.Y [D.pName @q, GV.PmType GV.Nominal, GV.PAxis [GV.AxTitle ""]]
--                   . GV.tooltip [D.tName @p, GV.TmType GV.Nominal]
--                   . GV.tooltip [D.tName @q, GV.TmType GV.Nominal]
--                   . GV.tooltip [D.tName @c, GV.TmType GV.Quantitative]

      mark          = GV.mark GV.Circle []
--      configuration = GV.configure . viewConfigAsHvega vc
      vl            = configuredVegaLite vc $
        [ GV.title title
        , dat
        , trans []
        , (GV.encoding . cEnc . posEnc) []
        , mark       
        ]
  return vl



{-
-- | Histograms
-- | Single, stacked, side-by-side, and faceted

data MultiHistogramStyle = StackedBar | AdjacentBar

singleHistogram
  :: forall x rs f
   . (D.DataFieldOf rs x, Foldable f, Real (V.Snd x))
  => Text -- ^ Title
  -> Maybe T.Text -- ^ optional label for "Count" on y-axis
  -> Int -- ^ number of bins
  -> AxisBounds (V.Snd x) -- ^ bounds for x in histogram calculation
  -> Bool -- ^ if true, add out of range values to first/last count
  -> ViewConfig
  -> f (D.Row rs)
  -> GV.VegaLite
singleHistogram title yLabelM nBins xBounds addOutOfRange vc@(ViewConfig width _ _) rows
  = let
      yLabel   = fromMaybe "count" yLabelM
      xLabel   = D.colName @x
      bandSize = (width / realToFrac nBins) - 10.0
      vecX     = FL.fold
        (FL.premap (realToFrac . F.rgetField @x) (FL.vector @VU.Vector))
        rows
      (minM, maxM) = case xBounds of
        GivenMinMax lo hi -> (Just lo, Just hi)
        _                 -> (Nothing, Nothing)
      minVal = fromMaybe (VU.minimum vecX) (fmap realToFrac minM)
      maxVal = fromMaybe (VU.maximum vecX) (fmap realToFrac maxM)
      bins   = H.binD minVal nBins maxVal
      hVec   = makeHistogram addOutOfRange bins vecX
      toVLRow (bv, ct) =
        GV.dataRow [(xLabel, GV.Number bv), (yLabel, GV.Number ct)] []
      dat =
        GV.dataFromRows [] $ List.concat $ VB.toList $ fmap toVLRow $ VB.convert
          hVec
      encX          = GV.position GV.X [D.pName @x, GV.PmType GV.Quantitative]
      encY = GV.position GV.Y [GV.PName yLabel, GV.PmType GV.Quantitative]
      hBar          = GV.mark GV.Bar [GV.MBinSpacing 1, GV.MSize bandSize]
      hEnc          = encX . encY
      configuration = GV.configure . viewConfigAsHvega vc
      vl            = GV.toVegaLite
        [GV.title title, dat, (GV.encoding . hEnc) [], hBar, configuration []]
    in
      vl


multiHistogram
  :: forall x c rs f
   . ( D.DataFieldOf rs x
     , Real (V.Snd x)
     , D.DataFieldOf rs c
--     , V.KnownField c
--     , F.ElemOf rs c
     , D.ToVLDataValue (F.ElField c)
--     , V.KnownField c
     , Ord (V.Snd c)
     , Foldable f
     )
  => Text -- ^ Title
  -> Maybe T.Text -- ^ label for counts
  -> Int -- ^ number of bins
  -> AxisBounds (V.Snd x)
  -> Bool -- ^ if true, add out of range counts to first/last bin
  -> MultiHistogramStyle
  -> ViewConfig
  -> f (D.Row rs)
  -> GV.VegaLite
multiHistogram title yLabelM nBins xBounds addOutOfRange mhStyle vc@(ViewConfig width _ _) rows
  = let
      yLabel = fromMaybe "count" yLabelM
      xLabel = D.colName @x
      allXF  = FL.premap (realToFrac . F.rgetField @x) (FL.vector @VB.Vector)
      mapByCF =
        let ff m r = M.insertWith (\xs ys -> xs ++ ys)
                                  (F.rgetField @c r)
                                  (pure $ realToFrac $ F.rgetField @x r)
                                  m -- FIX ++.  Ugh.
        in  FL.Fold ff M.empty (fmap VU.fromList)
      (vecAllX, mapByC) = FL.fold ((,) <$> allXF <*> mapByCF) rows
      (minM   , maxM  ) = case xBounds of
        GivenMinMax lo hi -> (Just lo, Just hi)
        _                 -> (Nothing, Nothing)
      minVal = fromMaybe (VB.minimum vecAllX) (fmap realToFrac minM)
      maxVal = fromMaybe (VB.maximum vecAllX) (fmap realToFrac maxM)
      bins   = H.binD minVal nBins maxVal
      makeRow k (bv, ct) = GV.dataRow
        [ (xLabel, GV.Number bv)
        , (yLabel, GV.Number ct)
        , D.toVLDataValue (V.Field @(V.Fst c) k)
        ]
        []
      makeRowsForOne (c, v) =
        let binned = makeHistogram addOutOfRange bins v
        in  List.concat $ VB.toList $ fmap (makeRow c) $ VB.convert binned
      dat = GV.dataFromRows [] $ List.concat $ fmap makeRowsForOne $ M.toList
        mapByC
      encY = GV.position
        GV.Y
        [ GV.PName yLabel
        , GV.PmType GV.Quantitative
        , GV.PAxis [GV.AxTitle yLabel]
        ]
      encC         = GV.color [D.mName @c, GV.MmType GV.Nominal]
      (hEnc, hBar) = case mhStyle of
        StackedBar ->
          let encX = GV.position GV.X [D.pName @x, GV.PmType GV.Quantitative]
              bandSize = (realToFrac width / realToFrac nBins) - 2
              hBar' = GV.mark GV.Bar [GV.MBinSpacing 1, GV.MSize bandSize]
          in  (encX . encY . encC, hBar')
        AdjacentBar ->
          let encX = GV.position
                GV.X
                [D.pName @c, GV.PmType GV.Nominal, GV.PAxis [GV.AxTitle ""]]
              encF  = GV.column [D.fName @x, GV.FmType GV.Quantitative]
              hBar' = GV.mark GV.Bar [GV.MBinSpacing 1]
          in  (encX . encY . encC . encF, hBar')
      configuration = GV.configure . viewConfigAsHvega vc
      vl            = GV.toVegaLite
        [GV.title title, dat, (GV.encoding . hEnc) [], hBar, configuration []]
    in
      vl


makeHistogram
  :: Bool -> H.BinD -> VU.Vector Double -> VU.Vector (H.BinValue H.BinD, Double)
makeHistogram addOutOfRange bins vecX =
  let histo =
          H.fillBuilder (H.mkSimple bins)
            $ ((VB.convert vecX) :: VB.Vector Double)
      hVec              = H.asVector histo
      minIndex          = 0
      maxIndex          = VU.length hVec - 1
      (minBV, minCount) = hVec VU.! minIndex
      (maxBV, maxCount) = hVec VU.! maxIndex
      newMin            = (minBV, minCount + (fromMaybe 0 $ H.underflows histo))
      newMax            = (maxBV, maxCount + (fromMaybe 0 $ H.overflows histo))
  in  if addOutOfRange
        then hVec VU.// [(minIndex, newMin), (maxIndex, newMax)]
        else hVec



-}
