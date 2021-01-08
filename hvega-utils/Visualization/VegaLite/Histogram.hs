{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Visualization.VegaLite.Histogram
  ( singleHistogram,
    multiHistogram,
    MultiHistogramStyle (..),
  )
where

import qualified Control.Foldl as FL

import qualified Data.Histogram as H
import qualified Data.Histogram.Fill as H
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import qualified Graphics.Vega.VegaLite as GV
import qualified Graphics.Vega.VegaLite.Compat as VegaCompat
import Graphics.Vega.VegaLite.Configuration
  ( AxisBounds (..),
    ViewConfig (..),
    configuredVegaLite,
  )

-- | Histograms
-- | Single, stacked, side-by-side, and faceted
data MultiHistogramStyle = StackedBar | FacetedBar | AdjacentBar

singleHistogram :: (Foldable f, Real a)
  => Text -- | Title
  -> Maybe Text -- | optional label for x-axis
  -> Maybe Text -- | optional label for "Count" on y-axis
  -> (row -> a) -- get value to count
  -> Int -- | number of bins
  -> AxisBounds a   -- | bounds for x in histogram calculation
  -> Bool   -- | if true, add out of range values to first/last count
  -> ViewConfig
  -> f row
  -> GV.VegaLite
singleHistogram title xLabelM yLabelM getVal nBins xBounds addOutOfRange vc@(ViewConfig width _ _) rows =
  let yLabel = fromMaybe "count" yLabelM
      xLabel = fromMaybe "" xLabelM
      bandSize = (width / realToFrac nBins) - 10.0
      vecX =
        FL.fold
          (FL.premap (realToFrac . getVal) (FL.vector @VU.Vector))
          rows
      (minM, maxM) = case xBounds of
        GivenMinMax lo hi -> (Just lo, Just hi)
        _ -> (Nothing, Nothing)
      minVal = fromMaybe (VU.minimum vecX) (fmap realToFrac minM)
      maxVal = fromMaybe (VU.maximum vecX) (fmap realToFrac maxM)
      bins = H.binD minVal nBins maxVal
      hVec = makeHistogram addOutOfRange bins vecX
      toVLRow (bv, ct) =
        GV.dataRow [(xLabel, GV.Number bv), (yLabel, GV.Number ct)] []
      dat =
        GV.dataFromRows [] $
          List.concat $
            VB.toList $
              fmap toVLRow $
                VB.convert
                  hVec
      encX = GV.position GV.X [GV.PName xLabel, GV.PmType GV.Quantitative]
      encY = GV.position GV.Y [GV.PName yLabel, GV.PmType GV.Quantitative]
      hBar = GV.mark GV.Bar [GV.MBinSpacing 1, GV.MSize bandSize]
      hEnc = encX . encY
      vl =
        configuredVegaLite vc $
          [VegaCompat.title title, dat, (GV.encoding . hEnc) [], hBar]
   in vl


multiHistogram ::
  ( Real a
  , Ord c
  , Foldable f
  )
  => Text -- | Title
  -> Maybe Text -- | optional label for x-axes
  -> Maybe Text -- | optional label for counts
  -> Maybe Text -- | optional label for categories
  -> (row -> a) -- | get value to count
  -> (row -> c) -- | get category for multi
  -> (c -> GV.DataValue)
  -> Int   -- | number of bins
  -> AxisBounds a
  -> Bool -- | if true, add out of range counts to first/last bin
  -> MultiHistogramStyle
  -> ViewConfig
  -> f row
  -> GV.VegaLite
multiHistogram title xLabelM yLabelM catLabelM  getVal getCat dvCat nBins xBounds addOutOfRange mhStyle vc@(ViewConfig width _ _) rows =
  let yLabel = fromMaybe "count" yLabelM
      xLabel = fromMaybe "X" xLabelM
      catLabel = fromMaybe "Category" catLabelM
      allXF = FL.premap (realToFrac . getVal) (FL.vector @VB.Vector)
      mapByCF =
        let ff m r =
              M.insertWith
                (\xs ys -> xs ++ ys)
                (getCat r)
                (pure $ realToFrac $ getVal r)
                m -- FIX ++.  Ugh.
         in FL.Fold ff M.empty (fmap VU.fromList)
      (vecAllX, mapByC) = FL.fold ((,) <$> allXF <*> mapByCF) rows
      (minM, maxM) = case xBounds of
        GivenMinMax lo hi -> (Just lo, Just hi)
        _ -> (Nothing, Nothing)
      minVal = fromMaybe (VB.minimum vecAllX) (fmap realToFrac minM)
      maxVal = fromMaybe (VB.maximum vecAllX) (fmap realToFrac maxM)
      bins = H.binD minVal nBins maxVal
      makeRow k (bv, ct) =
        GV.dataRow
          [ (xLabel, GV.Number bv),
            (yLabel, GV.Number ct),
            (catLabel, dvCat k)
          ]
          []
      makeRowsForOne (c, v) =
        let binned = makeHistogram addOutOfRange bins v
         in List.concat $ VB.toList $ fmap (makeRow c) $ VB.convert binned
      dat =
        GV.dataFromRows [] $
          List.concat $
            fmap makeRowsForOne $
              M.toList
                mapByC
      encY =
        GV.position
          GV.Y
          [ GV.PName yLabel,
            GV.PmType GV.Quantitative,
            GV.PAxis [GV.AxTitle yLabel]
          ]
      encC = GV.color [GV.MName catLabel, GV.MmType GV.Nominal]
      (hEnc, hBar) = case mhStyle of
        StackedBar ->
          let encX = GV.position GV.X [GV.PName xLabel, GV.PmType GV.Quantitative]
              bandSize = (realToFrac width / realToFrac nBins) - 2
              hBar' = GV.mark GV.Bar [GV.MBinSpacing 1, GV.MSize bandSize]
           in (encX . encY . encC, hBar')
        FacetedBar ->
          let encX =
                GV.position
                  GV.X
                  [GV.PName xLabel, GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
              encF = GV.column [GV.FName catLabel, GV.FmType GV.Nominal]
              hBar' = GV.mark GV.Bar [GV.MBinSpacing 1]
           in (encX . encY . encC . encF, hBar')
        AdjacentBar ->
          let encX =
                GV.position
                  GV.X
                  [GV.PName catLabel, GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
              encF = GV.column [GV.FName xLabel, GV.FmType GV.Ordinal]
              hBar' = GV.mark GV.Bar [GV.MBinSpacing 1]
           in (encX . encY . encC . encF, hBar')
      --      configuration = GV.configure . viewConfigAsHvega vc
      vl =
        configuredVegaLite
          vc
          [VegaCompat.title title, dat, (GV.encoding . hEnc) [], hBar]
   in vl


makeHistogram ::
  Bool -> H.BinD -> VU.Vector Double -> VU.Vector (H.BinValue H.BinD, Double)
makeHistogram addOutOfRange bins vecX =
  let histo =
        H.fillBuilder (H.mkSimple bins) $
          ((VB.convert vecX) :: VB.Vector Double)
      hVec = H.asVector histo
      minIndex = 0
      maxIndex = VU.length hVec - 1
      (minBV, minCount) = hVec VU.! minIndex
      (maxBV, maxCount) = hVec VU.! maxIndex
      newMin = (minBV, minCount + (fromMaybe 0 $ H.underflows histo))
      newMax = (maxBV, maxCount + (fromMaybe 0 $ H.overflows histo))
   in if addOutOfRange
        then hVec VU.// [(minIndex, newMin), (maxIndex, newMax)]
        else hVec
