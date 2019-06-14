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
module Frames.Visualization.VegaLite.Histogram
  ( singleHistogram
  , multiHistogram
  , MultiHistogramStyle(..)
  )
where

import qualified Frames.Visualization.VegaLite.Data
                                               as D

import           Graphics.Vega.VegaLite.Configuration
                                                ( ViewConfig(..)
                                                , viewConfigAsHvega
                                                , AxisBounds(..)
                                                )

import qualified Control.Foldl                 as FL
import qualified Data.List                     as List
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Vector                   as VB
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Frames                        as F
--import qualified Frames.Melt                   as F
import qualified Graphics.Vega.VegaLite        as GV

import qualified Data.Histogram                as H
import qualified Data.Histogram.Fill           as H

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



