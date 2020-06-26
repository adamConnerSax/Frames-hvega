{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Frames.Visualization.VegaLite.LineVsTime
  (
--    ParameterEstimate(..)
--  , NamedParameterEstimate(..)
    multiLineVsTime
--  , parameterPlot
--  , parameterPlotMany
--  , parameterPlotFlex

    -- * Configuration Re-exports
  , ViewConfig(..)
  , AxisBounds(..)
  , TimeEncoding(..)

    -- * Re-exports
  , DateTime(..)
  )
where


import qualified Frames.Visualization.VegaLite.Data
                                               as D

import           Graphics.Vega.VegaLite.Configuration
                                                ( ViewConfig(..)
                                                , configuredVegaLite
                                                , AxisBounds(..)
                                                , TimeEncoding(..)
                                                )
import qualified Graphics.Vega.VegaLite.Compat as VegaCompat

import qualified Control.Foldl                 as FL
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( DateTime(..) )

import qualified Frames                        as F
import qualified Frames.Melt                   as F
import qualified Data.Vinyl.TypeLevel          as V



-- | A type to represent the details of a parameter estimate
--data ParameterEstimate = ParameterEstimate  { value :: Double, confidence :: (Double, Double) }
--data NamedParameterEstimate = NamedParameterEstimate { name :: Text, pEstimate :: ParameterEstimate }

-- | Plot parameters vs. time
multiLineVsTime
  :: forall g t a rs f
   . ( Foldable f
     , D.DataFieldOf rs g
     , D.DataFieldOf rs t
     , D.DataFieldOf rs a
     , F.ElemOf '[g, t, a] t
     , V.Snd g ~ T.Text
     , Real (V.Snd a)
     )
  => T.Text -- ^ Plot Title
  -> AxisBounds (V.Snd a)
  -> TimeEncoding (V.Snd t)
  -> ViewConfig
  -> f (D.Row rs)
  -> GV.VegaLite
multiLineVsTime title yBounds timeEnc vc rows
  = let
      groupNames =
        FL.fold FL.list $ FL.fold (FL.premap (F.rgetField @g) FL.set) rows
      yScale    = FL.fold (D.axisBounds @a yBounds) rows
      parseInfo = D.addParse @t (GV.FoDate $ timeFormat timeEnc) D.defaultParse
      dat       = D.recordsToVLData (F.rcast @'[g, t, a]) parseInfo rows
      xAxValues =
        GV.DateTimes $ fmap (toDateTime timeEnc . F.rgetField @t) $ FL.fold
          FL.list
          rows
      yEnc =
        GV.position GV.Y $ [D.pName @a, GV.PmType GV.Quantitative] ++ yScale
      xEnc = GV.position
        GV.X
        [ D.pName @t
        , GV.PmType GV.Temporal
        , GV.PTimeUnit $ GV.Utc $ timeUnit timeEnc
        , GV.PAxis [GV.AxValues xAxValues]
        ]
--      orderEnc = GV.order [D.oName @t, GV.OmType GV.Temporal]
      colorEnc = GV.color [D.mName @g, GV.MmType GV.Nominal]
      enc      = xEnc . yEnc . colorEnc -- . orderEnc
      filterCols name =
        GV.transform . GV.filter (GV.FEqual (D.colName @g) (GV.Str name))
      lSpec name = GV.asSpec
        [ (GV.encoding . enc) []
        , GV.mark GV.Line [GV.MInterpolate GV.Monotone]
        , filterCols name []
        ]
      mSpec gName = GV.asSpec
        [ (GV.encoding . enc) []
        , GV.mark GV.Point [GV.MFilled True]
        , filterCols gName []
        ]
      specs = concat $ fmap (\x -> [lSpec x, mSpec x]) groupNames
    in
      configuredVegaLite vc [VegaCompat.title title, GV.layer specs, dat]

