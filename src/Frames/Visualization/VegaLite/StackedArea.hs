{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
module Frames.Visualization.VegaLite.StackedArea
  (
    -- * Visualizations
    stackedAreaVsTime
    -- * Configuration Re-exports
  , ViewConfig(..)
    -- * Hvega Re-exports
  , TimeEncoding(..)
  , TimeUnit(..)
  )
where

import qualified Frames.Visualization.VegaLite.Data
                                               as D

import           Graphics.Vega.VegaLite.Configuration
                                                ( ViewConfig(..)
                                                , configuredVegaLite
                                                , TimeEncoding(..)
                                                , AxisBounds(..)
                                                )
import qualified Graphics.Vega.VegaLite.Compat as VegaCompat

import qualified Control.Foldl                 as FL
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( TimeUnit(..) )
import qualified Frames                        as F
import qualified Frames.Melt                   as F
import qualified Data.Vinyl.TypeLevel          as V

stackedAreaVsTime
  :: forall g t a rs f
   . ( Foldable f
     , D.DataFieldOf rs g
     , D.DataFieldOf rs t
     , D.DataFieldOf rs a
     , F.ElemOf '[g, t, a] t -- required for parsing
     , Real (V.Snd a)
     )
  => T.Text -- ^ Title
  -> AxisBounds (V.Snd a)
  -> TimeEncoding (V.Snd t)
  -> ViewConfig -- sizing information
  -> f (D.Row rs)
  -> GV.VegaLite
stackedAreaVsTime title yBounds timeEnc vc vRows
  = let
      parseInfo = D.addParse @t (GV.FoDate $ timeFormat timeEnc) D.defaultParse
      yScale    = FL.fold (D.axisBounds @a yBounds) vRows
--      xScale    = FL.fold (D.axisBounds @t DataMinMax) vRows
      dat       = D.recordsToVLData (F.rcast @'[g, t, a]) parseInfo vRows
      xAxValues =
        GV.DateTimes $ fmap (toDateTime timeEnc . F.rgetField @t) $ FL.fold
          FL.list
          vRows
      xEnc =
        GV.position GV.X
          $ [ D.pName @t
            , GV.PmType GV.Temporal
            , GV.PTimeUnit $ GV.Utc $ timeUnit timeEnc
            , GV.PAxis [GV.AxValues xAxValues]
            ]
      yEnc =
        GV.position GV.Y $ [D.pName @a, GV.PmType GV.Quantitative] ++ yScale
      colorEnc = GV.color [D.mName @g, GV.MmType GV.Nominal]
      enc      = xEnc . yEnc . colorEnc
      specs =
        [ GV.asSpec
            [ (GV.encoding . enc) []
            , GV.mark GV.Area [GV.MInterpolate GV.Monotone]
            ]
        ]
    in
      configuredVegaLite vc [VegaCompat.title title, GV.layer specs, dat]
