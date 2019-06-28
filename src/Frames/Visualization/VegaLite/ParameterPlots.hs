{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Frames.Visualization.VegaLite.ParameterPlots
  (
    -- * Types for named parameters
    ParameterEstimate(..)
  , NamedParameterEstimate(..)
    -- * Plots
  , parameterPlot
  , parameterPlotMany
  , parameterPlotFlex

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
                                                , viewConfigAsHvega
                                                , AxisBounds(..)
                                                , TimeEncoding(..)
                                                )


import qualified Control.Foldl                 as FL
import           Control.Monad                  ( join )
import qualified Data.Array                    as A
import           Data.Functor.Identity          ( Identity(Identity) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Graphics.Vega.VegaLite        as GV
import           Graphics.Vega.VegaLite         ( DateTime(..) )
import qualified Data.List                     as List
import           Text.Printf                    ( printf )

import qualified Frames                        as F
import qualified Frames.Melt                   as F
import qualified Data.Vinyl.TypeLevel          as V
import qualified Data.Vinyl                    as V

import qualified Frames.Transform              as FT

import qualified Statistics.Types              as S

-- | A type to represent the details of a parameter estimate
data ParameterEstimate = ParameterEstimate  { value :: Double, confidence :: (Double, Double) }
data NamedParameterEstimate = NamedParameterEstimate { name :: Text, pEstimate :: ParameterEstimate }

type Name = "Name" F.:-> T.Text
type Estimate = "Estimate" F.:-> Double
type Lo = "Lo" F.:-> Double
type Hi = "Hi" F.:-> Double


unpackParameterEstimate
  :: forall pe rs
   . (F.ElemOf rs pe, V.KnownField pe, V.Snd pe ~ NamedParameterEstimate)
  => F.Record rs
  -> F.Record (rs V.++ '[Name, Estimate, Lo, Hi])
unpackParameterEstimate row = FT.mutate (f . F.rgetField @pe) row
 where
  f :: NamedParameterEstimate -> F.Record '[Name, Estimate, Lo, Hi]
  f (NamedParameterEstimate n (ParameterEstimate v (lo, hi))) =
    n F.&: v F.&: lo F.&: hi F.&: V.RNil


-- | Plot parameters with error bars
--  Flex version handles a foldable of results so we can, e.g., 
--  1. Compare across time or other variable in the data
--  2. Compare different fitting methods for the same data

-- TODO: Fix replicated y-axis ticks.  There since we need the difference between them (trailing "'"s) to provide y-offset
-- to the different results.  Otherwise they would overlap in y.  Maybe fix by switching to layers and only having Y-axis labels
-- on 0th?

type NullText = "NullText" F.:-> T.Text

parameterPlot
  :: forall pe rs f
   . ( Foldable f
     , Functor f
     , V.KnownField pe
     , V.KnownField NullText
     , F.ElemOf (rs V.++ '[NullText]) pe
     , F.ElemOf (rs V.++ '[NullText]) NullText
     , F.ElemOf (rs V.++ '[NullText] V.++ '[Name, Estimate, Lo, Hi]) NullText
     , F.ElemOf (rs V.++ '[NullText] V.++ '[Name, Estimate, Lo, Hi]) Estimate
     , F.ElemOf (rs V.++ '[NullText] V.++ '[Name, Estimate, Lo, Hi]) Lo
     , F.ElemOf (rs V.++ '[NullText] V.++ '[Name, Estimate, Lo, Hi]) Hi
     , F.ElemOf (rs V.++ '[NullText] V.++ '[Name, Estimate, Lo, Hi]) Name
     , V.Snd pe ~ NamedParameterEstimate
     )
  => T.Text
  -> S.CL Double
  -> ViewConfig
  -> f (D.Row rs)
  -> GV.VegaLite
parameterPlot title cl vc rows = parameterPlotFlex @NullText @pe
  False
  title
  cl
  vc
  (labeledRows rows)
 where
  nullText = ("" F.&: V.RNil) :: F.Record '[NullText]
  labeledRows x = fmap (FT.mutate (const nullText)) x

parameterPlotMany
  :: forall l pe rs f
   . ( Foldable f
     , Functor f
     , D.DataFieldOf rs l
     , F.ElemOf rs pe
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) l
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Estimate
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Lo
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Hi
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Name
     , V.KnownField pe
     , V.Snd l ~ T.Text
     , V.Snd pe ~ NamedParameterEstimate
     )
  => T.Text
  -> S.CL Double
  -> ViewConfig
  -> f (D.Row rs)
  -> GV.VegaLite
parameterPlotMany = parameterPlotFlex @l @pe True

parameterPlotFlex
  :: forall l pe rs f
   . ( Foldable f
     , Functor f
     , D.DataFieldOf rs l
     , F.ElemOf rs pe
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) l
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Estimate
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Lo
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Hi
     , F.ElemOf (rs V.++ '[Name, Estimate, Lo, Hi]) Name
     , V.KnownField pe
     , V.Snd l ~ T.Text
     , V.Snd pe ~ NamedParameterEstimate
     )
  => Bool -- ^ legend?
  -> T.Text -- ^ title 
  -> S.CL Double -- ^ confidence level
  -> ViewConfig -- ^ size, etc.
  -> f (D.Row rs) -- ^ data
  -> GV.VegaLite
parameterPlotFlex haveLegend title cl vc rows
  = let
      unpackedRows = fmap (unpackParameterEstimate @pe @rs) rows
      dat          = D.recordsToVLData (F.rcast @'[Name, l, Estimate])
                                       D.defaultParse
                                       unpackedRows
      xLabel =
        "Estimate (with "
          <> (T.pack $ printf "%2.0f" (100 * S.confidenceLevel cl))
          <> "% confidence error bars)"
      estimateXEnc = GV.position
        GV.X
        [ D.pName @Estimate
        , GV.PmType GV.Quantitative
        , GV.PAxis [GV.AxTitle xLabel]
        ]
      estimateYEnc = GV.position GV.Y [D.pName @Name, GV.PmType GV.Ordinal]
      handleLegend l = if haveLegend then l else (GV.MLegend []) : l
      estimateColorEnc =
        GV.color $ handleLegend $ [D.mName @l, GV.MmType GV.Nominal]
      estimateEnc  = estimateXEnc . estimateYEnc . estimateColorEnc
      estConfLoEnc = GV.position
        GV.X
        [D.pName @Lo, GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
      estConfHiEnc = GV.position
        GV.X2
        [D.pName @Hi, GV.PmType GV.Quantitative, GV.PAxis [GV.AxTitle ""]]
      estConfEnc =
        estConfLoEnc . estConfHiEnc . estimateYEnc . estimateColorEnc
      estSpec = GV.asSpec [(GV.encoding . estimateEnc) [], GV.mark GV.Point []]
      confSpec = GV.asSpec [(GV.encoding . estConfEnc) [], GV.mark GV.Rule []]
      configuration = GV.configure . viewConfigAsHvega vc
      vl = GV.toVegaLite
        [GV.title title, GV.layer [estSpec, confSpec], dat, configuration []]
    in
      vl


