{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Graphics.Vega.VegaLite.Configuration
  (
    -- * Helper Types    
    TimeEncoding(..)
  , AxisBounds(..)
  , ViewConfig(..)
    -- * helpers
  , intYear
--  , timeField
  , configuredVegaLite
  , viewConfigAsHvega
  , viewConfigAsTopLevel
    -- * Re-export
  )
where

import qualified Graphics.Vega.VegaLite        as GV

import           Data.Text                      ( Text )

data AxisBounds a where
  Default ::AxisBounds a
  DataMinMax ::AxisBounds a
  GivenMinMax ::a -> a -> AxisBounds a deriving (Eq, Show)

data ViewConfig = ViewConfig { vcWidth :: Double, vcHeight :: Double, vcPadding :: Double }

viewConfigAsHvega :: ViewConfig -> GV.BuildLabelledSpecs
viewConfigAsHvega (ViewConfig w h p) =
  GV.configuration (GV.View [GV.ViewWidth w, GV.ViewHeight h])
    . GV.configuration (GV.Padding $ GV.PSize p)

viewConfigAsTopLevel :: ViewConfig -> [(GV.VLProperty, GV.VLSpec)]
viewConfigAsTopLevel (ViewConfig w h p) =
  [ GV.width w
  , GV.height h
  , GV.padding (GV.PSize p)
  , GV.autosize [GV.AFit, GV.APadding, GV.AResize]
  ]

configuredVegaLite :: ViewConfig -> [(GV.VLProperty, GV.VLSpec)] -> GV.VegaLite
configuredVegaLite vc xs = GV.toVegaLite $ viewConfigAsTopLevel vc <> xs

data TimeEncoding a = TimeEncoding { timeFormat :: Text, timeUnit :: GV.TimeUnit, toDateTime:: a -> [GV.DateTime] }

-- helpers for time encoding
intYear :: TimeEncoding Int
intYear = TimeEncoding "%Y" GV.Year (\n -> [GV.DTYear n])

{-
timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStrF _ _) x = GV.Str $ toStrF x
-}
