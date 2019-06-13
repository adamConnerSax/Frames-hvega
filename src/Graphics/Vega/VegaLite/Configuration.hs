{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
module Graphics.Vega.VegaLite.Configuration
  (
    -- * Helper Types    
    TimeEncoding(..)
  , Scaling(..)
  , ViewConfig(..)
    -- * helpers
  , intYear
--  , timeField
  , viewConfigAsHvega
    -- * Re-export
  )
where

import qualified Graphics.Vega.VegaLite        as GV

import           Control.Arrow                  ( second )
import qualified Control.Foldl                 as FL
import qualified Data.Array                    as A
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time                     as Time

--import           Graphics.Vega.VegaLite         ( TimeUnit(..) )

data Scaling = Default | DataMinMax deriving (Eq)

data ViewConfig = ViewConfig { vcWidth :: Double, vcHeight :: Double, vcPadding :: Double }

viewConfigAsHvega :: ViewConfig -> GV.BuildLabelledSpecs
viewConfigAsHvega (ViewConfig w h p) =
  GV.configuration (GV.View [GV.ViewWidth w, GV.ViewHeight h])
    . GV.configuration (GV.Padding $ GV.PSize p)

data TimeEncoding = TimeEncoding { timeFormat :: Text, timeUnit :: GV.TimeUnit }

-- helpers for time encoding
intYear :: TimeEncoding
intYear = TimeEncoding "%Y" GV.Year

{-
timeField :: TimeEncoding a -> a -> GV.DataValue
timeField (TimeEncoding toStrF _ _) x = GV.Str $ toStrF x
-}
