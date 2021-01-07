{-# LANGUAGE CPP                   #-}
module Graphics.Vega.VegaLite.Compat
  (
    title
  , BaseTimeUnitT
  )
where

import Data.Text (Text)
import qualified Graphics.Vega.VegaLite        as GV

#if MIN_VERSION_hvega(0,4,0)
title :: Text -> GV.PropertySpec
title x = GV.title x []
#else
title :: Text -> (GV.VLProperty, GV.VLSpec)
title = GV.title
#endif

#if MIN_VERSION_hvega(0,10,0)
type BaseTimeUnitT = GV.BaseTimeUnit
#else
type BaseTimeUnitT = GV.TimeUnit
#endif

