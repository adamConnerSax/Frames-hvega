{-# LANGUAGE CPP                   #-}
module Graphics.Vega.VegaLite.Compat
  (
    title
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
