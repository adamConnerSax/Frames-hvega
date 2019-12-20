{-# LANGUAGE OverloadedStrings #-}
module Graphics.Vega.VegaLite.Geography where

import           Data.Text                      ( Text )

data VegaGeoSource = GeoJSON Text | TopoJSON Text


usAtlasCDN :: Text
usAtlasCDN = "https://cdn.jsdelivr.net/npm/us-atlas@3/"

usStates :: VegaGeoSource
usStates = TopoJSON $ usAtlasCDN <> "states-10m.json"

usStatesAlbers :: VegaGeoSource
usStatesAlbers = TopoJSON $ usAtlasCDN <> "states-albers-10m.json"

usCounties :: VegaGeoSource
usCounties = TopoJSON $ usAtlasCDN <> "counties-10m.json"

usCountiesAlbers :: VegaGeoSource
usCountiesAlbers = TopoJSON $ usAtlasCDN <> "counties-albers-10m.json"

loganPowellUrl :: Text
loganPowellUrl =
  "https://github.com/loganpowell/census-geojson/tree/master/GeoJSON/"

usCongressionalDistricts :: VegaGeoSource
usCongressionalDistricts =
  GeoJSON $ loganPowellUrl <> "500k/2018/congressional-district.json"



