# Frames-hvega v0.1.0.0

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]

## Introduction
hvega is a usful wrapper around the grammar-of-graphics style vega-lite javascript
library.  It produces a script which can be embedded in an html page.  vega-lite scripts
can contain their own data and this package provides tools for populating that data
using vinyl/Frames records, tools for converting other data into vinyl/Frames
records and some useful tools for creating hvega visualizations using the strongly-
typed data in the viny/Frames records.

[travis]:        <https://travis-ci.org/adamConnerSax/Frames-hvega>
[travis-badge]:  <https://travis-ci.org/adamConnerSax/Frames-hvega.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/Frames-hvega>
[hackage-badge]: <https://img.shields.io/hackage/v/Frames-hvega.svg>
[hackage-deps-badge]: <https://img.shields.io/hackage-deps/v/Frames-hvega.svg>
[hackage-deps]: <http://packdeps.haskellers.com/feed?needle=Frames-hvega>
