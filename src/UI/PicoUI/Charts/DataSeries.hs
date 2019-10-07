{-# LANGUAGE OverloadedStrings 
    , DuplicateRecordFields
    , RecordWildCards
    , OverloadedLists
    , RankNTypes
    , StandaloneDeriving
    , TypeSynonymInstances
    , FlexibleInstances
#-}

module UI.PicoUI.Charts.DataSeries where

import Data.Vector.Unboxed as U
import Data.Vector.Storable as S
import Data.Vector.Generic as G
import Data.Vector as V
import Data.IntMap.Strict as Map

import Color

-- Ok, need to think how to generalize all this.
-- Using Doubles as the most general type probably makes sense initially? If eventually we want to support Integral or
-- Ratio types for some reason, we can add on top.

-- Then, category names for "x" axis. In case we are not using x values as doubles, can simply use another vector with
-- category names that maps index i to the name? But then all data series being passed into our type will need to be
-- ordered and have values for ALL indices?

-- So, shall we have series for 1,2,3,4 dimensional data initially? (4-dim: bubble chart with x,y,bubble size, color)

-- One attempt to have one type to represent data series of all different kinds
-- How we *render* it (line, bar, bubble etc) - should be defined elsewhere!
-- Then we take GDS, add render type to it, stick them all into one chart, define
-- which axis each one relates to (maximum 2 per chart?) - and draw it.
-- 'a' should be Double, (Double,Double), (Double,Double,Double) etc for various dimensions of data

-- Now, to convert it to widget - we have to have a separate collider for each data point in case we want to handle
-- hover / click events on individual datapoints (as we might want to do and would be a good thing to do in general).
data GenericDataSeries a = GenericDataSeries {
    -- data points, x/y/z
    dataPoints :: U.Vector a, 
    -- names for data points - can be used as category names, or as individual point names in case of e.g. bubble charts
    pointNames :: V.Vector String, 
    -- a function that takes a point and creates a description for it, can be combined with point names
    pointNameFunction :: Maybe (a -> String),
    -- any additional comments for data points in case we want to display them somehow
    -- pointComments :: V.Vector String, 
    -- colors for points, if empty - use single color
    colors :: U.Vector Color,
    singleColor :: Color,
    -- function that calculates color based on values in case we want to get fancy
    -- if present, use this!
    colorFunction :: Maybe (a -> Color)
}

type DS1 = GenericDataSeries Double -- suitable for bar charts
type DS2 = GenericDataSeries (Double,Double) -- suitable for line, point charts
type DS3 = GenericDataSeries (Double,Double,Double) -- suitable for bubble charts or 3D

data SeriesDrawType = SDPoint | SDLine | SDBar | SDBubble | SDPie

-- unboxed data series, so mostly for math
data DataSeriesN a b = DataSeriesN {
    dataPoints :: U.Vector (a,b)
  , xName      :: String
  , yName      :: String
  -- minimum and maximum values for the axes
  , xmin, xmax :: a 
  , ymin, ymax :: b
} deriving Show
-- representing final screen coordinates
type ScreenDataSeries = DataSeriesN Int Int

data DataPoint a b = DataPoint {
    xvalue :: a,
    yvalue :: b
}

-- type DataSeriesRealFrac = forall a b. (Unbox a, Unbox b, RealFrac a, RealFrac b) => DataSeriesN a b

-- make n samples of the function f and turn it into a data series
sampleFunc :: Int -> Double -> Double -> (Double -> Double) -> DataSeriesN Double Double
sampleFunc n xmin xmax f = DataSeriesN {
    xName = "", yName = "",
    xmin = xmin, xmax = xmax,
    ymin = ymin' - 0.05 * abs ymin', ymax = ymax' + 0.05 * abs ymax',
    dataPoints = vec
} where step = (xmax - xmin) / fromIntegral n
        vec = G.generate (n+1) (\i -> let x = xmin + fromIntegral i * step in (x, f x))
        (_,ymin') = G.minimumBy (\(_,y1) (_,y2) -> if y1 < y2 then LT else GT) vec
        (_,ymax') = G.maximumBy (\(_,y1) (_,y2) -> if y1 < y2 then LT else GT) vec
           


-- convert data series into screen coordinates relative to 0,0 with xscreen max and yscreen max
ds2screen :: (Unbox a, Unbox b, RealFrac a, RealFrac b) => DataSeriesN a b -> Int -> Int -> ScreenDataSeries
ds2screen DataSeriesN{..} xscreen yscreen = 
    let xs = fromIntegral xscreen / (xmax - xmin)
        ys = fromIntegral yscreen / (ymax - ymin)
        newData = G.map (\(x,y) -> (round $ x * xs, yscreen - round (y * ys))) dataPoints
    in  DataSeriesN {
            dataPoints = newData,
            xName = "", yName = "",
            xmin = 0, xmax = xscreen,
            ymin = 0, ymax = yscreen
    }

    
