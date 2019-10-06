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
import Data.IntMap.Strict as Map

-- unboxed data series, so mostly for math
data DataSeriesN a b = DataSeriesN {
    dataPoints :: U.Vector (a,b)
  , xName      :: String
  , yName      :: String
  , xmin, xmax :: a -- minimum and maximum values for the axes
  , ymin, ymax :: b
} deriving Show
-- representing final screen coordinates
type ScreenDataSeries = DataSeriesN Int Int

-- type DataSeriesRealFrac = forall a b. (Unbox a, Unbox b, RealFrac a, RealFrac b) => DataSeriesN a b

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
-- maximumBy :: (a -> a -> Ordering) -> Vector a -> a            



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

    
