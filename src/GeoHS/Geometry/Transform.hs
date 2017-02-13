{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module GeoHS.Geometry.Transform (
  HasTransform(..)
, WGS84
, SphericalMercator
, wgs84
, sphericalMercator
) where

import SpatialReference
import Data.Tagged
import Data.Proxy

import GeoHS.Geometry.Types (LatLng, lngLat)
import GeoHS.Geometry.Lens
import Control.Lens

type WGS84 = Epsg 4326
type SphericalMercator = Epsg 3857

wgs84 :: Proxy WGS84
wgs84 = Proxy

sphericalMercator :: Proxy WGS84
sphericalMercator = Proxy

class HasTransform a crsFrom crsTo where
  transform :: Tagged crsFrom a -> Tagged crsTo a


instance HasTransform LatLng WGS84 SphericalMercator where
  transform (Tagged cs) = Tagged (lngLat lng1 lat1)
    where
      lng1 = cs^.lng * originShift / 180
      lat1 = (log (tan((90 + cs^.lat) * pi / 360 )) / (pi / 180)) * originShift / 180

instance HasTransform LatLng SphericalMercator WGS84 where
  transform (Tagged cs) = Tagged (lngLat lng1 lat1)
    where
      lng1  = (cs^.lng / originShift) * 180
      lat1' = (cs^.lat / originShift) * 180
      lat1  = 180 / pi * (2 * atan ( exp ( lat1' * pi / 180) ) - pi / 2)

originShift :: Double
originShift = 2 * pi * 6378137 / 2
