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
, earthRadius
, degreeToMeterResolution
, mercatorToWGS84
, wgs84ToMercator
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
  transform (Tagged cs) = Tagged (wgs84ToMercator cs)

wgs84ToMercator :: LatLng -> LatLng
wgs84ToMercator cs = lngLat lng1 lat1
  where
    lng1 = cs^.lng * originShift / 180
    lat1 = (log (tan((90 + cs^.lat) * pi / 360 )) / (pi / 180)) * originShift / 180

instance HasTransform LatLng SphericalMercator WGS84 where
  transform (Tagged cs) = Tagged (mercatorToWGS84 cs)

mercatorToWGS84 :: LatLng -> LatLng
mercatorToWGS84 cs = lngLat lng1 lat1
  where
    lng1  = (cs^.lng / originShift) * 180
    lat1' = (cs^.lat / originShift) * 180
    lat1  = 180 / pi * (2 * atan ( exp ( lat1' * pi / 180) ) - pi / 2)

originShift :: Floating a => a
originShift = pi * earthRadius

earthRadius :: Num a => a
earthRadius = 6378137


degreeToMeterResolution
  :: Floating a
  => a -> a -> a
degreeToMeterResolution latitude res
  = res * earthRadius * (pi/180) * cos (latitude * pi / 180)
{-# INLINE degreeToMeterResolution #-}
