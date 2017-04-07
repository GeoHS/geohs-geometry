{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module GeoHS.Geometry.Types (
  Point(Point)
, LatLng
, pattern LngLat
, pattern LatLng
, Bounds(Bounds)
, latLng
, lngLat
, emptyBounds
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup
import Data.Proxy
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import SpatialReference
import Data.Swagger
import Web.HttpApiData (FromHttpApiData(..))


type LatLng = Point

-- | Geographic coordinates
data Point = Point
  { lng :: {-# UNPACK #-} !Double
  , lat :: {-# UNPACK #-} !Double
  }
  deriving (Show, Ord, Eq, Generic, ToJSON, FromJSON, ToSchema)

pattern LngLat :: Double -> Double -> LatLng
pattern LngLat lng lat = Point lng lat

pattern LatLng :: Double -> Double -> LatLng
pattern LatLng lat lng = Point lng lat


latLng, lngLat :: Double -> Double -> LatLng

latLng = LatLng
{-# INLINE latLng #-}

lngLat = LngLat
{-# INLINE lngLat #-}

-- | A Bounding box in the usual Xmin,Ymin,Xmax,Ymax coordinate order
data Bounds = Bounds
  { west  :: {-# UNPACK #-} !Double
  , south :: {-# UNPACK #-} !Double
  , east  :: {-# UNPACK #-} !Double
  , north :: {-# UNPACK #-} !Double
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

emptyBounds :: Bounds
emptyBounds = Bounds 0 0 0 0
{-# INLINE emptyBounds #-}


instance Semigroup Bounds where
   Bounds w1 s1 e1 n1 <> Bounds w2 s2 e2 n2 = Bounds
    { south = min s1 s2
    , west  = min w1 w2
    , north = min n1 n2
    , east  = min e1 e2
    }

instance FromHttpApiData Bounds where
  parseUrlPiece x = case map (read . T.unpack) $ T.splitOn "," x of --FIXME
    [w,s,e,n] -> Right (Bounds w s e n)
    _ -> Left "Invalid format for BBOX"
  parseHeader x = case map (read . BS.unpack) $ BS.split ',' x of --FIXME
    [w,s,e,n] -> Right (Bounds w s e n)
    _ -> Left "Invalid format for BBOX"

instance ToParamSchema Bounds where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
    -- & pattern ?~ "((\d*(\.\d+)),){3}(\d*(\.\d+))" --FIXME
