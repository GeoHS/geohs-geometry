{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module GeoHS.Geometry where

import Data.Aeson (FromJSON, ToJSON)
import Data.Semigroup
import Data.Proxy
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import           Data.Swagger   (ToSchema(..))
import qualified Data.HashMap.Strict.InsOrd as HM
import GHC.Generics (Generic)
import Control.Lens
import Control.Lens.TH (makeFields, makeLensesFor)
import SpatialReference
import Data.Swagger
import Web.HttpApiData (FromHttpApiData(..))

class HasNorth o n | o -> n where north :: Lens' o n
class HasEast o n | o -> n where east :: Lens' o n
class HasSouth o n | o -> n where south :: Lens' o n
class HasWest o n | o -> n where west :: Lens' o n
class HasBounds o n | o -> n where bounds :: Lens' o n
class HasEpsg o n | o -> n where epsg :: Lens' o n
class HasWidth o n | o -> n where width :: Lens' o n
class HasHeight o n | o -> n where height :: Lens' o n


data Coords crs = Coords { coordsLng:: !Double, coordsLat:: !Double}
  deriving (Eq, Ord, Show)
makeFields ''Coords


data LatLng = LatLng { latLngLat :: !Double , latLngLng :: !Double }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
makeFields ''LatLng

data Bounds = Bounds
  { boundsSouthWest :: !LatLng
  , boundsNorthEast :: !LatLng
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
makeFields ''Bounds


instance FromHttpApiData Bounds where
  parseUrlPiece x = case map (read . T.unpack) $ T.splitOn "," x of --FIXME
    [w,s,e,n] -> Right Bounds
      { boundsNorthEast = LatLng n e
      , boundsSouthWest = LatLng s w
      }
    _ -> Left "Invalid format for BBOX"
  parseHeader x = case map (read . BS.unpack) $ BS.split ',' x of --FIXME
    [w,s,e,n] -> Right $ Bounds
      { boundsNorthEast = LatLng n e
      , boundsSouthWest = LatLng s w
      }
    _ -> Left "Invalid format for BBOX"

instance ToParamSchema Bounds where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
    -- & pattern ?~ "((\d*(\.\d+)),){3}(\d*(\.\d+))" --FIXME



instance Semigroup Bounds where
   a <> b = Bounds
    { boundsSouthWest = LatLng (min (a^.south) (b^.south )) (min (a^.west ) (b^.west))
    , boundsNorthEast = LatLng (min (a^.north) (b^.north )) (min (a^.east ) (b^.east))
    }

class HasNorthWest o a | o -> a where
  northWest :: Lens' o a
class HasSouthEast o a | o -> a where
  southEast :: Lens' o a

instance HasNorthWest Bounds LatLng where
  northWest = lens get_ set_ where
    get_ b    = LatLng (b^.northEast.lat) (b^.southWest.lng)
    set_ b nw = b & northEast.lat .~  nw^.lat
                  & southWest.lng .~  nw^.lng
             

instance HasNorth Bounds Double where north = northEast.lat
instance HasSouth Bounds Double where south = southWest.lat
instance HasWest Bounds Double where west = southWest.lng
instance HasEast Bounds Double where east = northEast.lng

instance HasSouthEast Bounds LatLng where
  southEast = lens get_ set_ where
    get_ b    = LatLng (b^.southWest.lat) (b^.northEast.lng)
    set_ b se = b & southWest.lat .~  se^.lat
                  & northEast.lng .~  se^.lng
