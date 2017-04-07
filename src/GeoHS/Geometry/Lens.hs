{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module GeoHS.Geometry.Lens where

import GeoHS.Geometry.Types
import GeoHS.Geometry.TH
import Control.Lens
import Data.Tagged

makeSameNameFields ''Point
makeSameNameFields ''Bounds

class HasNorthWest o a | o -> a where northWest :: Lens' o a
class HasSouthEast o a | o -> a where southEast :: Lens' o a
class HasNorthEast o a | o -> a where northEast :: Lens' o a
class HasSouthWest o a | o -> a where southWest :: Lens' o a
class HasBounds o n | o -> n where bounds :: Lens' o n
class HasEpsg o n | o -> n where epsg :: Lens' o n
class HasWidth o n | o -> n where width :: Lens' o n
class HasHeight o n | o -> n where height :: Lens' o n


instance HasNorthEast Bounds LatLng where
  northEast = lens get_ set_ where
    get_ b    = LatLng (b^.north) (b^.east)
    set_ b ne = b & north .~ ne^.lat & east .~ ne^.lng
  {-# INLINE northEast #-}
instance HasNorthWest Bounds LatLng where
  northWest = lens get_ set_ where
    get_ b    = LatLng (b^.north) (b^.west)
    set_ b nw = b & north .~ nw^.lat & west .~ nw^.lng
  {-# INLINE northWest #-}
instance HasSouthEast Bounds LatLng where
  southEast = lens get_ set_ where
    get_ b    = LatLng (b^.south) (b^.east)
    set_ b se = b & south .~ se^.lat & east .~ se^.lng
  {-# INLINE southEast #-}
instance HasSouthWest Bounds LatLng where
  southWest = lens get_ set_ where
    get_ b    = LatLng (b^.south) (b^.west)
    set_ b sw = b & south .~ sw^.lat & west .~ sw^.lng
  {-# INLINE southWest #-}

-- Setter modifies 'east' coordinate (ie: keeps origin of geo coordinates)
instance HasWidth Bounds Double where
  width = lens get_ set_ where
    get_ b = b^.east - b^.west
    set_ b w =  b & east .~ (b^.west + w)   
  {-# INLINE width #-}

-- Setter modifies 'north' coordinate (keeps origin of geo coordinates)
instance HasHeight Bounds Double where
  height = lens get_ set_ where
    get_ b = b^.north - b^.south
    set_ b h =  b & north .~ (b^.south + h)   
  {-# INLINE height #-}

instance HasLat (Tagged crs LatLng) Double where
  lat = taggedLens lat
  {-# INLINE lat #-}
instance HasLng (Tagged crs LatLng) Double where
  lng = taggedLens lng
  {-# INLINE lng #-}

instance HasWidth (Tagged crs Bounds) Double where
  width = taggedLens width
  {-# INLINE width #-}
instance HasHeight (Tagged crs Bounds) Double where
  height = taggedLens height
  {-# INLINE height #-}

instance HasNorth (Tagged crs Bounds) Double where
  north = taggedLens north
  {-# INLINE north #-}
instance HasSouth (Tagged crs Bounds) Double where
  south = taggedLens south
  {-# INLINE south #-}
instance HasEast (Tagged crs Bounds) Double where 
  east = taggedLens east
  {-# INLINE east #-}
instance HasWest (Tagged crs Bounds) Double where
  west = taggedLens west
  {-# INLINE west #-}

instance HasNorthWest (Tagged crs Bounds) (Tagged crs LatLng) where
  northWest = taggedLens' northWest
  {-# INLINE northWest #-}
instance HasNorthEast (Tagged crs Bounds) (Tagged crs LatLng) where
  northEast = taggedLens' northEast
  {-# INLINE northEast #-}
instance HasSouthEast (Tagged crs Bounds) (Tagged crs LatLng) where
  southEast = taggedLens' southEast
  {-# INLINE southEast #-}
instance HasSouthWest (Tagged crs Bounds) (Tagged crs LatLng) where
  southWest = taggedLens' southWest
  {-# INLINE southWest #-}

taggedLens :: Lens' a b -> Lens' (Tagged c a) b
taggedLens lns = lens (view lns . unTagged) (flip (fmap . set lns))
{-# INLINE taggedLens #-}

taggedLens' :: Lens' a b -> Lens' (Tagged c a) (Tagged c b)
taggedLens' lns = lens (Tagged . view lns . unTagged) (flip (fmap . set lns . unTagged))
{-# INLINE taggedLens' #-}

type ToBounds a = (HasNorth a Double, HasSouth a Double, HasWest a Double, HasEast a Double)

toBounds :: ToBounds a => a -> Bounds
toBounds bb = Bounds (bb^.west) (bb^.south) (bb^.east) (bb^.north)
{-# INLINE toBounds #-}
