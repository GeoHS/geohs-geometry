{-# LANGUAGE ViewPatterns #-}
module GeoHS.Geometry.TH ( sameNameFields, sameNameNamer) where

import Control.Lens
import Data.Char (toUpper)
import Language.Haskell.TH

sameNameFields :: LensRules
sameNameFields = defaultFieldRules
  & lensField .~ sameNameNamer

-- | A 'FieldNamer' for 'sameNameFields'.
sameNameNamer :: FieldNamer
sameNameNamer _ _ (nameBase -> field) = [ MethodName (mkName cls) (mkName field) ]
  where
    cls = "Has" ++ capitalize field
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
