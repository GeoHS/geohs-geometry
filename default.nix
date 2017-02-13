{ mkDerivation, aeson, base, bytestring, http-api-data
, insert-ordered-containers, lens, spatial-reference, stdenv
, swagger2, text
}:
mkDerivation {
  pname = "geohs-geometry";
  version = "0.1.0.0";
  src = ./.;
  hyperlinkSource = false;
  libraryHaskellDepends = [
    aeson base bytestring http-api-data insert-ordered-containers lens
    spatial-reference swagger2 text
  ];
  homepage = "https://github.com/GeoHS/geohs-geometry";
  description = "GeoHS Geometry types";
  license = stdenv.lib.licenses.bsd3;
}
