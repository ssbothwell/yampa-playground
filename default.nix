{ mkDerivation, base, simple-affine-space, stdenv }:
mkDerivation {
  pname = "yampa-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base simple-affine-space ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
