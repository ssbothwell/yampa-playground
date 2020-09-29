{ mkDerivation, base, extra, random, sdl2, sdl2-ttf, stdenv, text
, vector, Yampa
}:
mkDerivation {
  pname = "yampa-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base extra random sdl2 sdl2-ttf text vector Yampa
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
