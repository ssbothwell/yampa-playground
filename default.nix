{ mkDerivation, base, extra, lens, random, sdl2, sdl2-ttf, stdenv
, vector, Yampa
}:
mkDerivation {
  pname = "yampa-playground";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base extra lens random sdl2 sdl2-ttf vector Yampa
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
