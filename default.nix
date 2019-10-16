{ mkDerivation, async, base, bytestring, cassava, containers
, data-fix, directory, dirtree, filepath, free, hnix, hpack, lens
, mtl, optparse-applicative, prettyprinter, stdenv, stm, text, time
, transformers, typed-process, vector
}:
mkDerivation {
  pname = "nixec";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring cassava containers data-fix directory dirtree
    filepath free hnix lens mtl optparse-applicative prettyprinter stm
    text time transformers typed-process vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    async base bytestring cassava containers data-fix directory dirtree
    filepath free hnix lens mtl optparse-applicative prettyprinter stm
    text time transformers typed-process vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/kalhauge/nixec#readme";
  description = "A system for specifying evaluation scripts";
  license = stdenv.lib.licenses.bsd3;
}
