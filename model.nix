{ mkDerivation, base, containers, deepseq, fetchgit, ghc-prim
, pretty, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
, transformers
}:
mkDerivation {
  pname = "model";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/tittoassini/model";
    sha256 = "1g1jfv7dsacfvgx7x1izpyd2ig8l7ahs9r9ck6b1amhr1ddf6vgd";
    rev = "c2601f0f0aa87ac7356711fd67c3941824dcac0c";
  };
  libraryHaskellDepends = [
    base containers deepseq pretty text transformers
  ];
  testHaskellDepends = [
    base ghc-prim tasty tasty-hunit tasty-quickcheck text
  ];
  homepage = "http://github.com/tittoassini/model";
  description = "Derive a model of a data type using Generics";
  license = stdenv.lib.licenses.bsd3;
}
