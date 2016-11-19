{ mkDerivation, base, binary, bytestring, containers, derive, dlist
, fetchgit, ghc-prim, pretty, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, vector
}:
mkDerivation {
  pname = "flat";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/tittoassini/flat";
    sha256 = "0k66g1l4bb75bzs5snwc6ml4xzydb52mwd78jyz5wfx8jbxd1kmv";
    rev = "9531bec124b25105a0c0767fc010fefb87a6f241";
  };
  libraryHaskellDepends = [
    base binary bytestring containers dlist pretty text vector
  ];
  testHaskellDepends = [
    base bytestring derive ghc-prim tasty tasty-hunit tasty-quickcheck
    text
  ];
  homepage = "http://github.com/tittoassini/flat";
  description = "Principled and efficient binary serialization";
  license = stdenv.lib.licenses.mit;
}
