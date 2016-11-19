{ mkDerivation, base, bytestring, containers, cryptonite, deepseq
, directory, fetchgit, filepath, flat, ListLike, memory, model, mtl
, pretty, stdenv, tasty, tasty-hunit, tasty-quickcheck
, template-haskell, text, transformers, aeson
}:
mkDerivation {
  pname = "typed";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/tittoassini/typed";
    sha256 = "1xijdchbs0x2cqn4h0d8gy1p279yv8781jffx3rdcb2a2grqw0mx";
    rev = "60803aa94de66c9bfcc51192d6c747237af193e0";
  };
  libraryHaskellDepends = [
    base bytestring containers cryptonite deepseq directory filepath
    flat ListLike memory model mtl pretty template-haskell text
    transformers aeson
  ];
  testHaskellDepends = [
    base bytestring containers flat model tasty tasty-hunit
    tasty-quickcheck text
  ];
  homepage = "http://github.com/tittoassini/typed";
  description = "Language xx independent, reproducible, absolute types";
  license = stdenv.lib.licenses.bsd3;
}
