{ mkDerivation, acid-state, async, base, bytestring, containers
, data-default-class, deepseq, extra, fetchgit, filepath, flat
, hslogger, invariant, ListLike, mtl, pipes, safecopy, stdenv, stm
, template-haskell, text, th-lift, time, transformers, typed
, websockets
}:
mkDerivation {
  pname = "top";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/tittoassini/top";
    sha256 = "02jygwhjnllbsfpvinxxzjv93vhzzps8wp7bwrf9gacpccir98dm";
    rev = "4ea70d6eeb500bdefecb072647184f49328b940e";
  };
  libraryHaskellDepends = [
    acid-state async base bytestring containers data-default-class
    deepseq extra filepath flat hslogger invariant ListLike mtl pipes
    safecopy stm template-haskell text th-lift time transformers typed
    websockets
  ];
  testHaskellDepends = [
    async base bytestring extra hslogger stm template-haskell typed
    websockets
  ];
  homepage = "http://github.com/tittoassini/top";
  description = "API for top, the typed oriented protocol";
  license = stdenv.lib.licenses.bsd3;
}
