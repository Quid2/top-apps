{ mkDerivation, async, base, bytestring, containers, ed25519, extra
, pipes, process, regex-tdfa, stdenv, top, transformers, typed
}:
mkDerivation {
  pname = "top-apps";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base bytestring containers ed25519 extra pipes process
    regex-tdfa top transformers typed
  ];
  homepage = "http://github.com/tittoassini/top-apps";
  description = "Example applications for top, the typed oriented protocol";
  license = "unknown";
}
