{ mkDerivation, aeson, base, bytestring, http-client
, http-client-tls, lens, lib, mtl, tagsoup, text, wreq
}:
mkDerivation {
  pname = "MidnightMover";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring http-client http-client-tls lens mtl tagsoup
    text wreq
  ];
  license = lib.licenses.mit;
  mainProgram = "MidnightMover";
}
