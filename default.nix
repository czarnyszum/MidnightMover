{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, crypton-connection, crypton-x509, filepath, http-client
, http-client-tls, http-types, lens, lib, mtl, tagsoup, text, tls
, wreq
}:
mkDerivation {
  pname = "MidnightMover";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers crypton-connection
    crypton-x509 filepath http-client http-client-tls http-types lens
    mtl tagsoup text tls wreq
  ];
  license = lib.licenses.mit;
  mainProgram = "MidnightMover";
}
