{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, crypton-connection, crypton-x509, filepath, html-conduit
, http-client, http-client-tls, http-types, lens, lib, mtl, text
, tls, wreq, xml-conduit
}:
mkDerivation {
  pname = "MidnightMover";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers crypton-connection
    crypton-x509 filepath html-conduit http-client http-client-tls
    http-types lens mtl text tls wreq xml-conduit
  ];
  license = lib.licenses.mit;
  mainProgram = "MidnightMover";
}
