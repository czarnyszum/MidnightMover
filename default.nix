{ mkDerivation, aeson, base, blaze-html, bytestring, containers
, crypton-connection, crypton-x509, directory, encoding, filepath
, html-conduit, http-client, http-client-tls, http-types, lens, lib
, mtl, process, text, time, tls, webdriver, wreq, xml-conduit
}:
mkDerivation {
  pname = "MidnightMover";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html bytestring containers crypton-connection
    crypton-x509 directory encoding filepath html-conduit http-client
    http-client-tls http-types lens mtl process text time tls webdriver
    wreq xml-conduit
  ];
  license = lib.licenses.mit;
  mainProgram = "MidnightMover";
}
