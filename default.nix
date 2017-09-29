{ mkDerivation, base, network, stdenv, text, bytestring, containers, attoparsec, acid-state, safecopy, time, mtl }:
mkDerivation {
  pname = "my-smtp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base network text bytestring containers attoparsec acid-state safecopy time mtl ];
  description = "A implementation of the SMTP based on RFC 821";
  license = stdenv.lib.licenses.gpl3;
}
