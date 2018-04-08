{ mkDerivation, base, fetchgit, stdenv, transformers }:
mkDerivation {
  pname = "constrictor";
  version = "0.1.0.2";
  src = fetchgit {
    url = "https://github.com/chessai/constrictor.git";
    sha256 = "0ccf46r1fmzmv6azj90pqh6n8flhbmrjya5i2a24862c6g2hb43n";
    rev = "20da78d52eddf9cc39254d56aa4a596af6837fd6";
  };
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/chessai/constrictor.git";
  description = "strict versions of many things in base";
  license = stdenv.lib.licenses.mit;
}
