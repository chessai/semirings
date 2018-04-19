{ mkDerivation, base, fetchgit, stdenv, transformers }:
mkDerivation {
  pname = "constrictor";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/constrictor.git";
    sha256 = "1gv3f8n9dfmcf94yjvgf3h34749ii10ifa9h1kw3j1q9casqbzwl";
    rev = "f76fcb935b62fec90ad6e9cb3b8adb2507402f3a";
  };
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/chessai/constrictor.git";
  description = "strict versions of many things in base";
  license = stdenv.lib.licenses.mit;
}
