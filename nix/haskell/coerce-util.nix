{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "coerce-util";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/coerce-util.git";
    sha256 = "196rvpnldw88m7b4n9qd6cijqdq5cr4bfylnj4x6dyvyybdyil3s";
    rev = "d55b020e8458d27650c45a9a21c1241c78220323";
  };
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/chessai/coerce-util.git";
  description = "utils for Data.Coerce";
  license = stdenv.lib.licenses.mit;
}
