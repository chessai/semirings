{ mkDerivation, base, fetchgit, ghc-prim, stdenv, transformers }:
mkDerivation {
  pname = "constrictor";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/chessai/constrictor.git";
    sha256 = "0kr6nvk02i73jl841a0cwkqxyxlnxlhyqz1fzkks6s7rd6sfc8ky";
    rev = "03519497e84f9dfd70d49f7282d6ec6f65aae7f2";
  };
  libraryHaskellDepends = [ base ghc-prim transformers ];
  homepage = "https://github.com/chessai/constrictor.git";
  description = "strict versions of many things in base";
  license = stdenv.lib.licenses.mit;
}
