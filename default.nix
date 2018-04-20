{ package ? "semirings", compiler ? "ghc841" }:
let fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
    nixpkgs = fetchNixpkgs {
      rev = "951d6a9f93f95b500f37d3c2dbb27f8b7dc512c3"; 
      sha256 = "00a7bk1awgl2ndndakc280riq8sj6m0gifl46v0xq034pf8mnliw"; 
      sha256unpacked = "0zzi0sv4a156qkbp3xhb85d5vm127kcxgmsqhfwvwgmlz8fidlnd"; 
    };
    pkgs = import nixpkgs { config = {}; overlays = []; };
    inherit (pkgs) haskell;
 
  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);
    
  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };
    {
      coerce-util = cp "coerce-util"; 
      constrictor = cp "constrictor"; 
      semirings = build "semirings" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  semirings = if pkgs.lib.inNixShell then drv.env else drv;
}
