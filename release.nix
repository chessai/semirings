builtins.listToAttrs
  (map
    (compiler: {
      name = compiler;
      value = import ./default.nix { inherit compiler; };
    })
    [ "ghc742"
      "ghc762"
      "ghc784"
      "ghc7103"
      "ghc802"
      "ghc822"
      "ghc842" ])
