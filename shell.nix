{ package ? "semirings", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).semirings
