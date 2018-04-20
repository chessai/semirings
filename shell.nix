{ package ? "semirings", compiler ? "ghc842" }:

(import ./default.nix {
  inherit package compiler;
}).semirings
