{ package ? "fcg", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).fcg
