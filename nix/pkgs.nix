# This file contains a ready-to-use 'nixpkgs' with the
# sources from the niv based 'sources.nix' and the 
# 'overlay.nix' applied.
#
# It supports IOHK's Haskell.Nix and uses the nixpkgs 
# version provided by IOHK.
#
# This file also supports extra settings for cross compiling 
# especially for static executables based on musl64.
{ selectCrossPkgs ? (x: x)
, crossConfig ? { }
}:
let
  sources = import ./sources.nix { };
  # Haskell.nix support  
  haskellDotNix =
    let
      tmpNixpkgs = selectCrossPkgs
        (import (import sources."haskell.nix" { }).sources.nixpkgs-2009 { });
    in
    import sources."haskell.nix" { pkgs = tmpNixpkgs; };
  haskellDotNixNixpkgs = import haskellDotNix.sources.nixpkgs-2009;
  nixpkgsArgs =
    haskellDotNix.nixpkgsArgs //
    {
      overlays =
        haskellDotNix.nixpkgsArgs.overlays
        ++ import ./overlay.nix sources;
      config =
        haskellDotNix.nixpkgsArgs.config
        // crossConfig
        // { allowUnfree = true; };
    };
in
selectCrossPkgs (haskellDotNixNixpkgs nixpkgsArgs)

