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
        (import (import sources."haskell.nix" { }).sources.nixpkgs { });
    in
    import sources."haskell.nix" { pkgs = tmpNixpkgs; };
  haskellDotNixNixpkgs = import haskellDotNix.sources.nixpkgs;
  nixpkgsArgs =
    haskellDotNix.nixpkgsArgs //
    {
      overlays =
        haskellDotNix.nixpkgsArgs.overlays
        ++
        [
          (self: super:
            {
              # Reflect the sources
              # ===================
              inherit sources;

              # For updating dependencies
              niv = (import sources.niv { pkgs = super; }).niv;

            })
        ];

      config =
        haskellDotNix.nixpkgsArgs.config
        // crossConfig
        // { allowUnfree = true; };
    };
in
selectCrossPkgs (haskellDotNixNixpkgs nixpkgsArgs)

