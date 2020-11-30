{ pkgs ? import ./nix/pkgs.nix { }
, withProfiling ? false
}:
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "hinterface";
    src = ./.;
  };
  projectFileName = "cabal.project";
  compiler-nix-name = "ghc8102";
  pkg-def-extras = [ ];
  modules =
    ([{
      packages.hinterface.doCoverage = true;
    }]) ++
    (if withProfiling then
      [{
        packages.hinterface.package.ghcOptions = "-fprof-auto";
        packages.hinterface.components.library.enableLibraryProfiling = true;
      }]
    else [ ])
  ;
}

