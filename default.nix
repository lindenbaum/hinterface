{ pkgs ? import ./nix/pkgs.nix { }
}:
let
  out =
    pkgs.haskell-nix.project {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "hinterface";
        src = ./.;
      };
      projectFileName = "cabal.project";
      compiler-nix-name = "ghc8105";
      pkg-def-extras = [ ];
      modules = [{
        packages.hinterface.doCoverage = true;
        packages.hinterface.components.tests.hinterface-test.build-tools = [
          out.hspec-discover
        ];
      }];
    };

in
out

