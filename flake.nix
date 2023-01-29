{
  description = "Erlang and Haskell communication library";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    # Flake for better Haskell builds
    # For updating to a newer fersion comment this line
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # --------------------------------
    # This is required for ./shell.nix
    # And the ./shell.nix is required for the vscode
    # nix-environment-selector plugin
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    # --------------------------------
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {flake-utils, nixpkgs, haskellNix, ...}:
   flake-utils.lib.eachSystem [flake-utils.lib.system.x86_64-linux] (system:
        let
          # Set the line below to 'true' to enable profiling builds
          withProfiling = false;

          pkgs = import nixpkgs {
            inherit system overlays;
            inherit (haskellNix) config;
          };
          overlays = [haskellNix.overlay (final: prev:
            {
              hinterface = final.haskell-nix.project {
                src = final.haskell-nix.cleanSourceHaskell {
                  src = ./.;
                  name = "hinterface";
                };
                projectFileName = "cabal.project";
                compiler-nix-name = "ghc944"; # "ghc925";
                pkg-def-extras = [];
                modules =
                  [
                    {
                      packages.hinterface.components.library = {
                        enableLibraryProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                      };
                      packages.hinterface.components.exes.hinterface-echo = {
                        enableProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                      };
                      packages.hinterface.components.tests.hinterface-test = {
                        enableProfiling = withProfiling;
                        ghcOptions = if withProfiling then ["-fprof-auto"] else [];
                        # HACK make 'cabal test' work
                        # https://github.com/input-output-hk/haskell.nix/issues/231#issuecomment-731699727
                        build-tools = [
                          final.hinterface.hsPkgs.hspec-discover
                        ];
                        # END OF HACK
                      };
                    }
                  ];
                 shell.tools = {
                    cabal = {};
                    hlint = {};
                    ormolu = {};
                    haskell-language-server = {};
                  };
                 # Non-Haskell shell tools go here
                 shell.buildInputs = with pkgs; [
                    alejandra
                    erlang
                 ];
              };
            })];
        in
        pkgs.hinterface.flake {}
    );
}
