let
  pkgs = import ./nix/pkgs.nix { };
in
(import ./default.nix { inherit pkgs; }).shellFor {
  packages = p: [ p.hinterface ];
  withHoogle = true;
  tools = {
    cabal = {};
    haskell-language-server = {};
  };
  buildInputs = with pkgs.haskellPackages;
    [
      tasty-discover
      pkgs.erlang_nox
    ];
}

