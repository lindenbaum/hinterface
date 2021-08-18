let
  pkgs = import ./nix/pkgs.nix { };
in
(import ./default.nix { inherit pkgs; }).shellFor {
  packages = p: [ p.hinterface ];
  withHoogle = true;
  tools = {
    cabal = "latest";
    ormolu = "latest";
    haskell-language-server = "latest";
  };
  buildInputs = with pkgs.haskellPackages;
    [
      tasty-discover
    ];
}

