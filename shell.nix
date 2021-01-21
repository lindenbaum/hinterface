let
  pkgs = import ./nix/pkgs.nix { };
in
(import ./default.nix { inherit pkgs; }).shellFor {
  packages = p: [ p.hinterface ];
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    ormolu = "0.1.4.1";
    haskell-language-server = "0.8.0";
  };
  buildInputs = with pkgs.haskellPackages;
    [
      tasty-discover
    ];
}

