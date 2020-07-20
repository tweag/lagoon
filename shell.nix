{ }:
let
  let
  pkgs = import ./nix {};
in
pkgs.haskell.packages.shellFor {
    packages = p: [
      p."{{cookiecutter.project_name}}"
    ];
    buildInputs = with pkgs.haskellPackages; [
      myHaskellPackages.cabal-install
      ghcid
      ormolu
      hlint
      (import sources.niv {}).niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
  };