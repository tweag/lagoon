{ callPackage, nixpkgsSrc, rPackages }:
  let
    buildRPackage =
      callPackage "${nixpkgsSrc}/pkgs/development/r-modules/generic-builder.nix"
      { Cocoa = null; Foundation = null; };
    depends =
      [
        rPackages.DBI
        rPackages.dplyr
        rPackages.dbplyr
        rPackages.jsonlite
        rPackages.httr
      ];
in
  buildRPackage
    { name = "rlagoon";
      src = ../RLagoon;
      requireX = false;
      propagatedBuildInputs = depends;
      nativeBuildInputs = depends;
    }
