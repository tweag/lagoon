{ callPackage, nixpkgsSrc, rPackages, rWrapper }:
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
    devDependencies = 
      [
        rPackages.devtools
        rPackages.roxygen2
      ];
in {
  rdevEnv = rWrapper.override {
      packages = devDependencies ++ depends;
  };
  rlagoon = buildRPackage
    { name = "rlagoon";
      src = ../RLagoon;
      requireX = false;
      propagatedBuildInputs = depends;
      nativeBuildInputs = depends;
    };
}
