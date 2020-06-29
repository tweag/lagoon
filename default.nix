let
  pkgs = import ./nix {};
in
  {
    inherit (pkgs)
      datalakeDocker
      pydatalakeTarGz
      rubydatalake
      # TODO - This isn't working
      rubydatalake-tests
      rubydatalakegem
      ;
    inherit (pkgs.haskellPackages)
      datalake-server
      datalake-cmdline
      ;
    inherit (pkgs.pythonPackages) pydatalake;
    inherit (pkgs.rPackages) rdatalake;
  } // pkgs.datalake-tests # TODO - These tests are also broken after refactor
