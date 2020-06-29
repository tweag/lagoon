let
  pkgs = import ./nix {};
in
  {
    inherit (pkgs)
      lagoonDocker
      pylagoonTarGz
      rubylagoon
      # TODO - This isn't working
      rubylagoon-tests
      rubylagoongem
      ;
    inherit (pkgs.haskellPackages)
      lagoon-server
      lagoon-cmdline
      ;
    inherit (pkgs.pythonPackages) pylagoon;
    inherit (pkgs.rPackages) rlagoon;
  } // pkgs.lagoon-tests # TODO - These tests are also broken after refactor
