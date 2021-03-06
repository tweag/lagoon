{ fetch ? import ./fetch.nix }:
[

  (self: super:
    { haskellPackagesGHCJS = super.haskell.packages.ghcjsHEAD; }
  )

  # Some extra sources
  (self: super:
    let
      ourOverrides =
          (super.haskell.lib.packageSourceOverrides
            { reflex-dom-contrib = fetch "reflex-dom-contrib";
            }
          );
    in
    { haskellPackages = super.haskellPackages.extend ourOverrides;
      haskellPackagesGHCJS = super.haskellPackagesGHCJS.extend ourOverrides;
    }
  )

  # Our packages
  (self: super:

    let
      ourSourceOverrides =
        super.haskell.lib.packageSourceOverrides
         (
         (self.lib.mapAttrs (k: v:
            self.lib.cleanSourceWith
              { filter = name: type:
                  # Some folders have a "test-cases" directory which we don't
                  # actually depend on during build
                  !(type == "directory" &&
                    self.lib.strings.hasSuffix "test-cases" name);
                src = self.lib.cleanSource v;
              })
         { lagoon-interface = ../src/interface;
           lagoon-backend = ../src/backend;
           lagoon-cmdline = ../clients/cmdline;
           lagoon-server = ../server;
           lagoon-frontend = ../src/frontend;
         })
         );
    in
    { haskellPackages = super.haskellPackages.extend ourSourceOverrides;
      haskellPackagesGHCJS =
        super.haskellPackagesGHCJS.extend ourSourceOverrides;
    }
  )

  # Fixups for reflex and its dependencies
  (self: super:
    let
      ourOverrides =
        haskellSelf: haskellSuper:
         { exception-transformers =
             super.haskell.lib.doJailbreak
             haskellSuper.exception-transformers;

           haskell-src-exts-util = super.haskell.lib.doJailbreak
               haskellSuper.haskell-src-exts-util;

           reflex-dom-contrib = super.haskell.lib.doJailbreak
               haskellSuper.reflex-dom-contrib;

           reflex =super.haskell.lib.dontCheck
             ( haskellSuper.callPackage (import (fetch "reflex")) {});
         } // (
           let
             repo = import (fetch "reflex-dom") haskellSelf super;
             reflex-dom-core = super.haskell.lib.dontCheck
               (haskellSuper.callPackage
               ("${fetch "reflex-dom"}/reflex-dom-core") {});
             reflex-dom = haskellSuper.callPackage
               ("${fetch "reflex-dom"}/reflex-dom")
               { ghcBackend = "warp";};
           in { inherit reflex-dom reflex-dom-core ; }
         ) // (import (fetch "jsaddle") haskellSelf)
        ;
    in
    { haskellPackages = super.haskellPackages.extend ourOverrides;
      haskellPackagesGHCJS = super.haskellPackagesGHCJS.extend ourOverrides;
    }
  )

  (self: super:
    { rubylagoongem = self.stdenv.mkDerivation
        { name = "rubylagoongem";
          src = ../clients/RubyLagoon;
          buildPhase  =
            ''
              gem build lagoon.gemspec
            '';
          installPhase =
            ''
              mkdir -p $out
              cp lagoon-*.gem $out
            '';
          buildInputs = [ self.ruby ];
        };
    }
  )

  (self: super:
    { rubylagoon = super.bundlerEnv
        { name = "rubylagoon";
          ruby = super.ruby;
          gemdir = ../clients/RubyLagoon;
        };
    }
  )

  (self: super:
    { rubylagoon-tests =
        with (super.callPackage ./tests/environment.nix
          { inherit (self.haskellPackages) lagoon-server; });
        let
          path = self.lib.makeBinPath
            [ self.ruby self.gcc ];
          runRubyTests = self.writeScript "run-ruby-tests"
            ''
              #!${self.stdenv.shell}
              source $stdenv/setup
              set -euo pipefail
              set -x
              export PATH=${path}:$PATH
              export LAGOON_HOST=localhost
              export LAGOON_PORT=22089
              export RUBY_TEST_DIR=${../clients/RubyLagoon/test-cases}
              # TODO:
              #   1. Copy all the gems generated by "rubylagoon"
              #   2. Run this within withPG and withDL
              gem install --force --local \
                ${super.rubylagoongem}/* \
                ${super.rubylagoon}/lib/ruby/gems/2.4.0/cache/*
              ruby ${../clients/RubyLagoon/test.rb}

              touch $out
            '';
          in self.runCommand "run-rubylagoon-tests" {}
                ''
                  ${withLagoonServer {} runRubyTests}
                '';
    }
  )

  # jsaddle checks involve lots of dependencies so disable them
  (self: super:
    { haskellPackages =
        super.haskellPackages.extend
          (haskellSelf: haskellSuper:
            { jsaddle-warp =
              super.haskell.lib.dontCheck haskellSuper.jsaddle-warp;
            }
          );
    }
  )

  (self: super:
    { rPackages =
        let rLagoonPkgs = import ../clients/RLagoon/default.nix
          { inherit (self) callPackage rPackages rWrapper;
            nixpkgsSrc = self.path;
          };
        in
        super.rPackages.override
          {
            overrides =
              {
                rlagoon = rLagoonPkgs.rlagoon;
              };
          };
    }
  )

  # Helper bundle with all the lagoon packages
  (self: super:
    { pylagoonTarGz = self.stdenv.mkDerivation
        { name = "pylagoon-tar-gz";
          src = self.lib.cleanSource ../clients/PyLagoon;
          buildInputs = [ (super.python.withPackages (ps: [ps.setuptools])) ];
          phases = [ "unpackPhase" "installPhase" ] ;
          installPhase =
            ''
              touch -t "201108231405.14" * **/*
              python setup.py sdist --formats=zip,gztar --dist-dir=$out
            '';
        };
      }
  )

  (self: super:
    {
      python = super.python.override {
        packageOverrides = selfPython: superPython:
          {
            pylagoon = super.python.pkgs.buildPythonPackage rec
              { pname = "pylagoon";
                version = "0.1.0";
                src = "${super.pylagoonTarGz}/PyLagoon-${version}.tar.gz";
                propagatedBuildInputs =
                  [ superPython.psycopg2
                    superPython.pyyaml
                    superPython.pandas
                    superPython.requests
                    superPython.sqlalchemy
                  ];
              };
          };
      };
    }
  )

  # Helper bundle with all the lagoon packages
  (self: super:
    { lagoonPackages = haskellPackages:
        super.lib.attrsets.filterAttrs
          (k: _: self.lib.strings.hasPrefix "lagoon-" k)
          haskellPackages;
    }
  )

  (self: super:
    { lagoon-tests = super.callPackage ./tests/test.nix {}; }
  )

  (self: super:
    { libredirect = super.callPackage ./libredirect {} ; }
  )

  (self: super: 
    { lagoonDocker = super.callPackage ../docker/docker.nix {lagoon-server=super.haskellPackages.lagoon-server; lagoon-cmdline=super.haskellPackages.lagoon-cmdline;}; }
  )
]
