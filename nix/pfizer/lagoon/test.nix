# This module exports a single derivation that succeeds if the tests succeed,
# and fails otherwise. No output is generated.
{ runCommand
, coreutils
, glibcLocales
, postgresql
, curl
, lib
, stdenv
, writeScript
, haskellPackages
, netcat
, callPackage
, awscli
}:

with
  (callPackage ./environment.nix
    { inherit (haskellPackages) lagoon-server; }
  );
let

  inherit (haskellPackages) lagoon-cmdline;

  # The test derivation. Doesn't produce any output.
  lagoonTests =
    { # TODO: run compaction unit tests
      runtests =
        runCommand
          "runtests"
          { buildInputs = [ lagoon-cmdline ]; }
          (withGeneratedData (withLagoonServer
              { schema = "CmdlineTests"; } runTests));

      runs3tests =
        runCommand
          "runs3tests"
          { buildInputs = [ lagoon-cmdline ]; }
          (withGeneratedData (withLagoonServer
              { schema = "CmdlineTests"; enableS3 = true; } runS3Tests));

      runcompacttests =
        runCommand
          "runtests-compact"
          { buildInputs = [ lagoon-cmdline ]; }
          (withGeneratedData (withLagoonServer
              { schema = "CmdlineTests";} runTestsCompact));

      runsecuritytests =
        runCommand
          "runsecuritytests"
          { buildInputs = [ lagoon-cmdline ]; }
          (withLagoonServer
              { schema = "CmdlineTests"; } runSecurityTests);

    };
  # The test runner
  runTests =
    let
      runner =
        fixShell ../../../clients/cmdline/test-cases/runtests.sh;
    in writeScript "run-lagoon-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        GO="lagoon" \
          LAGOON_TEST_DIR="${testCasesDir}" \
          ${runner} --no-gen-data --no-load-config \
          2>&1 \
          | sed 's/^/runtests: /'
        touch $out
      '';

  # The test runner
  runS3Tests =
    let
      runner =
        fixShell ../../../clients/cmdline/test-cases/runs3tests.sh;
    in writeScript "run-lagoon-s3-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${awscli}/bin:$PATH
        GO="lagoon" \
          LAGOON_TEST_DIR="${testCasesDir}" \
          ${runner} --no-gen-data --no-load-config \
          2>&1 \
          | sed 's/^/runs3tests: /'
        touch $out
      '';

  # The security test runner
  runSecurityTests =
    let
      path = lib.makeBinPath [ curl ];
      runner =
        fixShell ../../../clients/cmdline/test-cases/runsecuritytests.sh;
    in writeScript "run-lagoon-security-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${path}:$PATH
        GO="lagoon" \
          EXAMPLE_DATASET="${../../../clients/cmdline/test-cases/tests/087_escaped_headers.csv}" \
          ${runner} --no-load-config \
          2>&1 \
          | sed 's/^/runsecuritytests: /'
        touch $out
      '';

  # The golden test runner
  runTestsCompact =
    let
      runner =
        fixShell ../../../clients/cmdline/test-cases/runcompactiontests.sh;
    in writeScript "run-lagoon-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        GO="lagoon" \
          LAGOON_TEST_DIR="${testCasesDir}" \
          ${runner} --no-load-config \
          2>&1 \
          | sed 's/^/runcompacttests: /'
        touch $out
      '';

  testCasesDir = lib.cleanSource ../../../clients/cmdline/test-cases;

in lagoonTests
