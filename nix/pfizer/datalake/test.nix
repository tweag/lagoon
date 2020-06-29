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
    { inherit (haskellPackages) datalake-server; }
  );
let

  inherit (haskellPackages) datalake-cmdline;

  # The test derivation. Doesn't produce any output.
  datalakeTests =
    { # TODO: run compaction unit tests
      runtests =
        runCommand
          "runtests"
          { buildInputs = [ datalake-cmdline ]; }
          (withGeneratedData (withDatalakeServer
              { schema = "CmdlineTests"; } runTests));

      runs3tests =
        runCommand
          "runs3tests"
          { buildInputs = [ datalake-cmdline ]; }
          (withGeneratedData (withDatalakeServer
              { schema = "CmdlineTests"; enableS3 = true; } runS3Tests));

      runcompacttests =
        runCommand
          "runtests-compact"
          { buildInputs = [ datalake-cmdline ]; }
          (withGeneratedData (withDatalakeServer
              { schema = "CmdlineTests";} runTestsCompact));

      runsecuritytests =
        runCommand
          "runsecuritytests"
          { buildInputs = [ datalake-cmdline ]; }
          (withDatalakeServer
              { schema = "CmdlineTests"; } runSecurityTests);

    };
  # The test runner
  runTests =
    let
      runner =
        fixShell ../../../clients/cmdline/test-cases/runtests.sh;
    in writeScript "run-datalake-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        GO="datalake" \
          DATALAKE_TEST_DIR="${testCasesDir}" \
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
    in writeScript "run-datalake-s3-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${awscli}/bin:$PATH
        GO="datalake" \
          DATALAKE_TEST_DIR="${testCasesDir}" \
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
    in writeScript "run-datalake-security-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        export PATH=${path}:$PATH
        GO="datalake" \
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
    in writeScript "run-datalake-tests"
      ''
        #!${stdenv.shell}
        source $stdenv/setup
        set -euo pipefail
        GO="datalake" \
          DATALAKE_TEST_DIR="${testCasesDir}" \
          ${runner} --no-load-config \
          2>&1 \
          | sed 's/^/runcompacttests: /'
        touch $out
      '';

  testCasesDir = lib.cleanSource ../../../clients/cmdline/test-cases;

in datalakeTests
