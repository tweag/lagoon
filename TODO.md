Note: this will be removed prior to release

Cleanup TODO List:

* [X] General cleanup/reorg
* [X] Update nix build after refactor
* [X] Review old "test" attributes and see if they are still useful (integration tests)
        * "runtests" passes
        * "runs3tests" fails -> requies a configured s3 client/project as far as I can tell
        * "runsecuritytests" passes
        * "runcompacttests" passes
* [ ] Documentation cleanup/simplification

    [X] README.md
    [X] clients/README.md
    [ ] clients/python/README.md
    [ ] clients/R/README.md
    [ ] clients/ruby/README.md
    [X] clients/cmdline/README.md
    [X] src/README.md
    [X] src/backend/README.md
    [X] src/interface/README.md
    [X] src/frontend/README.md
    [X] src/server/README.md
    [X] nix/
* [X] Move server to /app
* [X] DockerHub Org
* [X] License File
* [X] Move Docker image(s) to repo
* [ ] Update Docker image entrypoint
* [ ] Updated CI Pipeline
    [ ] Select preferred CI platform (GitHub Actions?)
    [ ] Translate old CI scripts
    [ ] Add stage for integration tests
    [ ] Add stage for building docker images
* [ ] Examples

Also:
* [ ] Bump nixpkgs and switch srcs to niv 
* [ ] PyDatalake -> poetry2nix packaging

Questions:
* Q: Can we delete datalake/clients/cmdline/test-cases/docker-compose.yaml?
* Q: Publish python, R, ruby packages to pypi, cran, etc.?

General Notes for Myself:
* The few unit tests which exist are executed during the nix build
    * https://stackoverflow.com/questions/52864666/is-it-possible-to-keep-the-test-suite-logs-generated-by-cabal-when-run-through-n
* Integration tests will not run if you have postgres running locally on the default 5432 port
* Integration tests seem to hang without exiting on success
* Saeed can talk about cachix actions for GitHub 
