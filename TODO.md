Cleanup TODO List:

* [X] General cleanup/reorg
* [X] Update nix build after refactor
* [X] Review old "test" attributes and see if they are still useful (integration tests)
* [X] Documentation cleanup/simplification

    * [X] README.md
    * [X] clients/README.md
    * [X] clients/python/README.md
    * [X] clients/R/README.md
    * [X] clients/ruby/README.md
    * [X] clients/cmdline/README.md
    * [X] src/README.md
    * [X] src/backend/README.md
    * [X] src/interface/README.md
    * [X] src/frontend/README.md
    * [X] src/server/README.md
    * [X] nix/
* [X] Move server to /app
* [X] DockerHub Org
* [X] License File
* [X] Move Docker image(s) to repo
* [X] Update Docker image entrypoint
* [~] Updated CI Pipeline
    * [X] Select preferred CI platform (GitHub Actions?)
    * [X] Translate old CI scripts
    * [X] Add stage for integration tests (impossible until they are updated)
    * [X] Add stage for building docker images
* [ ] Examples
    * [X] Getting started guide
    * [X] Config examples

Nice to have:
* [ ] Fix integration tests:
    * [ ] Fix issue where integration tests never exit, making them un-runnable on CI
    * [ ] Update integration tests to run in sandbox instead of on host
* [ ] Bump nixpkgs and switch srcs to niv 
* [ ] PyDatalake -> poetry2nix packaging
* [ ] Publish python, R, ruby packages to pypi, cran, etc.?
