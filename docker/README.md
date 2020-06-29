# docker

Since this project uses nix for packaging, Docker images are generated using nix's dockertools module and are defined as a nix expression in [docker.nix](docker.nix) instead of a traditional Dockerfile.

Docker image derivations should be accessed via the default.nix in the repository root directory. To build the datalake server image tarball and load into docker, run:
    
    docker load < $(nix-build ../default.nix -A datalakeDocker.datalake-server)
