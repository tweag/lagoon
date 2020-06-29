# Datalake Developer Documentation

## Overview

The overarching goal of the Datalake project is to provide a central database
of medical research results to facilitate scientific research. It provides means
to add new results into this "datalake" (ingest), query this database (data
discovery), as well as ways to interact with the data through bindings to R,
MatLab and programming language API bindings.

The datalake project consists of a number of related components, each of which
has its own developer documentation:

* [datalake-interface](interface/HACKING.md) is a Haskell library that defines the API
  between the backend (datalake server) on the one hand and the frontend (either
  the webapp, the datalake command-line tool or the R/Ruby/Python clients) on
  the other.
* [datalake-backend](backend/HACKING.md) implements the bulk of the backend
  functionality (server and server).
* [datalake-cmdline](cmdline/HACKING.md) is a thin layer around the backend library
  that makes its functionality available through a command line program.
* [datalake-server](server/HACKING.md) is a thin layer around the backend
  library that makes its functionality available through a REST API.
* [datalake-webapp](webapp/HACKING.md) provides data discovery as well as
  ingest functionality through a web interface.
* [RDatalake](RDatalake/HACKING.md) provides an R interface to the datalake.
* [MatlabDatalake](MatlabDatalake/HACKING.md) provides a Matlab interface to the
  datalake.


## Tests

### Unit Tests

Unit tests are included with the respective libraries and are executed during the nix build.

### Integration tests

A suite of integration tests is included with the command line client in [clients/cmdline/test-cases](../clients/cmdline/test-cases). 
These tests are bundled during the nix build as an executable script, e.g.
    
    nix-build -A runtests

To run without nix, please see the configuration script [clients/cmdline/test-cases/config.sh](../clients/cmdline/test-cases/config.sh).
