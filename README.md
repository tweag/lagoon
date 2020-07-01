# Datalake

![](https://github.com/tweag/lagoon/workflows/master/badge.svg)

Datalake is an application for centralizing structured and semi-structured data in an easy-to-query format. Written in Haskell 
and built on top of Postgres, Datalake supports a variety of features including automatic schema generation, SQL queries, 
cross-dataset querying, and dataset versioning. Datalake includes a REST API, allowing it to be easily integrated with a 
wide range of client applications. 

## Components

### Server and CLI

The core components of datalake are its [server](server) and [command line client](clients/cmdline) applications. Follow the [getting started guide](docs/GETTING_STARTED.md) to 
configure and start running a datalake instance.


### Clients

For convenience, Datalake also includes client libraries for interacting with its REST API in a variety of programming languages:

  * [Python (PyDatalake)](clients/PyDatalake)
  * [Ruby (RubyDatalake)](clients/RubyDatalake)
  * [R (RDatalake)](clients/RDatalake)

## Installation

### Docker

Pre-built docker images containing the datalake server and CLI may be found on [DockerHub](TODO). Simply pull one of the images there and follow the [configuration guide](docs/CONFIG.md) to get started.

### From source

Datalake is packaged with [nix](https://nixos.org/download.html), and all build configurations can be found in the [nix/](nix) directory.

To build the datalake server, simply install nix then run:

    nix-build -A datalake-server


#### Available Nix attributes:

**Applications and Libraries:**
  * datalake-server
  * datalake-cmdline
  * pydatalake
  * rubydatalake
  * rubydatalakegem
  * rdatalake

**Test Scripts:**
  * runtests (runs all integration tests)
  * runcompacttests
  * runsecuritytests
  * rubydatalake-tests

**Docker Images:**
  * datalakeDocker.server
  * datalakeDocker.client
