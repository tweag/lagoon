# Lagoon

![](https://github.com/tweag/lagoon/workflows/master/badge.svg)

Lagoon is an application for centralizing structured and semi-structured data in an easy-to-query format. Written in Haskell 
and built on top of Postgres, Lagoon supports a variety of features including automatic schema generation, SQL queries, 
cross-dataset querying, and dataset versioning. Lagoon includes a REST API, allowing it to be easily integrated with a 
wide range of client applications. 

## Components

### Server and CLI

The core components of lagoon are its [server](server) and [command line client](clients/cmdline) applications. Follow the [getting started guide](docs/GETTING_STARTED.md) to 
configure and start running a lagoon instance.


### Clients

For convenience, Lagoon also includes client libraries for interacting with its REST API in a variety of programming languages:

  * [Python (PyLagoon)](clients/PyLagoon)
  * [Ruby (RubyLagoon)](clients/RubyLagoon)
  * [R (RLagoon)](clients/RLagoon)

## Installation

### Docker

Pre-built docker images containing the lagoon server and CLI may be found on [DockerHub](TODO). Simply pull one of the images there and follow the [configuration guide](docs/CONFIG.md) to get started.

### From source

Lagoon is packaged with [nix](https://nixos.org/download.html), and all build configurations can be found in the [nix/](nix) directory.

To build the lagoon server, simply install nix then run:

    nix-build -A lagoon-server


#### Available Nix attributes:

**Applications and Libraries:**
  * lagoon-server
  * lagoon-cmdline
  * pylagoon
  * rubylagoon
  * rubylagoongem
  * rlagoon

**Test Scripts:**
  * runtests (runs all integration tests)
  * runcompacttests
  * runsecuritytests
  * rubylagoon-tests

**Docker Images:**
  * lagoonDocker.server
  * lagoonDocker.client
