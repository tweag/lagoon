# Datalake Developer Documentation

The overarching goal of the Datalake project is to provide a central database
of medical research results to facilitate scientific research. It provides means
to add new results into this "datalake" (ingest), query this database (data
discovery), as well as ways to interact with the data through bindings to R,
MatLab and programming language API bindings.

The datalake project consists of a number of interlocked parts, each of which
has its own developer documentation:

* [datalake-interface](interface/HACKING.md) is a library that defines the API
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

# Building

## Building datalake

To build datalake, run the following from the root of the project:

`stack build`

# Installation

## Run everything on a single machine

This is useful for development purposes, or just to experiment with `datalake`
(the command-line client) Python, Ruby or R clients. First you will need docker images for the server and, optionally, for the webapp:

* `$ make server-image` creates the datalake-server image
* `$ make webapp-image` creates the datalake-webapp image

Alternatively you can build all images with:

``` shell
$ make docker-images
```

The following tools are required to build the images:

* The [`make`](https://www.gnu.org/software/make/) build tool
* The Haskell [`stack`](https://docs.haskellstack.org/en/stable/README/) build
  tool
* The [`pandoc`](https://pandoc.org/) tool to convert markdown to HTML (webapp)

Note that building the webapp may take a while, so consider building the server
image only.

You can then run

``` shell
$ docker-compose up
```

from the top-level directory, which will start docker containers with the
datalake-server and the datalake webapp. If you have _not_ built the webapp,
and do not want to have it running, comment out the `datalake-webapp:` section
from the `docker-compose.yaml` file.

The datalake-server will be accessible on your localhost at port `1234`.

# Testing

The datalake tests are located in [cmdline/test-cases](cmdline/test-cases).
Please see the configuration instructions in
[cmdline/test-cases/config.sh](cmdline/test-cases/config.sh).
