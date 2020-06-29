## Building the project

*Requirements: you can either install globally on your system the
external dependencies listed below before building, or you can use Nix
instead to install them in a local sandbox transparently (see below).*

**External dependencies:**
* bzip2
* zlib
* PostgreSQL >= 9.0
* R >= 3.0
* terraform

This project uses [Stack][haskell-stack] to build.

To provide the external dependencies using Nix, first
[follow the installation instructions][nix] for the Nix cross-platform
package manager. Then, to build the project, execute the following in
a local checkout:

```
$ stack --nix build
```

External dependencies don't need to be installed explicitly. When you
get tired of passing the `--nix` flag to each Stack command, add the
following to `~/.stack/config.yaml` to make it implicit:

```yaml
nix:
  enable: true
```

If you don't want to use Nix just call `stack` without the `--nix`
flag and without setting the above config, after having installed the
external dependencies listed above manually. Also, in that case you'll
need to perform the following once to install GHC:

```
$ stack setup
```

The following commands install the R package used import data and write
queries:

```
$ R --slave -e "install.packages(c('dplyr', 'jsonlite', 'httr'), repos='http://cran.us.r-project.org')"
$ R --slave -e "install.packages('RDatalake', repos=NULL, type='sources')"
```

[nix]: https://nixos.org/nix
[haskell-stack]: https://docs.haskellstack.org/

## Deploying in the cloud

This repository contains [Terraform][terraform] formulas for standing
up a demo cluster in a virtual private cloud (VPC) on AWS. The demo
cluster demonstrates a machine running a Datalake server for
interacting with a PostgreSQL database via a REST API and using the
`datalake` tool to load more data. You'll need AWS credentials to create
a new test cluster. Those should be added to `~/.aws/credentials` as
described [here][aws-creds]. You'll need Terraform installed, or you
can call a locally provisioned version of Terraform using Nix as
above:

```
$ stack --nix exec terraform
```

To create the test cluster, including a PostgreSQL database:

```
$ echo 'name="<username>"' >> terraform.tfvars
$ terraform apply
```

Where `<username>` is a name by which you can be identified on the
management console for admin purposes (you only need to add the user
name to `terraform.tfvars` once).

The last few lines of output will show a number of variables. If needed,
this variables can be retrieved again later with

```
$ terraform output
```

You can ssh into the client as follows, where the `<client>` value is as given
by Terraform. You may need to restrict the file permissions for `pfizer.pem` in
order for ssh to allow a connection.

```
$ ssh -i pfizer.pem ec2-user@<client>
```

On the client, you can connect to the DB instance from the
command-line as follows:

```
$ stack exec -- psql <db>
```

Use the password value as printed by Terraform.

It is also possible to connect to the Datalake server running on the client
machine via HTTP at `http://<client>:22089`.

## Continuous Integration and Artifacts

The continuous integration service [circleci.com][circleci] is used to test the
code and generate build outputs. For documentation about how to deploy
[datalake-server](./server/README.md) and [datalake-webapp](./webapp/README.md)
generally please see their respective documentation.

### Binary bundles

The CI server creates two binary bundles: [one for
linux](ci/bundle-doc-linux.md) and [one for windows](ci/bundle-doc-win.md).
Please see the corresponding documentation for more information.

### Docker images

Two docker images are built:

* `datalake-server`: a docker image running the `datalake-server` executable on
  port `1234`. It needs to be linked to a container running a postgres
  database. The `datalake-server` image enables the needed pg extensions and
  creates the tables. *NOTE*: it also populates the database with dummy data
  and is not suitable for production use.

* `datalake-webapp`: a docker image serving the `datalake-webapp` on port `80`.
  It needs to be linked to a container running the `datalake-server`
  executable.

The images are uploaded to an amazon [container registry][ecr]. An
authenticated user can retrieve the docker images as follows:

```
docker pull 906792741042.dkr.ecr.us-east-1.amazonaws.com/datalake-server:latest
docker pull 906792741042.dkr.ecr.us-east-1.amazonaws.com/datalake-webapp:latest
```

The `run.sh` script pulls the latest docker images and starts postgres,
datalake-server and datalake-webapp containers. The webapp can be accessed on
the host's machine on port `80`.

[terraform]: https://www.terraform.io/
[aws-creds]: http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html#cli-config-files
[circleci]: http://circleci.com/
[ecr]: https://aws.amazon.com/ecr/
