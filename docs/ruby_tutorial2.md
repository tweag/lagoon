# Getting Started with RubyDatalake Part 2: Connecting to the Datalake

## Prerequisites

I assume you have successfully downloaded the datalake software and successfully
installed the RubyDatalake gem. If not, go through [part 1](./ruby_tutorial1.md)

## Opening a connection to the Datalake

In this tutorial, we walk through some of the code from
`examples/ruby_examples.rb`, specifically the code to setup the connection and
the `pull_dataset` function which demonstrates how to work with existing data in
the datalake. The [next tutorial](./ruby_tutorial3.md) will walk through
ingesting datasets using ruby.

First, we load the datalake gem.

```
require 'datalake'
```

First we describe the connection properties using a dictionary variable:

```
dlserver_config = {host: 'bichkd-t440p.pfizer.com', port: 22089}
```

`dl_server_config` is a basic ruby map with properties pointing to the REST
service. We've pointed it to the internal datalake instance we're using for
testing, but we can change the host to correspond to other internal datalake
instances if they're available.

Next we instantiate a Datalake object using these properties:

```
dlake = Datalake.new(dlserver: dlserver_config)
```

Alternatively, a yaml file can be used to specify the host properties
(see [reference documentation][refdoc] under "Setting up a connection" for
details).

Finally we load information on available datasources by calling the load method to our
newly created Datalake object.

```
dlake.load
```

This concludes part 2. Continue on to [part 3](ruby_tutorial3.md) or return to
the [tutorial contents](ruby_tutorials.md).

[refdoc]: https://github.com/tweag/datalake/tree/master/RubyDatalake/README.md
