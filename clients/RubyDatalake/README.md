# Datalake Ruby Client

## Prerequisites for 0.02

To use Datalake with https connections, please specify the SSL_CERT_FILE variable. e.g.
export SSL_CERT_FILE=/path/to/cert.pem

## Usage

This is an example usage. Please refer to the relevant sections for in-depth
explanation.

To start with let's create a simple `csv` file that we will use for testing. We
use the [File](https://ruby-doc.org/core-2.2.0/File.html) module:

``` ruby
File.open('test_file.csv', 'w') {|f| f.write("foo,bar\n1,2\n5,6")}
File.read('test_file.csv').lines {|l| puts l}
# =>
#   foo,bar
#   1,2
#   5,6
```

Then let's load the gem and create a connection to the server. You can read
more about this in the [Installing the gem](#installing-the-gem) and [Setting
up a connection](#setting-up-a-connection) sections below.

``` ruby
irb(main):003:0> require 'datalake'
irb(main):004:0> dlake = Datalake.new
```

We can then ingest the file created above and name the source "Experiment 42".
Please refer to [Uploading a dataset](#uploading-a-dataset) for the function
arguments.

``` ruby
src = dlake.ingest('test_file.csv', name: "Experiment 42")
# => #<Datalake::Source ... name="Experiment 42", ... columns=[...]>
```

The ingest process might log the current progress:

```
Start: Starting ingest proper
Notice: Processed 2 records
Done: Starting ingest proper
Start: Creating indices for public.t195
Start: Creating primary key
Done: Creating primary key
Start: Creating index on column c1
...
Done: Creating index on column c2
Done: Creating indices for public.typed195
{
  "typed": [
    "typed195",
    "Experiment_42_v2_typed"
  ],
  "viewName": "Experiment_42_v2",
  "name": "Experiment 42",
  ...
  "columns": [
    {
      "inView": "foo",
      "name": "c1",
      "header": "foo",
      "type": "INTEGER"
    },
    {
      "inView": "bar",
      "name": "c2",
      "header": "bar",
      "type": "INTEGER"
    }
  ],
  ...
}
```

The returned object is the source, which can be turned into a
[DataFrame](http://www.rubydoc.info/gems/daru/0.1.1/Daru/DataFrame) directly.
Refer to the section [Working with dataframes](#working-with-dataframes) for more
information.

``` ruby
df = src.to_df
# => #<Daru::DataFrame(2x3)>
#       ix foo bar
#    0   1   1   2
#    1   2   5   6
```

Pre-ingested sources can be retrieved as well. Here we first load all the
sources from the server, and use the `Enumerable` function
[find](https://ruby-doc.org/core-2.2.3/Enumerable.html#method-i-find) to
retrieve "Experiment 42". Refer to the section [Working with
sources](#working-with-sources) for more information.

``` ruby
dlake.load
src = dlake.sources.find{ |s| s.name == "Experiment 42"}
# => #<Datalake::Source ... name="Experiment 42", ... columns=[...]>
df = src.to_df
# => #<Daru::DataFrame(2x3)>
#       ix foo bar
#    0   1   1   2
#    1   2   5   6
```

Since it is not always convenient to load information about all sources first,
we can specify some filtering to be performed by the server. Refer to the
section [Reading information from the
server](#reading-information-from-the-server) for more information.

``` ruby
dlake.load(nil, name: "Experiment 42")
src = dlake.sources.first
# => #<Datalake::Source ... name="Experiment 42", ... columns=[...]>
irb(main):038:0> df = src.to_df
# => #<Daru::DataFrame(2x3)>
#       ix foo bar
#    0   1   1   2
#    1   2   5   6
```

### Installing the gem

The `datalake` Ruby code is provided as a Ruby gem. See the relevant
documentation in order to get started with [rubygems](https://rubygems.org/).

**Rubygem server**

If you have access to a Rubygem server where the `datalake` gem is hosted, you
can simply run

``` bash
$ gem install datalake
```

**Gem file**

If you have access to the datalake gem file, run

``` bash
$ gem install datalake-x.x.x.gem
```

In order to obtain a `datalake-x.x.x.gem` file from source please see the
[Packaging](#packaging) section below.

**Gem dependency**

If you are writing a gem, add the following to your `.gemspec` file:

``` ruby
Gem::Specification.new do |s|

    ...

  s.add_runtime_dependency 'datalake'

    ...

end
```

**Note**: You need to make sure that the gem is available at runtime.

Once installed, the gem can then be used inside `irb` or inside your own
programs:

``` ruby
require 'datalake'
```

### Setting up a connection

There are two important classes: `Datalake` and `Datalake::Source`. The
`Datalake` class is used for configuration. Assuming that `datalake-server` is
running locally on port `3001` create an object as follows:

``` ruby
dlserver_config = {host: 'localhost', port: 3001}
dlake = Datalake.new(dlserver: dlserver_config)
```

The `Datalake` constructor can also be configured by supplying a `YAML` file:

``` ruby
dlake = Datalake.new(file: "config.yaml")
```

or through environment variables (when using environment variables, the
configuration parameters can then be omitted entirely):

``` ruby
dlake = Datalake.new
```

#### Authentication

The credentials can be provided when creating the server, through the
configuration file or through environment variables.  If credentials are
provided through any of the means listed above, the server will try to
authenticate. Upon successful authentication, all subsequent requests will be
performed as the authenticated user.

``` ruby
dlake = Datalake.new(user: "my-username", password: "my-password", verbose: true)
# => [INFO] Found credentials, authenticating
# => [INFO] Authentication successful for user "my-username"
```

If you do not which to authenticate (even if credentials are provided), you can
specify `authenticate: false` when creating the server:


``` ruby
dlake = Datalake.new(verbose: true, authenticate: false)
# => [INFO] Not authenticating
```

#### Configuration reference

The yaml configuration and/or environment variables should be set as follows:

| environment variable | yaml key        | description              |
| -------------------- | --------        | -----------              |
| `DATALAKE_HOST`      | `dlserver_host` | datalake-server endpoint |
| `DATALAKE_PORT`      | `dlserver_port` | datalake-server port     |
| `USER`               | `user`          | datalake-server username |
| `PASSWORD`           | `password`      | datalake-server password |


The arguments specified explicitely in the constructor have priority over the
yaml values. The yaml values have priority over the environment variables.

### Reading information from the server

When a `Datalake` object is created no request to the server is made. The
sources are empty:

``` ruby
dlake.sources
# => nil
```

The `load` method needs to be called for the data to be fetched from the
database. Once this is done `dlake` will contain all the metadata present in
the datalake database:

``` ruby
dlake.load
dlake.sources.length
# => 163
dlake.sources[1..5].map(&:name)
# => ["My source #1", "My source #2", "My source #3", "My source #4", "My source #5"]
```

The `load` method will load all sources into memory. There are several ways to
avoid loading all the sources. Either by specifying a `Range` as the first
parameter:


``` ruby
dlake.load(1..3)
dlake.sources.length
# => 3
dlake.load(1...3)
dlake.sources.length
# => 2
```

Or by specifying filter attributes as a hash:

``` ruby
dlake.load(nil, name: "gene_protein.json")
dlake.sources.length
# => 1
dlake.sources.first.name
# => "gene_protein.json"
dlake.load(nil, created_after: Time.now)
dlake.sources.length
# => 0
```

**Parameters for `load`:**

| Key Name               | Type                 | Ingest equivalent              |
| ---------------------- | -------------        | ----------------------------   |
| `offset`               | `String`             | `--offset`                     |
| `limit`                | `Int`                | `--limit`                      |
| `search`               | `String`             | `--search`                     |
| `ix`                   | `Int`                | `--ix`                         |
| `tags`                 | `Array` of `String`s | `--tag <foo> --tag <bar>`      |
| `description`          | `String`             | `--description`                |
| `name`                 | `String`             | `--name`                       |
| `user`                 | `String`             | `--user`                       |
| `columns`              | `Array` of `String`s | `--column <foo> --column <bar` |
| `created_after`        | `Time` or `String`   | `--created-after`              |
| `created_before`       | `Time` or `String`   | `--created-before`             |
| `include_deprecated`   | `Boolean`            | `--include-deprecated`         |

See the [datalake-server](../server) documentation for more information.

### Working with sources

Most `ingest` source fields are available:

``` ruby
require 'date'

src = dlake.sources.first
src.created
# => "2016-11-10T11:46:40.42856Z"
Date.parse(src.created).strftime('%a %d %b %Y')
# => "Thu 10 Nov 2016"
src.columns.map(&:type)
# => ["BOOLEAN", "TEXT"]
```

The source's content can be accessed. It is not cached and will be downloaded
every time the function `get_contents` is called:

``` ruby
dlake.sources.last.get_contents
# => "\"Foo\"\n1\n"
```

See the `Source` class documentation for more information.

### Uploading a dataset

The method `Datalake#ingest` is available for ingestion operations. It can be
used either with a `File` or by specifying a filepath:

``` ruby
new_src = dlake.ingest 'my_source.csv'
new_src.columns.map(&:name)
# => ["c1", "c2", "c3"]

file = File.new('my_source.csv', 'r')
new_src = dlake.ingest file
new_src.columns.map(&:name)
# => ["c1", "c2", "c3"]
```

Additionally upload parameters can be specified:

``` ruby
new_src = dlake.ingest('my_source.csv', name: "Experiment 42")
new_src.name
# => "Experiment 42"
```

All parameters are available. Most parameters can be specified in a
`camel_case` format:

``` ruby
new_src = dlake.ingest('my_source.csv', json_path: "{ length: [ _ ] }")
```

Tags can be specified as a list:

``` ruby
new_src = dlake.ingest('my_source.csv', tags: ["foo", "bar"])
```

**Upload parameters**:

| Key name            | Type                 | Ingest equivalent                    |
| ------------------  | ------------------   | ------------------------------------ |
| `input`             | `String`             | (Name of the uploaded file)          |
| `name`              | `String`             | `--name`                             |
| `file_type`         | `String`             | `--comma`/`--tab`/`--json`           |
| `peek_at`           | `Int`                | `--peek-at`                          |
| `decompress_method` | `String`             | `--unzip`                            |
| `json_path`         | `String`             | `--json-path`                        |
| `encoding`          | `String`             | `--latin1`, `--utf8`                 |
| `description`       | `String`             | `--description`                      |
| `tags`              | `Array` of `String`s | `--tag <foo> --tag <bar>`            |

See the [datalake-server](../server) documentation for more information.

### Working with dataframes

`RubyDatalake` has basic support for [daru](https://github.com/v0dro/daru).

``` ruby
dlake = Datalake::Datalake.new('localhost', 1234)
dlake.load
src = dlake.sources.first
df = src.to_df
df.nrows
# => 10004
```

The method `to_df` can also be passed a block that describes
[`sequel`](http://http://sequel.jeremyevans.net/) server-side filtering. For
instance:


``` ruby
df = src.to_df {|x| x.filter('ix > 10').filter('ix <= 25')}
df.nrows
# => 15
```

The value `x` is a `Sequel::Dataset` object equivalent to

``` ruby
x = DB.from(src.viewName)
```

All `Sequel::Dataset` operations are available.

## <a name="packaging">Packaging the gem</a>

Make sure then the `gem` command is available. Inside `RubyDatalake` run the
following command:

``` bash
$ gem build datalake.gemspec
```

This will create a file named `datalake-x.x.x.gem`. This file is self-contained
and cross-platform. This file needs to be distributed to users for them to use
the Ruby `datalake` code.
