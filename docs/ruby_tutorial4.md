# Getting Started with RubyDatalake Part 4: Ingesting new datasets into the datalake

## Prerequisites

You've finished [part 1](ruby_tutorial1.md), [part 2](ruby_tutorial2.md),
and [part 3](ruby_tutorial3.md) of this tutorial.

## Ingesting using the command line tool

## Using the Datalake object

This walkthrough section walks through the `ingest_dataset` function in the
examples, while also making use of some helper functions in the script.

Note the `ingest_dataset` function takes an argument called `dlake`. This should
be an object of the `Datalake` class that was created in
the [first tutorial](ruby_tutorial1.md).

## Downloading a test dataset

We need a dataset to ingest. Here we use a convenience function in this script, `download`, to download a file. `download` here takes two arguments, the first is a uri for the file to download and the second is the file. 

As an example, we use a dataset of subcellular localization annotations:

```
download('http://cscoe.pfizer.com/static/austin_huang/datalake/demodata/subcellular_location.csv.zip', 'subcellular_location.csv.zip')
```

After running this command, there should be a local file called
`subcellular_location.csv.zip`.

Next we use the `ingest` method of the Datalake object to ingest the dataset:

## Ingesting a Dataset

```
  ingestResult = dlake.ingest(localFile,
                              name: make_name(),
                              tags: ['scratch', 'protein'])
```

The first argument to ingest is the name of the file to be ingested, the second
argument is what you want to name the dataset, the third argument are tags.
There are 

The localFile variable is passed as the filename, this was assigned earlier in
the script to point to the downloaded file `subcellular_location.csv.zip`.

Note we call the `make_name` helper function to assign a name to the dataset.
Ordinarily, instead of `make_name`, we would use something more descriptive such
as "subcellular-localization-dataset".

The ingest method can take additional metadata arguments, including a long-form
description field. See the [reference documentation][refdoc] for details.

## Working with the ingested dataset directly

Now that the dataset is ingested, you should be able to follow the usage of
[the previous section](ruby_tutorial3.md) to access it.

However, we can also use the return value of the `Datalake` object's `ingest`
method to `ingestResult` to work with the dataset directly.

```
  df = ingestResult.to_df{|x| x.filter(:Gene_name => ['FGR', 'TSPAN6'])}
```

As [before](ruby_tutorial3.md), we can work with the resulting ingested data
directly using server-side sequel querying and locally as a daru dataframe:

This concludes the walkthrough. You can return to
the [tutorial contents](ruby_tutorials.md).

[refdoc]: https://github.com/tweag/datalake/tree/master/RubyDatalake/README.md
