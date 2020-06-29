# Getting Started with RubyDatalake Part 3: Using Datasets in the Datalake

## Prerequisites

You've finished [part 1](ruby_tutorial1.md) and [part 2](ruby_tutorial2.md) of
the documentation.

## META COMMENT

Selecting the dataset by their postgres viewname is a bit of an inconvenience
relative to the R api (where you can get datasets by their name). The view name
is printed when you use the command line tool. It usually looks the same as the
name of the dataset except various whitespace characters turn into underscore
and the version number is appended (as in the example below)

Working on smoothing this out currently.

## Loading and filtering data from the datalake

Next we look at the `pull_dataset` function

````
  requestTable = "compsci01_Protein_Atlas___Subcellular_Localization_of__v9"
  selected = dlake.sources.select{ |x| x.viewName() == requestTable }
````

````
  raise "Should have exactly one table which matches requested viewName" \
    unless selected.length == 1
  # Pull out entries in the table as local data frame via sequel
  df = selected.first.to_df{|x| x.filter(:Gene_name => ['FGR', 'TSPAN6'])}
````

This concludes part 3. Continue on to [part 4](ruby_tutorial4.md) or return to
the [tutorial contents](ruby_tutorials.md).


