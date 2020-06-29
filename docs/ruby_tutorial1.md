# Getting Started with RubyDatalake Part 1: Installation

## Prerequisites

We assume you have ruby installed along with the `gem` installer tool available
on your system.

## Download and Package Contents

The needed data lake client software can be downloaded
from [the download page](http://cscoe.pfizer.com/static/austin_huang/datalake/).
Download the distribution corresponding to your operating system (Mac OSX,
Linux, and Windows).

For interfacing with the datalake through Ruby, only the `RubyDatalake/`
directory and ruby example file `examples/ruby_examples.rb` are referenced in this tutorial.

For information regarding other package contents (which includes a command-line
ingest tool and other language clients) See `README.txt` and details in 
the [github reference documentation](https://github.com/PfizerRD/datalake).

## Installing the Gem

Open a command line window (`CMD` on windows, `terminal` on mac), and enter the
`RubyDatalake/` directory. Run

````
gem install datalake-0.0.1.gem
````

## Try loading the Library in irb

Launch `irb`, the ruby REPL. If the installation was successful, you should be
able to load the gem contents using:

````
require 'datalake'
````

This concludes part 1. Continue on to [part 2](ruby_tutorial2.md) or return to
the [tutorial contents](ruby_tutorials.md).
