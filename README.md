# Pfizer datalake

This pilot creates processes to accelerate consumption and deployment data
sources and services. It implements processes with lightweight interfaces to
automatically ingest, build services, and retrieve data for far less
time and effort compared to currently available methods.

These capabilities use REST services that can be consumed by any modern
analytics technology stack (R, Python, Javascript, Ruby, Java, etc.).

Benefits of this workflow include:

* New analyses and web apps can incorporate previously-obtained contextual
information with minimal effort.

* Multiple projects benefit from incorporating a deeper and richer degree of
  biological context drawn from previous efforts.

* Automating collection of shared metadata enables a shared community data
  ecosystem.

For additional details, see the [download page][downloadpage] and [pilot
document][pilotdoc].

Pfizer team contact: [Austin Huang](mailto: austin.huang@pfizer.com)

## What's included

The project includes the following components -

### For Analysts:

* **[RDatalake](./RDatalake/):** library for importing data and writing
  queries from from R, using familiar domain specific languages such as
  [`dplyr`][dplyr]. This is an installable R package.
* **[MatDatalake](./MatDatalake/):** Experimental Matlab library for
  importing data and writing queries from Matlab.
* **[PyDatalake](./PyDatalake/):** Experimental Python library for
  importing and manipulating data, leveraging
  [pandas](http://pandas.pydata.org/) and
  [SQLAlchemy](http://www.sqlalchemy.org/) capabilities.
* **[RubyDatalake](./RubyDatalake/):** Experimental Ruby library for
  importing and manipulating data, leveraging
  [daru](https://github.com/v0dro/daru) and
  [sequel](http://sequel.jeremyevans.net/) capabilities.

### For Administrators:

* **[datalake-server](./server/):** executable serving ingested data in a JSON
  format.
* **[datalake-webapp](./webapp/):** webapp for browsing datasets.

For details regarding code, installation and setup, see [developer documentation here](HACKING.md).

[downloadpage]: http://cscoe.pfizer.com/static/austin_huang/datalake/
[dplyr]: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html
[pilotdoc]:http://cscoe.pfizer.com/austin_huang/ds_proposal_2016/austin_huang_pilot.pdf

