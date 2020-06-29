### Building

Install devtools with

```
$ R --slave -e "install.packages('devtools', repos='http://cran.us.r-project.org')"
```

Then the package can be built and installed (or reinstalled) with

```
$ R --slave -e "require(devtools); install('.')"
```

When called from the R prompt, `install('.')` will attempt to reload the
package if it was already loaded.

### Building the documentation

The documentation can be generated before installation with

```
$ R --slave -e "require(devtools); document('.')"
```

The generated documentation is kept in the github repository. It could be
removed from there at the expense of adding its generation as an additional
installation step.
