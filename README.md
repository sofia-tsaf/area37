# area37

Experiment with Rishi, demonstrating how the SOFIA analysis for Area 37
(Mediterranean and Black Sea) can run in TAF.

## How to run

Install the TAF package from CRAN.

Then open R in the `area37` directory and run:

```
library(TAF)
taf.bootstrap()
sourceAll()
```

The `sourceAll` function runs the TAF scripts sequentially in alphabetical
order:

```
data.R
model.R
output.R
report.R
```

An alternative to `sourceAll` is the `makeAll` function, which omits TAF scripts
that have already been run.

## Dependencies

This analysis uses the [sraplus](https://github.com/DanOvando/sraplus) package,
which has many underlying package [dependencies](README_dependencies.md). These are all
installed automatically with one command:

```
library(remotes)
install_github("DanOvando/sraplus")
```

## Explore results

The results from each script appear in the corresponding working subdirectory.
For example, the `data.R` script writes results into a folder called `data`.

Since this analysis takes around 1 hour to run, the results are made available
as release assets `area37_results_*.zip` (~200 MB) on the
[releases](https://github.com/arni-magnusson/area37/releases) page.

## Learn more

The [package help page](https://rdrr.io/cran/TAF/man/TAF-package.html) shows a
complete list of R functions in the TAF package. At the bottom of the package
help page there is also a list of references:

- ICES Transparent Assessment Framework: https://taf.ices.dk.
- To explore example TAF stock assessments, see the introductory
  [video](https://www.youtube.com/watch?v=FweJbr9hfdY) and
  [tutorial](https://github.com/ices-taf/doc/tree/master/tutorial-1/README.md).
- The [TAF Wiki](https://github.com/ices-taf/doc/wiki) provides additional help
  resources.

In an R session, the package help page can be brought up with the command
`?TAF`.

## Discussion points

See [README_discussion.md](README_discussion.md).
