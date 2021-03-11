# sofia24

Experiment with Rishi, demonstrating how the SOFIA analysis for the
Mediterranean and Black Sea can run in TAF.

## How to run

Install the icesTAF package from CRAN.

Then open R in the `area37` directory and run:

```
library(icesTAF)
taf.bootstrap()
sourceAll()
```

## Explore results

The results from each script appear in the corresponding working subdirectory.
For example, the `data.R` script writes results into a folder called `data`.

The scripts are run sequentially, in alphabetical order, so the `model.R` script
reads from the `data` folder and writes into the `model` folder.
