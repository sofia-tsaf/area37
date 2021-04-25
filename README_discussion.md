# Discussion

---

## What characterizes a TAF analysis?

- **Modular** - the analysis is divided into well defined steps, with a separate
  script for each step.

- **Structured** - the scripts are run sequentially, the purpose of each script
  is clear, scripts are tightly linked with each other

- **Generic** - each script ends by writing out files in generic formats (often
  \*.csv and \*.png) that can be used by any software to create products, such
  as dynamic reports and interactive websites.

- **Data provenance** - the workflow describes the origin of the data, version
  controls the data, and documents in precise terms how the data were processed.

- **Reproducible** - the analysis can be run on any computer (today or after
  some years) to get the same results, using snapshots of the exact software
  version and relative paths.

- **Standardized** - TAF is widely used in hundreds of analyses in fisheries
  science every year, which invites collaboration and increases reviewability
  of the analysis.

---

## Benefits for SOFIA

The main benefits of running the SOFIA analysis in TAF are:

1. Analysis of this scope becomes **manageable** with a foundation to organize
   all input files, scripts, and output files.

2. A structured modular design makes it easier to **modify code** without
   introducing errors.

3. The structured modular design also makes it easier to read and understand the
   code, improving **reviewability**.

4. Finally, structured model design makes it easy to **compare alternative
   methods** and to see exactly what is different from the last SOFIA analysis.

5. Formal **data management** in TAF shows information about where the data came
   from and what is inside each data file.

6. TAF lends itself well to **parallel** computing, decreasing run times.

7. The workflow pipeline can be extended to generate **dynamic report** and feed
   into an **interactive website**.

8. TAF analyses are well suited for **open science**, where anyone can browse
   and download the analysis.

9. Reproducibility means we can run the SOFIA analysis on our **laptops** or
   **online servers**.

10. Reproducibility also means we can organize close **collaboration** with
    high-impact research groups in fisheries science.

---

## Opportunities

- Technical infrastructure for **Reproducible Workflows** to organize in-house
  scientific analyses. The initial focus would be on SOFIA, but a similar
  approach could benefit other aspects of FAO work.

- Online environment to make the analytical workflows available as open science,
  along with all input and output tables. This will strengthen FAO's role as the
  **Authorative Source of Information** in global fisheries and aquaculture.

- Moreover, FAO can provide an **Active Forum of Collaborative Research**
  focusing on the state of world fisheries and aquaculture. FAO will host an
  online platform where data and analyses are organized, and invite selected
  research groups to conduct their analyses on this FAO online platform.

One approach would be to have a fishery officer who will oversee the *technical
infrastructure*, *open data dissemination*, and *scientific collaboration*
related to SOFIA.

This person would have a strong background in stock assessment, statistical
computing, scientific workflow systems, project management, building
partnerships, and coordinating large user groups.
