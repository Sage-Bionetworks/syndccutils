# Synapse DCC Utilities for R

| **Service** | **Master** | **Develop** |
|:-------------:|:------:|:-------:|
| CI Status | [![Travis-CI Build Status](https://travis-ci.org/Sage-Bionetworks/syndccutils.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/syndccutils) | [![Travis-CI Build Status](https://travis-ci.org/Sage-Bionetworks/syndccutils.svg?branch=develop)](https://travis-ci.org/Sage-Bionetworks/syndccutils) |
| Test Coverage | *pending* | *pending* |

This is the R version of the `syndccutils` package.

Code for managing data coordinating operations (e.g., development of the CSBC/PS-ON Knowledge Portal and individual Center pages) for Sage-supported communities through Synapse. Currently this package is able to:
* Build tables and charts to summarize annotations from a view
* Build a DCC dashboard to summarize multiple projects
* Provide tools/scripts to handle metadata management (e.g to/from GEO)
* Pull publications from PubMed
* Build projects
* Invite members to a team

What to see examples? Go to [config/targets.md](config/targets.md)

## Installation

```r
devtools::install_github("Sage-Bionetworks/syndccutils", subdir = "R")
```

## Getting Started
There are a few ideas to get started using this package

### Building visualizations from a file view
Here we can use a file view with annotations to create multiple charts of visualizations

### Summarizing projects by dashboard
Here we can summarize activity by project, date added, data type, etc.

### Data tracking for internal consortia spaces
Data available preceding a public data release is subject to change. Here we can note files added, deleted and changes to file metadata.
