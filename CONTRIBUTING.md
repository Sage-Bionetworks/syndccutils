# Contributing to syndccutils

This outlines how to propose a change to syndccutils.

### Issues

To report bugs or request new features, open an issue on GitHub. If you’ve found
a bug, illustrate it with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  Create a Git branch, either within the syndccutils repository or in your own
   fork, for each pull request (PR). Pull requests should be opened against the
   `master` branch.
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org). 
You can use the [styler](https://CRAN.R-project.org/package=styler) 
package to apply these styles, but please don't restyle code that has nothing
to do with your PR.
*  When adding code to R or Python packages, include tests for any new functions
   or changes in existing functions' behavior. The R package uses
   [testthat](https://cran.r-project.org/package=testthat) for testing.
*  When making changes to the R package, run `devtools::document()` before
   submitting the PR. This ensures that the documentation, NAMESPACE, etc. are
   up to date with the changes you have made.
*  If you are a member of the syndccutils repository, request review from 1-2
   people on your pull requests.
*  It is okay to merge your own pull requests provided they have been approved
   by the reviewers.
