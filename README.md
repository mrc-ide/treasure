
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treasure

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/treasure/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/treasure/actions)
[![Coverage
status](https://codecov.io/gh/mrc-ide/treasure/branch/main/graph/badge.svg)](https://codecov.io/github/mrc-ide/treasure)
<!-- badges: end -->

The goal of treasure is to provide basic, generic unit-costing help for
[malariasimulation](https://mrc-ide.github.io/malariasimulation/).

Functionality is fairly minimal. A key goal of treasure is to be a
transparent and version controlled repository for unit cost estimates
that included detailed citations or referencs for cost sources.

## Installation

You can install the development version of treasure from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/treasure")
```

## Updates

Updates and improvements are encouranged via
[PRs](https://github.com/mrc-ide/treasure/pulls)!

Updates should be citable from a published source. They should be
in-line with the aim of this package to be source for generic costing
estimates and therefore should be broadly representative of the costing
for a given intervention, and not focussed on a specific location or
implementation approach.

Any suggested update should include: 1. An update to the costing
function deafault value. 2. Updated documentation for the function
including a citable source in the references section. 3. Corresponding
update to the unit tests for the modified costing function.