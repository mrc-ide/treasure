
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treasure <img src="man/figures/Treasure.png" align="right" width=30% height=30% />

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/mrc-ide/treasure/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrc-ide/treasure/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mrc-ide/treasure/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/treasure)
<!-- badges: end -->

The goal of treasure is to provide basic, generic unit-costing help for
[malariasimulation](https://mrc-ide.github.io/malariasimulation/).

Functionality is minimal. The key goal of treasure is to be a
transparent and version controlled repository for unit cost estimates
that included detailed citations or references for cost sources.

:warning: Cost estimates are targeted to be as up-to-date as possible.
Retrospective costing of past programmes may therefore require
adjustment.

:warning: This is a living package and costs will be updated when new
data are available. As such, be sure to reference and use a specific
version if stable costing is required for a project.

:warning: This is not an exhaustive source of malaria programme costs.
For an example of a more complete costing exercise please see
[Patouillard et al](https://gh.bmj.com/content/2/2/e000176), and for a
recent cost and cost-effectiveness review [Conteh et
al](https://www.sciencedirect.com/science/article/pii/S1098301521001479?via%3Dihub).

:warning: When using treasure, please be sure to reference the original
sources, not just the package.

:warning: All costs are in USD\$, however the reference year for costs
may differ and costs have not be inflation-adjusted.

## Updates

Updates, reviews and improvements are encouraged via
[PRs](https://github.com/mrc-ide/treasure/pulls)!

Updates should be citable from a published source. They should be
in-line with the aim of this package to be a source for generic costing
estimates and therefore should be broadly representative of the costing
for a given intervention, and (ideally) not focussed on a specific
location or implementation approach.

Any suggested update via a PR should include: 1. An update to the
costing function default value. 2. Updated documentation for the
function including a citable source in the references section. 3.
Corresponding update to the unit tests for the modified costing
function.

## Installation

You can install the development version of treasure from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mrc-ide/treasure")
```
