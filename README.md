# seqmed

Causal mediation analysis for sequential mediators, using simulation-based and model-based
approaches. The methods decompose a total effect into direct and indirect components
running through one or more ordered mediators.

This repository holds the R package together with the working notes and data that support
it. The installable package lives in `seqmed/`.

## Layout

┌──────────────┬─────────────────────────────────────────────────────────────────┐
│ Path         │ Contents                                                        │
├──────────────┼─────────────────────────────────────────────────────────────────┤
│ `seqmed/`    │ The R package, with its own README, NEWS, and CRAN comments     │
│ `data/`      │ Fitted models and effect definitions used by the working notes  │
│ `METHODS.md` │ Investigation of methods for narrowing the simulation intervals │
│ `PLAN.md`    │ Variable conventions and the development plan                   │
└──────────────┴─────────────────────────────────────────────────────────────────┘

## Installation

```r
install.packages("seqmed", repos = NULL, type = "source")
```

Or from a clone, using remotes:

```r
# install.packages("remotes")
remotes::install_local("seqmed")
```

Required packages are installed automatically. They are methods, mvtnorm, parallel,
pbapply, stats, and utils. Two further packages are needed only for specific paths.
MASS supplies the confidence intervals used by `mo.med()`, and MplusAutomation is
required when `sim.med.sa()` runs in sensitivity-analysis mode.

## Functions

The package exports four functions.

┌────────────────┬─────────────────────────────────────────────────────────────────┐
│ Function       │ Purpose                                                         │
├────────────────┼─────────────────────────────────────────────────────────────────┤
│ `mo.med()`     │ Model-based mediation analysis with bootstrap confidence limits │
│ `sim.med()`    │ Simulation-based mediation analysis                             │
│ `sim.med.sa()` │ Sensitivity analysis for unmeasured confounding                 │
│ `dummy.data()` │ Fitted GLM models from simulated data, for examples and testing │
└────────────────┴─────────────────────────────────────────────────────────────────┘

```r
library(seqmed)

fits <- dummy.data()
est  <- mo.med(fits)
```

## Requirements

R 4.1.0 or later.

## Licence

AGPL-3.0-or-later. See [LICENSE](LICENSE).
