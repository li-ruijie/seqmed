# seqmed

Causal mediation analysis for sequential mediators using simulation-based and model-based approaches.

## Installation

Install from a local clone:

```r
install.packages("path/to/seqmed", repos = NULL, type = "source")
```

Or using devtools/remotes:

```r
# install.packages("devtools")
devtools::install("path/to/seqmed")
```

### Dependencies

Required packages (installed automatically):

- methods, mvtnorm, parallel, pbapply, stats, utils

Optional packages (install manually if needed):

- **MASS** — for `mo.ci()` confidence intervals
- **MplusAutomation** — for sensitivity analysis mode (`s.mode = "sa"`)

## Usage

The package exports four functions:

- `dummy.data()` — generate fitted GLM models from simulated data for examples and testing
- `mo.med()` — model-based mediation analysis with bootstrap CIs
- `sim.med()` — simulation-based mediation analysis
- `sim.med.sa()` — sensitivity analysis for simulation-based mediation
