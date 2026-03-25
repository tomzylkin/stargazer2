# stargazer2

A revival and successor to the `stargazer` R package for results reporting.

## Project Overview

**Goal:** A CRAN-distributable R package that produces publication-quality results tables from modern econometrics packages.

**Output formats:** LaTeX (priority), ASCII text (priority), HTML.

**Main function:** `stargazer()` — intentional drop-in replacement for the original `stargazer` package. Users should be able to swap packages and have existing scripts work with minimal changes.

## Supported Packages

**Initial:**
- `fixest` — `feols`, `feglm`, `fepois`
- `alpaca` — `feglm`

**Planned expansion:**
- Staggered diff-in-diff: `did`, `staggered`, `did2s`, `eventstudyr`

## Target Users

Economists and social scientists familiar with the original `stargazer` package.

## Developer Background

PhD economist, 10+ years R experience. Has written and published R and Stata packages. Familiar with C++ for R extensions.

## Package Conventions

### Dependencies
- **Base R preferred** for internal code — minimize external dependencies
- External dependencies limited to packages already likely present in an economist's R environment (e.g., `fixest`, `alpaca` as `Suggests`, not `Imports`)
- Do **not** import tidyverse packages

### Style
- Follow tidyverse style conventions for **naming and formatting only**: `snake_case`, spaces around operators
- All user-facing functions documented with `roxygen2`
- Use `testthat` for unit tests

### CRAN Compliance
Write code as if it will be submitted to CRAN from the start.

## Internal Architecture

Three strictly separated layers. New package support requires only writing a new extraction method — formatting and rendering are untouched.

1. **Model extraction** — pulls coefficients, SEs, and statistics from model objects
2. **Table formatting** — assembles rows, columns, and alignment
3. **Output rendering** — converts formatted table to LaTeX / ASCII / HTML

## Design Principle

Beautiful LaTeX tables with a single function call, just like the original `stargazer`. **Familiarity and simplicity are core values.**

## Key Design Decisions

### 1. Fixed Effects Reporting

- Fixed effects should **never** appear as coefficient rows
- Instead, report FEs as indicator rows at the bottom of the table (before fit statistics), one row per unique FE variable across all models, with Yes/No entries per column
- Extract FE information from fixest's `fixef_vars` slot

### 2. Standard Error Reporting

- Always display the SE type actually used in estimation; do not assume classical OLS standard errors
- Add a table note identifying the SE type, e.g. "Robust standard errors in parentheses" or "Standard errors clustered by X"
- User can override SE type display via a function argument
- Extract SE type from fixest's `se_type` and `cluster` slots

### 3. First Test Case

Use Laurent Berge's first fixest walkthrough example as the canonical test, which produces a 3-column table:

```r
library(fixest)
data(trade)
gravity_ols     = feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year, trade)
gravity_pois    = fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)
gravity_negbin  = fenegbin(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)
```

`stargazer(gravity_ols, gravity_pois, gravity_negbin)` should produce a clean LaTeX table with:

- One column per model
- Coefficient rows for `log(dist_km)`
- FE indicator rows for Origin, Destination, Product, Year
- SE type note appropriate to each model
- Fit statistics (N, R² or equivalent per model type)
- Column headers identifying the estimator used
