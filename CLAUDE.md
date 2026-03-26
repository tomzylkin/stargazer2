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

## Standard Error Interface

`stargazer2` accepts standard errors via three mechanisms, applied in this order of precedence:

1. **`vcov` argument** — a list of variance-covariance matrices (one per model). `stargazer2` extracts the square root of the diagonal internally. Most flexible — works with any vcov estimator that returns a matrix.

2. **`se` argument** — a list of numeric vectors of standard errors (one per model), exactly as in the original `stargazer`. For drop-in compatibility with existing scripts.

3. **Auto-extraction** — if neither `vcov` nor `se` is supplied, `stargazer2` reads SE information directly from the model object where available (`fixest`, `alpaca`). Falls back to classical OLS SEs for `lm` objects.

Table notes should always reflect the SE type actually used, whether supplied by the user or extracted automatically.

## Test Plan

### Test Dataset

Use the `wage1` dataset from the `wooldridge` package. Before running any models, construct categorical variables from the existing dummy variables:

```r
library(wooldridge)
data(wage1)

wage1$region <- factor(
  ifelse(wage1$northcen == 1, "northcen",
  ifelse(wage1$south == 1, "south",
  ifelse(wage1$west == 1, "west", "northeast"))),
  levels = c("northeast", "northcen", "south", "west")
)

wage1$occupation <- factor(
  ifelse(wage1$profocc == 1, "professional",
  ifelse(wage1$clerocc == 1, "clerical",
  ifelse(wage1$servocc == 1, "service", "other"))),
  levels = c("other", "professional", "clerical", "service")
)

wage1$industry <- factor(
  ifelse(wage1$construc == 1, "construction",
  ifelse(wage1$ndurman == 1, "nondurable_manuf",
  ifelse(wage1$trcommpu == 1, "transport",
  ifelse(wage1$trade == 1, "trade",
  ifelse(wage1$services == 1, "services",
  ifelse(wage1$profserv == 1, "prof_services", "other")))))),
  levels = c("other", "construction", "nondurable_manuf",
             "transport", "trade", "services", "prof_services")
)
```

### Base Models

All tables in Step 1 use these four models unless otherwise noted:

```r
m1 <- lm(lwage ~ educ + exper + tenure, wage1)
m2 <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
m3 <- lm(lwage ~ educ + exper + tenure + female + married +
          region + occupation, wage1)
m4 <- lm(lwage ~ educ + exper + tenure + female + married +
          region + occupation + industry, wage1)
```

### Step 1: Core Correctness

Do not proceed to Step 2 until all Step 1 tables pass review.
If any table requires a code fix, restart from Table 1.

**Table 1** — Original stargazer, lm only, default options:
```r
library(stargazer)
stargazer(m1, m2, m3, m4)
```

**Table 2** — stargazer2, same inputs, default options:
```r
stargazer(m1, m2, m3, m4)
```
Target: output should match Table 1 exactly for lm objects.

**Table 3** — stargazer2, robust standard errors via vcov:
```r
library(sandwich)
stargazer(m1, m2, m3, m4,
          vcov = list(vcovHC(m1, type = "HC1"),
                      vcovHC(m2, type = "HC1"),
                      vcovHC(m3, type = "HC1"),
                      vcovHC(m4, type = "HC1")))
```

**Table 4** — stargazer2, clustered standard errors by industry:
```r
stargazer(m1, m2, m3, m4,
          vcov = list(vcovCL(m1, cluster = ~industry, data = wage1),
                      vcovCL(m2, cluster = ~industry, data = wage1),
                      vcovCL(m3, cluster = ~industry, data = wage1),
                      vcovCL(m4, cluster = ~industry, data = wage1)))
```

**Table 5** — stargazer2, mixed SE types (column 1 robust, columns 2–4 clustered by industry):
```r
stargazer(m1, m2, m3, m4,
          vcov = list(vcovHC(m1, type = "HC1"),
                      vcovCL(m2, cluster = ~industry, data = wage1),
                      vcovCL(m3, cluster = ~industry, data = wage1),
                      vcovCL(m4, cluster = ~industry, data = wage1)))
```

**Table 6** — stargazer2, models 3 and 4 re-estimated via feols with robust SEs, models 1 and 2 remain lm with robust SEs via vcov:
```r
m3_fe <- feols(lwage ~ educ + exper + tenure + female + married |
               region + occupation, wage1)
m4_fe <- feols(lwage ~ educ + exper + tenure + female + married |
               region + occupation + industry, wage1,
               vcov = "HC1")

stargazer(m1, m2, m3_fe, m4_fe,
          vcov = list(vcovHC(m1, type = "HC1"),
                      vcovHC(m2, type = "HC1"),
                      NULL,
                      NULL))
```
`NULL` entries signal `stargazer2` to auto-extract SEs from the feols objects.

**Table 7** — stargazer2, feols only, multiple FE combinations including interacted FEs, clustered by region x industry:
```r
f1 <- feols(lwage ~ educ + exper + tenure + female + married |
            region, wage1, vcov = ~region^industry)
f2 <- feols(lwage ~ educ + exper + tenure + female + married |
            occupation, wage1, vcov = ~region^industry)
f3 <- feols(lwage ~ educ + exper + tenure + female + married |
            region + occupation, wage1, vcov = ~region^industry)
f4 <- feols(lwage ~ educ + exper + tenure + female + married |
            region + occupation + industry, wage1, vcov = ~region^industry)
f5 <- feols(lwage ~ educ + exper + tenure + female + married |
            region^industry, wage1, vcov = ~region^industry)

stargazer(f1, f2, f3, f4, f5)
```
This tests FE indicator rows with interacted fixed effects (`region^industry` should appear as a single FE row labeled "Region x Industry FE").

### Numerical Verification

For every table in every step, `stargazer2` output must be verified against the underlying model objects directly — not just against stargazer formatting. Specifically:

- Coefficients in the table must match `coef(model)` exactly
- Standard errors in the table must match:
  - `sqrt(diag(vcov(model)))` for default lm SEs
  - `sqrt(diag(supplied_vcov_matrix))` when `vcov` argument is used
  - the SE slot of the fixest object when auto-extracted
- t-statistics must match coefficients / standard errors exactly
- p-values must be consistent with t-statistics and degrees of freedom
- N must match `nobs(model)`
- R² must match `summary(model)$r.squared` for lm objects
- For fixest objects, fit statistics must match `fitstat()` output

Claude Code should write explicit numerical comparison tests using `testthat` that check these quantities to within a tolerance of 1e-10, not just visually inspect the output.

### Step 2: Formatting Consistency

Iterate on Tables 1 and 2 to verify that all formatting options available in the original `stargazer` produce consistent output in `stargazer2` for lm objects. Options to test include: `title`, `label`, `column.labels`, `dep.var.labels`, `covariate.labels`, `omit`, `keep`, `digits`, `star.cutoffs`, `notes`, `font.size` (LaTeX).

Do not proceed to Step 3 until Step 2 passes review.

### Step 3: Formatting Consistency for fixest

Iterate on Table 7 to verify that all formatting options tested in Step 2 produce analogously structured output for feols models. Output need not be identical to Step 2 (fixest-specific features like FE rows have no lm equivalent) but should be visually consistent and follow the same conventions.

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
