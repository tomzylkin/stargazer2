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
