# stargazer2

A revival and successor to the `stargazer` R package for publication-quality
regression tables, with native support for modern econometrics packages.

## Installation

```r
# CRAN (once available)
install.packages("stargazer2")

# Development version
# install.packages("pak")
pak::pak("tomzylkin/stargazer2")
```

## Overview

`stargazer2` is a drop-in replacement for `stargazer`. For `lm` and `glm`
models the output is identical to the original. The key additions are:

- **`glm` support** — logit, probit, Poisson, and any other family/link
  combination; log likelihood and AIC reported automatically.
- **`fixest` support** — `feols`, `fepois`, `fenegbin`, `feglm`: fixed
  effects reported as indicator rows (not coefficient rows), SE type
  auto-detected from the model object.
- **`plm` support** — pooled OLS, fixed effects (`within`), random effects,
  first differences, and between estimators; FE and RE indicator rows
  auto-populated from the panel index.
- **`alpaca` support** — `feglm` via `alpaca_vcovSandwich()` and
  `alpaca_vcovCL()` helpers.
- **`vcov` argument** — pass any list of vcov matrices; the SE type is
  inferred and reported in the table note automatically. Mixed SE types
  across columns are reported by column group.
- **Style presets** — `style = "stargazer2"` (clean default),
  `"stargazer"`, `"aer"`, `"qje"`.

## Quick start

```r
library(stargazer2)

m1 <- lm(mpg ~ cyl + hp, mtcars)
m2 <- lm(mpg ~ cyl + hp + wt, mtcars)

# Text preview
stargazer(m1, m2, type = "text")

# LaTeX output (default)
stargazer(m1, m2, title = "Motor Trend Car Road Tests", label = "tab:cars")
```

### Robust and clustered standard errors

```r
library(sandwich)

stargazer(m1, m2,
          vcov = list(vcovHC(m1, type = "HC1"),
                      vcovHC(m2, type = "HC1")))
```

The table note is updated automatically: *"HC1 heteroskedasticity-robust
standard errors."*

### fixest models

```r
library(fixest)

f1 <- feols(log(Euros) ~ log(dist_km) | Origin + Destination, trade, vcov = "HC1")
f2 <- fepois(Euros     ~ log(dist_km) | Origin + Destination, trade, vcov = "HC1")

stargazer(f1, f2, type = "text")
```

Fixed effects appear as "Yes / No" indicator rows; the SE type is read
directly from the model and reported in the table note automatically.

## Credits

Original `stargazer` package by Marek Hlavac (2022). *stargazer:
Well-Formatted Regression and Summary Statistics Tables.* R package version
5.2.3. <https://CRAN.R-project.org/package=stargazer>
