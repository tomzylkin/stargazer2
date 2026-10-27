# stargazer2 0.1.2

## Bug fixes

* `stargazer()` now always prints output to the console, even when `out` is
  specified. Previously, specifying `out` suppressed console output. This
  matches the behavior of the original `stargazer` package.

# stargazer2 0.1.1

## New features

* Native support for `plm` models (`within`/FE, `random`/RE, `pooling`,
  `fd`, `between`): fixed- and random-effects reported as indicator rows
  (e.g. "Firm RE"), with SE type auto-detected, including `plm`-specific
  `vcovHC`/`vcovSCC`/`vcovBK`/`vcovDC` estimators (Arellano, Driscoll-Kraay,
  Beck-Katz, double-clustered, etc.).

* `df` argument to `stargazer()`: set `df = FALSE` to suppress the
  `(df = ...)` suffixes on the Residual Std. Error and F Statistic rows,
  matching original `stargazer` behavior.

* Correlation matrices (square, symmetric, with 1s on the diagonal) passed
  to `stargazer()` are now detected automatically and rendered as an N x N
  grid, matching original `stargazer` behavior, in both `"text"` and
  `"latex"` output.

# stargazer2 0.1.0

Initial CRAN release.

## New features

* Drop-in replacement for the `stargazer` package for `lm` objects; output
  is identical to the original for default settings.

* Native support for `fixest` models (`feols`, `fepois`, `fenegbin`,
  `feglm`): fixed effects reported as indicator rows, SE type auto-detected
  from the model object.

* Native support for `alpaca::feglm` models via companion vcov helpers
  (`alpaca_vcovSandwich`, `alpaca_vcovCL`).

* `vcov` argument accepts a list of variance-covariance matrices (one per
  model); SE type is inferred from the matrix class and reported in the
  table note. Mixed SE types across columns are reported by column group.

* Three output formats: `"latex"` (default), `"text"`, `"html"`.

* Four table style presets via `style =`: `"stargazer2"` (clean default,
  single `\hline`), `"stargazer"` (matches original package exactly),
  `"aer"` (American Economic Review), `"qje"` (Quarterly Journal of
  Economics).

* Summary statistics tables from data frames and matrices.
