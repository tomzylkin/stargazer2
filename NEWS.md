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
