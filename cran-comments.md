## Submission

This is a feature update (0.1.0 -> 0.1.1) to an already-accepted CRAN
package.

### Summary of changes

* Added native support for `plm` panel-data models (`within`/FE,
  `random`/RE, `pooling`, `fd`, `between`): fixed- and random-effects
  reported as indicator rows, with SE type auto-detected, including
  `plm`-specific `vcovHC`/`vcovSCC`/`vcovBK`/`vcovDC` estimators.

* Added a `df` argument to `stargazer()` to optionally suppress the
  `(df = ...)` suffixes on the Residual Std. Error and F Statistic rows.

* `stargazer()` now detects correlation matrices (square, symmetric, 1s
  on the diagonal) and renders them as an N x N grid in both `"text"` and
  `"latex"` output.

No breaking changes; all existing arguments and output are unaffected.

## R CMD check results

0 errors | 0 warnings | 1 note

### Notes

1. **unable to verify current time** — WSL2 system clock issue on the
   development machine; not reproducible on CRAN servers.

### Local check caveat

This development machine lacks `pdflatex`/`texi2dvi`/`qpdf`/`tidy`, so a
local `R CMD check --as-cran` additionally reports a PDF-manual ERROR and
two WARNINGs (PDF manual build, HTML validation) that are artifacts of the
missing toolchain, not package issues — all Rd, test, vignette, and
example checks pass. These are expected to be absent on CRAN's build
servers, consistent with the previous submission.

### Suggested packages and archived status

`alpaca` is listed in `Suggests` and is currently archived on CRAN. All code
paths that use `alpaca` are guarded with `requireNamespace("alpaca",
quietly = TRUE)`, and the vignette section that uses it is similarly guarded
so the package builds and checks cleanly without `alpaca` installed.

### Downstream dependencies

`stargazer2` has no reverse dependencies on CRAN.
