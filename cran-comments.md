## Submission

This is a patch update (0.1.1 -> 0.1.2) to an already-accepted CRAN package.

### Summary of changes

* `stargazer()` now always prints output to the console, even when `out` is
  specified. Previously, specifying `out` suppressed console output. This
  matches the behavior of the original `stargazer` package.

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
