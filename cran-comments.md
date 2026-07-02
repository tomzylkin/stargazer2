## Resubmission

This is a resubmission addressing two issues flagged in the initial review:

1. **`.claude` directory** — excluded from the built package via `.Rbuildignore`.

2. **No prebuilt vignette index / missing `inst/doc`** — Pandoc is now
   installed; vignettes are pre-built and `inst/doc/` is included in the
   source package.

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes

1. **New submission** — this is the first submission of `stargazer2` to CRAN.

2. **unable to verify current time** — WSL2 system clock issue on the
   development machine; not reproducible on CRAN servers.

### Suggested packages and archived status

`alpaca` is listed in `Suggests` and is currently archived on CRAN. All code
paths that use `alpaca` are guarded with `requireNamespace("alpaca",
quietly = TRUE)`, and the vignette section that uses it is similarly guarded
so the package builds and checks cleanly without `alpaca` installed.

### Downstream dependencies

This is a new package with no reverse dependencies.
