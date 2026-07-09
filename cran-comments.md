## Resubmission

This is a resubmission addressing two issues flagged in the second review:

1. **References in DESCRIPTION** — added Hlavac (2022)
   <https://CRAN.R-project.org/package=stargazer> as a reference in the
   Description field, following the format requested by CRAN.

2. **Missing `\value` tag in `extract_model.Rd`** — added a `@return`
   roxygen tag to the `extract_model` generic documenting the structure
   and meaning of the returned model record list.

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
