# Regression tests for fixest SE-type labeling across fixest versions.
#
# Background: the SE-type string ("IID", "Clustered (region)", ...) is recorded
# in different places depending on the fixest version:
#   - fixest >= 0.14.0: attached as 'vcov_type' to the matrix from vcov().
#   - fixest <= 0.13.x: attached as 'type' to summary(model)$coeftable; vcov()
#     returns a bare matrix.
# Reading only vcov()'s attribute mislabeled clustered SEs as "OLS" on older
# fixest. fixest_vcov_type() reads whichever source is populated; these tests
# lock in both paths plus the label mapping, and verify behavior against the
# installed fixest.

# ---------------------------------------------------------------------------
# Label mapping: se_label_from_fixest_type() (version-independent)
# ---------------------------------------------------------------------------

test_that("se_label_from_fixest_type maps fixest type strings to notes", {
  f <- stargazer2:::se_label_from_fixest_type

  expect_identical(f("IID", method = "feols"),    "OLS standard errors")
  expect_identical(f("IID", method = "fepois"),   "MLE standard errors")
  expect_identical(f("IID", method = "fenegbin"), "MLE standard errors")

  expect_identical(f("Clustered (region)"),
                   "standard errors clustered by region")
  expect_identical(f("Clustered (Origin & Destination)"),
                   "standard errors clustered by Origin and Destination")

  expect_identical(f("Heteroskedasticity-robust"),
                   "heteroskedasticity-robust standard errors")
  expect_identical(f("Heteroskedasticity-robust", vcov_call = "HC1"),
                   "HC1 heteroskedasticity-robust standard errors")
})

test_that("se_label_from_fixest_type falls back to IID label when type absent", {
  f <- stargazer2:::se_label_from_fixest_type
  # A missing type must NOT be silently treated as a non-default SE.
  expect_identical(f(NULL, method = "feols"), "OLS standard errors")
  expect_identical(f("",   method = "feols"), "OLS standard errors")
})

# ---------------------------------------------------------------------------
# Version dispatch: fixest_vcov_type() reads the right source per layout.
# Mock objects reproduce each fixest layout so both paths are covered even
# when only one fixest version is installed.
# ---------------------------------------------------------------------------

test_that("fixest_vcov_type reads matrix attr (fixest >= 0.14 layout)", {
  V <- diag(2)
  attr(V, "vcov_type") <- "IID"
  mod <- structure(list(), class = "mock_fixest_new")
  registerS3method("vcov", "mock_fixest_new",
                   function(object, ...) V, envir = environment())
  registerS3method("summary", "mock_fixest_new",
                   function(object, ...) list(coeftable = matrix(0, 2, 2)),
                   envir = environment())

  expect_identical(stargazer2:::fixest_vcov_type(mod), "IID")
})

test_that("fixest_vcov_type falls back to summary attr (fixest <= 0.13 layout)", {
  Vbare <- diag(2)                      # no vcov_type attribute (old vcov())
  ct <- matrix(0, 2, 2)
  attr(ct, "type") <- "Clustered (region)"
  mod <- structure(list(), class = "mock_fixest_old")
  registerS3method("vcov", "mock_fixest_old",
                   function(object, ...) Vbare, envir = environment())
  registerS3method("summary", "mock_fixest_old",
                   function(object, ...) list(coeftable = ct),
                   envir = environment())

  expect_identical(stargazer2:::fixest_vcov_type(mod), "Clustered (region)")
})

# ---------------------------------------------------------------------------
# Integration: real fixest, explicit vcov (deterministic across versions).
# ---------------------------------------------------------------------------

test_that("auto-extracted clustered feols is labeled as clustered, not OLS", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("wooldridge")
  data("wage1", package = "wooldridge")
  wage1$region <- factor(
    ifelse(wage1$northcen == 1, "northcen",
    ifelse(wage1$south    == 1, "south",
    ifelse(wage1$west     == 1, "west", "northeast")))
  )
  m <- fixest::feols(lwage ~ educ + exper | region, wage1, vcov = ~region)
  rec <- stargazer2:::extract_model(m)
  expect_identical(rec$se_label, "standard errors clustered by region")
})

test_that("auto-extracted IID feols is labeled as OLS standard errors", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("wooldridge")
  data("wage1", package = "wooldridge")
  m <- fixest::feols(lwage ~ educ + exper, wage1, vcov = "iid")
  rec <- stargazer2:::extract_model(m)
  expect_identical(rec$se_label, "OLS standard errors")
})
