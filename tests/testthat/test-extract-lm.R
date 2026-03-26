# Numerical verification tests for extract_model.lm().
#
# Per the test plan, every extracted quantity must match the underlying model
# object to within 1e-10.  These tests run on the wage1 dataset and cover
# the four base models m1–m4 defined in the test plan, plus the vcov-override
# code paths used in Tables 3–6.

tol <- 1e-10

# ---------------------------------------------------------------------------
# Test data setup
# ---------------------------------------------------------------------------

setup_wage1 <- function() {
  skip_if_not_installed("wooldridge")
  data("wage1", package = "wooldridge")

  wage1$region <- factor(
    ifelse(wage1$northcen == 1, "northcen",
    ifelse(wage1$south    == 1, "south",
    ifelse(wage1$west     == 1, "west", "northeast"))),
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
    ifelse(wage1$ndurman  == 1, "nondurable_manuf",
    ifelse(wage1$trcommpu == 1, "transport",
    ifelse(wage1$trade    == 1, "trade",
    ifelse(wage1$services == 1, "services",
    ifelse(wage1$profserv == 1, "prof_services", "other")))))),
    levels = c("other", "construction", "nondurable_manuf",
               "transport", "trade", "services", "prof_services")
  )

  wage1
}

# ---------------------------------------------------------------------------
# Helper: check one record against the underlying lm model
# ---------------------------------------------------------------------------

check_lm_record <- function(rec, model, vcov_mat = NULL, tol = 1e-10) {
  expected_coefs <- unname(coef(model))
  if (!is.null(vcov_mat)) {
    expected_se <- unname(sqrt(diag(vcov_mat)))
  } else {
    expected_se <- unname(sqrt(diag(vcov(model))))
  }
  expected_tstat <- expected_coefs / expected_se
  expected_pval  <- 2 * pt(-abs(expected_tstat), df = df.residual(model))
  s <- summary(model)

  expect_equal(rec$coefs,  expected_coefs,  tolerance = tol)
  expect_equal(rec$se,     expected_se,     tolerance = tol)
  expect_equal(rec$tstat,  expected_tstat,  tolerance = tol)
  expect_equal(rec$pval,   expected_pval,   tolerance = tol)
  expect_equal(rec$nobs,   as.integer(nobs(model)))
  expect_equal(rec$fit$r2,     s$r.squared,     tolerance = tol)
  expect_equal(rec$fit$adj_r2, s$adj.r.squared, tolerance = tol)
  expect_equal(rec$fit$sigma,  s$sigma,         tolerance = tol)

  fs <- s$fstatistic
  if (!is.null(fs)) {
    expect_equal(rec$fit$fstat,     unname(fs["value"]),  tolerance = tol)
    expect_equal(rec$fit$fstat_df1, as.integer(fs["numdf"]))
    expect_equal(rec$fit$fstat_df2, as.integer(fs["dendf"]))
    expected_fp <- pf(fs["value"], fs["numdf"], fs["dendf"], lower.tail = FALSE)
    expect_equal(rec$fit$fstat_pval, unname(expected_fp), tolerance = tol)
  }

  expect_equal(length(rec$fixed_effects), 0L)
  expect_equal(rec$dep_var, deparse(formula(model)[[2L]]))
}

# ---------------------------------------------------------------------------
# Tests: OLS standard errors (auto-extraction)
# ---------------------------------------------------------------------------

test_that("extract_model.lm: m1 OLS SEs are exact", {
  wage1 <- setup_wage1()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  rec   <- stargazer2:::extract_model(m1)
  check_lm_record(rec, m1, tol = tol)
  expect_equal(rec$se_label, "OLS standard errors")
  expect_equal(rec$model_label, "OLS")
})

test_that("extract_model.lm: m2 OLS SEs are exact", {
  wage1 <- setup_wage1()
  m2    <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  rec   <- stargazer2:::extract_model(m2)
  check_lm_record(rec, m2, tol = tol)
})

test_that("extract_model.lm: m3 with region + occupation OLS SEs are exact", {
  wage1 <- setup_wage1()
  m3    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation, wage1)
  rec   <- stargazer2:::extract_model(m3)
  check_lm_record(rec, m3, tol = tol)
  # Non-intercept covariate names must match coef(model)
  # ("(Intercept)" is renamed to "Constant" per stargazer convention)
  expected_names <- sub("^\\(Intercept\\)$", "Constant", names(coef(m3)))
  expect_equal(rec$coef_names, expected_names)
})

test_that("extract_model.lm: m4 full model OLS SEs are exact", {
  wage1 <- setup_wage1()
  m4    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation + industry, wage1)
  rec   <- stargazer2:::extract_model(m4)
  check_lm_record(rec, m4, tol = tol)
})

# ---------------------------------------------------------------------------
# Tests: vcov override (Table 3 — HC1 robust)
# ---------------------------------------------------------------------------

test_that("extract_model.lm: HC1 vcov override gives correct SEs", {
  skip_if_not_installed("sandwich")
  wage1 <- setup_wage1()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  V     <- sandwich::vcovHC(m1, type = "HC1")
  rec   <- stargazer2:::extract_model(m1, vcov_override = V)

  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = tol)
  # Coefficients unchanged
  expect_equal(rec$coefs, unname(coef(m1)), tolerance = tol)
  # t-stats recomputed
  expect_equal(rec$tstat, unname(coef(m1)) / unname(sqrt(diag(V))), tolerance = tol)
  # SE label is non-empty (may be generic if class not detectable)
  expect_true(nchar(rec$se_label) > 0L)
})

# ---------------------------------------------------------------------------
# Tests: vcov override (Table 4 — clustered by industry)
# ---------------------------------------------------------------------------

test_that("extract_model.lm: clustered vcov override gives correct SEs", {
  skip_if_not_installed("sandwich")
  wage1 <- setup_wage1()
  m2    <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  V     <- sandwich::vcovCL(m2, cluster = ~industry, data = wage1)
  rec   <- stargazer2:::extract_model(m2, vcov_override = V)

  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = tol)
  expect_true(nchar(rec$se_label) > 0L)
})

# ---------------------------------------------------------------------------
# Tests: se override (vector path)
# ---------------------------------------------------------------------------

test_that("extract_model.lm: se vector override accepted", {
  wage1    <- setup_wage1()
  m1       <- lm(lwage ~ educ + exper + tenure, wage1)
  fake_se  <- rep(0.1, length(coef(m1)))
  rec      <- stargazer2:::extract_model(m1, se_override = fake_se)
  expect_equal(rec$se, fake_se, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Tests: p-value sign consistency
# ---------------------------------------------------------------------------

test_that("extract_model.lm: p-values consistent with t-stats", {
  wage1 <- setup_wage1()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  rec   <- stargazer2:::extract_model(m1)

  # Higher |t| must imply lower p
  ord_t <- order(abs(rec$tstat), decreasing = TRUE)
  ord_p <- order(rec$pval)
  expect_equal(ord_t, ord_p)

  # p-values in [0, 1]
  expect_true(all(rec$pval >= 0 & rec$pval <= 1))
})

# ---------------------------------------------------------------------------
# Tests: nobs matches nobs(model)
# ---------------------------------------------------------------------------

test_that("extract_model.lm: nobs matches nobs(model)", {
  wage1 <- setup_wage1()
  m3    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation, wage1)
  rec   <- stargazer2:::extract_model(m3)
  expect_equal(rec$nobs, as.integer(nobs(m3)))
})
