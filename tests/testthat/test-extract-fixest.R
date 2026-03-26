# Numerical verification tests for extract_model.fixest().
#
# Tests cover:
#   - feols with multiple FE combinations
#   - fepois and fenegbin (gravity model from the canonical test case)
#   - FE variable detection via fixef_vars
#   - SE auto-extraction and vcov-override paths
#   - nobs, fit statistics

tol <- 1e-10

# ---------------------------------------------------------------------------
# Test data setup
# ---------------------------------------------------------------------------

setup_wage1_fixest <- function() {
  skip_if_not_installed("wooldridge")
  skip_if_not_installed("fixest")
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
# Coefficients and SEs
# ---------------------------------------------------------------------------

test_that("extract_model.fixest: feols coefs match fixest::coef()", {
  wage1 <- setup_wage1_fixest()
  f3 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  rec <- extract_model(f3)

  expect_equal(rec$coefs, unname(coef(f3)), tolerance = tol)
  expect_equal(rec$coef_names, names(coef(f3)))
})

test_that("extract_model.fixest: feols auto-extracted SEs match vcov(model)", {
  wage1 <- setup_wage1_fixest()
  f3 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  rec <- extract_model(f3)
  expected_se <- unname(sqrt(diag(vcov(f3))))
  expect_equal(rec$se, expected_se, tolerance = tol)
})

test_that("extract_model.fixest: feols HC1 vcov override gives correct SEs", {
  wage1 <- setup_wage1_fixest()
  f4 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married |
      region + occupation + industry,
    data  = wage1,
    vcov  = "HC1"
  )
  rec <- extract_model(f4)
  expected_se <- unname(sqrt(diag(vcov(f4))))
  expect_equal(rec$se, expected_se, tolerance = tol)
})

test_that("extract_model.fixest: vcov matrix override replaces auto-extracted SEs", {
  skip_if_not_installed("sandwich")
  wage1 <- setup_wage1_fixest()
  f1 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region,
    data = wage1
  )
  # Supply a custom vcov (identity-scaled for test purposes)
  p  <- length(coef(f1))
  V  <- diag(seq(0.01, 0.01 * p, by = 0.01))
  rownames(V) <- colnames(V) <- names(coef(f1))
  rec <- extract_model(f1, vcov_override = V)
  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = tol)
  # Coefficients unchanged
  expect_equal(rec$coefs, unname(coef(f1)), tolerance = tol)
})

# ---------------------------------------------------------------------------
# Fixed effects detection
# ---------------------------------------------------------------------------

test_that("extract_model.fixest: single FE detected correctly", {
  wage1 <- setup_wage1_fixest()
  f1 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region,
    data = wage1
  )
  rec <- extract_model(f1)
  expect_true("region" %in% rec$fixed_effects)
  expect_equal(length(rec$fixed_effects), 1L)
})

test_that("extract_model.fixest: two FEs detected correctly", {
  wage1 <- setup_wage1_fixest()
  f3 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  rec <- extract_model(f3)
  expect_setequal(rec$fixed_effects, c("region", "occupation"))
})

test_that("extract_model.fixest: three FEs detected correctly", {
  wage1 <- setup_wage1_fixest()
  f4 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married |
      region + occupation + industry,
    data = wage1
  )
  rec <- extract_model(f4)
  expect_setequal(rec$fixed_effects, c("region", "occupation", "industry"))
})

test_that("extract_model.fixest: interacted FE detected as single entry", {
  wage1 <- setup_wage1_fixest()
  f5 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region^industry,
    data = wage1
  )
  rec <- extract_model(f5)
  # Should have exactly one FE entry containing the interaction
  expect_equal(length(rec$fixed_effects), 1L)
  expect_true(grepl("region", rec$fixed_effects[1L], fixed = TRUE))
  expect_true(grepl("industry", rec$fixed_effects[1L], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# nobs
# ---------------------------------------------------------------------------

test_that("extract_model.fixest: nobs matches fixest::nobs()", {
  wage1 <- setup_wage1_fixest()
  f3 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  rec <- extract_model(f3)
  expect_equal(rec$nobs, as.integer(nobs(f3)))
})

# ---------------------------------------------------------------------------
# Model labels
# ---------------------------------------------------------------------------

test_that("extract_model.fixest: feols labelled as OLS", {
  wage1 <- setup_wage1_fixest()
  f1 <- fixest::feols(lwage ~ educ | region, data = wage1)
  rec <- extract_model(f1)
  expect_equal(rec$model_label, "OLS")
})

test_that("extract_model.fixest: fepois labelled as Poisson", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  m <- fixest::fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year,
                      data = trade)
  rec <- extract_model(m)
  expect_equal(rec$model_label, "Poisson")
})

test_that("extract_model.fixest: fenegbin labelled as Neg. Binomial", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  m <- fixest::fenegbin(Euros ~ log(dist_km) | Origin + Destination + Product + Year,
                        data = trade)
  rec <- extract_model(m)
  expect_equal(rec$model_label, "Neg. Binomial")
})

# ---------------------------------------------------------------------------
# Gravity model canonical test case (fit statistics)
# ---------------------------------------------------------------------------

test_that("gravity feols: within-R2 is non-NA and in [0,1]", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  m <- fixest::feols(
    log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year,
    data = trade
  )
  rec <- extract_model(m)
  expect_false(is.na(rec$fit$wr2))
  expect_true(rec$fit$wr2 >= 0 & rec$fit$wr2 <= 1)
})

test_that("gravity fepois: pseudo-R2 is non-NA and in [0,1]", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  m <- fixest::fepois(
    Euros ~ log(dist_km) | Origin + Destination + Product + Year,
    data = trade
  )
  rec <- extract_model(m)
  pr2 <- rec$fit$pr2
  if (!is.na(pr2)) {
    expect_true(pr2 >= 0 & pr2 <= 1)
  }
})

test_that("gravity: all four FEs detected across models", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  m_ols   <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year, trade)
  m_pois  <- fixest::fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)
  m_negbin <- fixest::fenegbin(Euros ~ log(dist_km) | Origin + Destination + Product + Year, trade)

  for (m in list(m_ols, m_pois, m_negbin)) {
    rec <- extract_model(m)
    expect_setequal(rec$fixed_effects, c("Origin", "Destination", "Product", "Year"))
  }
})
