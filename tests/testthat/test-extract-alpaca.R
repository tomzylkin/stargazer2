# Numerical verification tests for extract_model.feglm() (alpaca package).
#
# Tests cover:
#   - Logit model: coefficients, SEs, z-stats, p-values, nobs
#   - Poisson model: coefficients, SEs, squared-correlation R²
#   - Fixed effects detection via lvls.k names
#   - Model label detection (Logit, Probit, Poisson)
#   - SE auto-extraction and vcov-override paths

tol <- 1e-10

# ---------------------------------------------------------------------------
# Test data setup
# ---------------------------------------------------------------------------

setup_alpaca_data <- function() {
  skip_if_not_installed("alpaca")
  set.seed(123)
  n <- 500
  grp <- sample(seq_len(20), n, replace = TRUE)
  x1  <- rnorm(n)
  x2  <- rnorm(n)
  eta <- 0.4 * x1 - 0.3 * x2 + rnorm(n, sd = 0.5)
  list(
    logit = data.frame(
      y   = as.integer(eta > 0),
      x1  = x1,
      x2  = x2,
      grp = grp
    ),
    poisson = data.frame(
      y   = rpois(n, exp(0.2 * x1 - 0.1 * x2)),
      x1  = x1,
      x2  = x2,
      grp = grp
    )
  )
}

# ---------------------------------------------------------------------------
# Coefficients and SEs
# ---------------------------------------------------------------------------

test_that("extract_model.feglm (logit): coefs match alpaca::coef()", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)

  expect_equal(rec$coefs, unname(coef(mod)), tolerance = tol)
  expect_equal(rec$coef_names, names(coef(mod)))
})

test_that("extract_model.feglm (logit): auto-extracted SEs match sqrt(diag(vcov(mod)))", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expected_se <- unname(sqrt(diag(vcov(mod))))
  expect_equal(rec$se, expected_se, tolerance = tol)
})

test_that("extract_model.feglm (logit): z-stats are coef / se", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expected_z <- unname(coef(mod)) / unname(sqrt(diag(vcov(mod))))
  expect_equal(rec$tstat, expected_z, tolerance = tol)
})

test_that("extract_model.feglm (logit): p-values use normal distribution", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expected_z <- unname(coef(mod)) / unname(sqrt(diag(vcov(mod))))
  expected_p <- 2 * pnorm(-abs(expected_z))
  expect_equal(rec$pval, expected_p, tolerance = tol)
  expect_true(all(rec$pval >= 0 & rec$pval <= 1))
})

test_that("extract_model.feglm (logit): nobs matches mod$nobs", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$nobs, as.integer(mod$nobs[["nobs"]]))
})

test_that("extract_model.feglm (poisson): coefs and SEs correct", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$poisson,
                       family = poisson())
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$coefs, unname(coef(mod)), tolerance = tol)
  expect_equal(rec$se, unname(sqrt(diag(vcov(mod)))), tolerance = tol)
})

# ---------------------------------------------------------------------------
# Fixed effects detection
# ---------------------------------------------------------------------------

test_that("extract_model.feglm: single FE detected from lvls.k names", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$fixed_effects, "grp")
})

# ---------------------------------------------------------------------------
# Model labels
# ---------------------------------------------------------------------------

test_that("extract_model.feglm: logit labelled as 'Logit'", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$model_label, "Logit")
})

test_that("extract_model.feglm: probit labelled as 'Probit'", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("probit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$model_label, "Probit")
})

test_that("extract_model.feglm: Poisson labelled as 'Poisson'", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$poisson,
                       family = poisson())
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$model_label, "Poisson")
})

# ---------------------------------------------------------------------------
# SE label
# ---------------------------------------------------------------------------

test_that("extract_model.feglm: default SE label is 'MLE standard errors'", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$se_label, "MLE standard errors")
})

test_that("extract_model.feglm: vcov override changes SEs and label", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  p  <- length(coef(mod))
  V  <- diag(seq(0.01, 0.01 * p, by = 0.01))
  rec <- stargazer2:::extract_model(mod, vcov_override = V)
  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = tol)
  # Coefficients unchanged
  expect_equal(rec$coefs, unname(coef(mod)), tolerance = tol)
})

# ---------------------------------------------------------------------------
# Fit statistics
# ---------------------------------------------------------------------------

test_that("extract_model.feglm: squared-correlation R² is in [0,1]", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_false(is.na(rec$fit$r2))
  expect_true(rec$fit$r2 >= 0 & rec$fit$r2 <= 1)
})

test_that("extract_model.feglm: fit type is 'glm'", {
  d <- setup_alpaca_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, data = d$logit,
                       family = binomial("logit"))
  rec <- stargazer2:::extract_model(mod)
  expect_equal(rec$fit$type, "glm")
})
