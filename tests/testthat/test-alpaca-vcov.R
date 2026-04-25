# Tests for alpaca_vcovSandwich() and alpaca_vcovCL() helpers.

tol <- 1e-10

setup_alpaca_vcov_data <- function() {
  skip_if_not_installed("alpaca")
  set.seed(77)
  n <- 500
  list(
    logit = data.frame(
      y    = rbinom(n, 1, 0.4),
      x1   = rnorm(n),
      x2   = rnorm(n),
      grp  = sample(seq_len(20), n, replace = TRUE),
      grp2 = sample(seq_len(5),  n, replace = TRUE)
    )
  )
}

# ---------------------------------------------------------------------------
# alpaca_vcovSandwich
# ---------------------------------------------------------------------------

test_that("alpaca_vcovSandwich: SEs match summary(type='sandwich')", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovSandwich(mod)
  s   <- summary(mod, type = "sandwich")
  expected_se <- s$cm[, "Std. error"]
  expect_equal(sqrt(diag(V)), expected_se, tolerance = tol)
})

test_that("alpaca_vcovSandwich: class is 'vcovAlpacaSandwich'", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovSandwich(mod)
  expect_true(inherits(V, "vcovAlpacaSandwich"))
})

test_that("alpaca_vcovSandwich: dimnames match coefficient names", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovSandwich(mod)
  expect_equal(rownames(V), names(coef(mod)))
  expect_equal(colnames(V), names(coef(mod)))
})

# ---------------------------------------------------------------------------
# alpaca_vcovCL
# ---------------------------------------------------------------------------

test_that("alpaca_vcovCL: SEs match summary(type='clustered')", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovCL(mod, ~grp)
  s   <- summary(mod, type = "clustered", cluster = ~grp)
  expected_se <- s$cm[, "Std. error"]
  expect_equal(sqrt(diag(V)), expected_se, tolerance = tol)
})

test_that("alpaca_vcovCL: class is 'vcovAlpacaCL'", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovCL(mod, ~grp)
  expect_true(inherits(V, "vcovAlpacaCL"))
})

test_that("alpaca_vcovCL: cluster attribute stores the formula", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  cl  <- ~grp
  V   <- alpaca_vcovCL(mod, cl)
  expect_identical(attr(V, "cluster"), cl)
})

# ---------------------------------------------------------------------------
# se_label_from_vcov integration
# ---------------------------------------------------------------------------

test_that("vcovAlpacaSandwich gives 'heteroskedasticity-robust' label", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovSandwich(mod)
  expect_equal(stargazer2:::se_label_from_vcov(V),
               "heteroskedasticity-robust standard errors")
})

test_that("vcovAlpacaCL one-way gives 'clustered by grp' label", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovCL(mod, ~grp)
  expect_equal(stargazer2:::se_label_from_vcov(V),
               "standard errors clustered by grp")
})

test_that("vcovAlpacaCL two-way additive gives 'clustered by grp and grp2'", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp + grp2, d$logit, binomial("logit"))
  V   <- alpaca_vcovCL(mod, ~grp + grp2)
  expect_equal(stargazer2:::se_label_from_vcov(V),
               "standard errors clustered by grp and grp2")
})

test_that("vcovAlpacaCL interaction gives 'clustered by grp x grp2'", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp + grp2, d$logit, binomial("logit"))
  V   <- alpaca_vcovCL(mod, ~grp^grp2)
  expect_equal(stargazer2:::se_label_from_vcov(V),
               "standard errors clustered by grp x grp2")
})

# ---------------------------------------------------------------------------
# Full stargazer() integration
# ---------------------------------------------------------------------------

test_that("stargazer: alpaca_vcovSandwich produces correct note", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- gsub("\n[ \t]+", " ", stargazer(mod, vcov = list(alpaca_vcovSandwich(mod)),
                                         type = "text"))
  expect_match(out, "heteroskedasticity-robust standard errors", fixed = TRUE)
})

test_that("stargazer: alpaca_vcovCL produces correct clustered note", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- gsub("\n[ \t]+", " ", stargazer(mod, vcov = list(alpaca_vcovCL(mod, ~grp)),
                                         type = "text"))
  expect_match(out, "standard errors clustered by grp", fixed = TRUE)
})

test_that("stargazer: alpaca_vcovSandwich SEs appear in table correctly", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  V   <- alpaca_vcovSandwich(mod)
  rec <- stargazer2:::extract_model(mod, vcov_override = V)
  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = tol)
  expect_equal(rec$coefs, unname(coef(mod)), tolerance = tol)
})

test_that("stargazer: mixed MLE and clustered columns labelled per-column", {
  d   <- setup_alpaca_vcov_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- gsub("\n[ \t]+", " ",
    stargazer(mod, mod,
              vcov = list(NULL, alpaca_vcovCL(mod, ~grp)),
              type = "text"))
  expect_match(out, "MLE standard errors",              fixed = TRUE)
  expect_match(out, "standard errors clustered by grp", fixed = TRUE)
})
