test_that("extract_model.glm: logit — model label, z-stats, MLE SE label", {
  skip_if_not_installed("stats")
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial("logit"))
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label, "Logit")
  expect_equal(rec$se_label, "MLE standard errors")
  expect_equal(rec$nobs, as.integer(nobs(m)))
  expect_equal(rec$fit$type, "glm")

  # Coefficients match coef()
  expect_equal(rec$coefs, unname(coef(m)), tolerance = 1e-10)

  # SEs match sqrt(diag(vcov()))
  expect_equal(rec$se, unname(sqrt(diag(vcov(m)))), tolerance = 1e-10)

  # z-stats = coef / se
  expect_equal(rec$tstat, rec$coefs / rec$se, tolerance = 1e-10)

  # p-values are two-sided normal (not t)
  expected_pvals <- 2 * pnorm(-abs(rec$tstat))
  expect_equal(rec$pval, expected_pvals, tolerance = 1e-10)

  # Log-likelihood and AIC in fit
  expect_equal(rec$fit$ll, as.numeric(logLik(m)), tolerance = 1e-10)
  expect_equal(rec$fit$aic, AIC(m), tolerance = 1e-10)

  # No fixed effects
  expect_equal(rec$fixed_effects, character(0L))

  # "(Intercept)" renamed to "Constant"
  expect_true("Constant" %in% rec$coef_names)
})

test_that("extract_model.glm: probit — model label", {
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial("probit"))
  rec <- stargazer2:::extract_model(m)
  expect_equal(rec$model_label, "Probit")
  expect_equal(rec$fit$type, "glm")
})

test_that("extract_model.glm: poisson — model label, z-stats", {
  m <- glm(gear ~ wt + hp, data = mtcars, family = poisson("log"))
  rec <- stargazer2:::extract_model(m)
  expect_equal(rec$model_label, "Poisson")
  expect_equal(rec$fit$type, "glm")
  expect_equal(rec$se_label, "MLE standard errors")
  # p-values are standard normal
  expected_pvals <- 2 * pnorm(-abs(rec$tstat))
  expect_equal(rec$pval, expected_pvals, tolerance = 1e-10)
})

test_that("extract_model.glm: Gaussian identity — OLS-equivalent stats", {
  m <- glm(mpg ~ wt + hp, data = mtcars, family = gaussian("identity"))
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label, "OLS")
  expect_equal(rec$se_label, "OLS standard errors")
  expect_equal(rec$fit$type, "ols")

  # R² matches manual computation
  y   <- fitted(m) + residuals(m)
  r2  <- 1 - sum(residuals(m)^2) / sum((y - mean(y))^2)
  expect_equal(rec$fit$r2, r2, tolerance = 1e-10)

  # sigma is present
  expect_false(is.na(rec$fit$sigma))

  # F-stat is present
  expect_false(is.na(rec$fit$fstat))
  expect_equal(rec$fit$fstat_df1, 2L)  # 2 predictors

  # t-statistics (not z) — p-values use pt
  df_r <- df.residual(m)
  expected_pvals <- 2 * pt(-abs(rec$tstat), df = df_r)
  expect_equal(rec$pval, expected_pvals, tolerance = 1e-10)

  # Coefficients and SEs match base
  expect_equal(rec$coefs, unname(coef(m)), tolerance = 1e-10)
  expect_equal(rec$se, unname(sqrt(diag(vcov(m)))), tolerance = 1e-10)
})

test_that("extract_model.glm: vcov_override — custom SEs and non-empty label", {
  skip_if_not_installed("sandwich")
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial("logit"))
  V <- sandwich::vcovHC(m, type = "HC1")
  rec <- stargazer2:::extract_model(m, vcov_override = V)

  expect_equal(rec$se, unname(sqrt(diag(V))), tolerance = 1e-10)
  expect_true(nchar(rec$se_label) > 0L)
})

test_that("extract_model.glm: dep_var extracted from formula", {
  m <- glm(am ~ wt, data = mtcars, family = binomial)
  rec <- stargazer2:::extract_model(m)
  expect_equal(rec$dep_var, "am")
})

test_that("stargazer() renders a glm logit table without error", {
  m <- glm(am ~ wt + hp, data = mtcars, family = binomial("logit"))
  out <- stargazer(m, type = "text")
  expect_type(out, "character")
  # MLE note and fit stats appear
  expect_true(any(grepl("MLE", out)))
  expect_true(any(grepl("Log Likelihood", out)))
  expect_true(any(grepl("Akaike", out)))
})

test_that("stargazer() renders a mixed lm + glm table without error", {
  m_lm  <- lm(mpg ~ wt + hp, data = mtcars)
  m_glm <- glm(am ~ wt + hp, data = mtcars, family = binomial("logit"))
  out <- stargazer(m_lm, m_glm, type = "text")
  expect_type(out, "character")
  expect_true(any(grepl("OLS|Logit", out)))
})
