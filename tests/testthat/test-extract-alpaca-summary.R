# Numerical verification tests for extract_model.summary.feglm().
#
# alpaca users obtain heteroskedasticity-robust or clustered SEs via
#   s <- summary(mod, type = "clustered", cluster = ~X)
# and pass s directly to stargazer().  These tests verify that extraction
# from summary.feglm objects is numerically exact and that SE labels are
# correctly inferred from the unevaluated summary() call expression.

tol <- 1e-10

# ---------------------------------------------------------------------------
# Test data setup
# ---------------------------------------------------------------------------

setup_alpaca_summary_data <- function() {
  skip_if_not_installed("alpaca")
  set.seed(99)
  n <- 600
  grp  <- sample(seq_len(25), n, replace = TRUE)
  grp2 <- sample(seq_len(6),  n, replace = TRUE)
  x1   <- rnorm(n)
  x2   <- rnorm(n)
  eta  <- 0.5 * x1 - 0.3 * x2 + rnorm(n, sd = 0.5)
  list(
    logit = data.frame(
      y    = as.integer(eta > 0),
      x1   = x1,
      x2   = x2,
      grp  = grp,
      grp2 = grp2
    ),
    poisson = data.frame(
      y    = rpois(n, exp(0.2 * x1 - 0.1 * x2)),
      x1   = x1,
      x2   = x2,
      grp  = grp,
      grp2 = grp2
    )
  )
}

# ---------------------------------------------------------------------------
# Coefficients, SEs, z-stats, p-values from summary.feglm
# ---------------------------------------------------------------------------

test_that("extract_model.summary.feglm (default): coefs match cm[,'Estimate']", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$coefs, unname(s$cm[, "Estimate"]), tolerance = tol)
  expect_equal(rec$coef_names, rownames(s$cm))
})

test_that("extract_model.summary.feglm (default): SEs match cm[,'Std. error']", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$se, unname(s$cm[, "Std. error"]), tolerance = tol)
})

test_that("extract_model.summary.feglm (sandwich): SEs differ from MLE SEs", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s_mle  <- summary(mod)
  s_sand <- summary(mod, type = "sandwich")
  rec_mle  <- stargazer2:::extract_model(s_mle)
  rec_sand <- stargazer2:::extract_model(s_sand)
  # Sandwich SEs should differ from MLE SEs
  expect_false(isTRUE(all.equal(rec_mle$se, rec_sand$se, tolerance = 1e-6)))
  # But coefficients are identical
  expect_equal(rec_mle$coefs, rec_sand$coefs, tolerance = tol)
})

test_that("extract_model.summary.feglm (clustered): SEs match s$cm", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod, type = "clustered", cluster = ~grp)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$se, unname(s$cm[, "Std. error"]), tolerance = tol)
})

test_that("extract_model.summary.feglm: z-stats match cm[,'z value']", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod, type = "clustered", cluster = ~grp)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$tstat, unname(s$cm[, "z value"]), tolerance = tol)
})

test_that("extract_model.summary.feglm: p-values match cm[,'Pr(> |z|)']", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod, type = "clustered", cluster = ~grp)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$pval, unname(s$cm[, "Pr(> |z|)"]), tolerance = tol)
})

# ---------------------------------------------------------------------------
# nobs, FEs, model label, dep var
# ---------------------------------------------------------------------------

test_that("extract_model.summary.feglm: nobs correct", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$nobs, as.integer(s$nobs[["nobs"]]))
})

test_that("extract_model.summary.feglm: FEs from lvls.k names", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$fixed_effects, "grp")
})

test_that("extract_model.summary.feglm: Logit model label", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$model_label, "Logit")
})

test_that("extract_model.summary.feglm: Poisson model label", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$poisson, poisson())
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$model_label, "Poisson")
})

test_that("extract_model.summary.feglm: dep_var extracted from formula", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s   <- summary(mod)
  rec <- stargazer2:::extract_model(s)
  expect_equal(rec$dep_var, "y")
})

# ---------------------------------------------------------------------------
# SE label inference via stargazer() call-expression parsing
# ---------------------------------------------------------------------------

# Collapse word-wrapped note lines so we can match phrase-level strings.
# The ASCII renderer may break "MLE standard\n               errors" across
# lines; this helper restores the original single-line note for matching.
note_text <- function(out) {
  gsub("\n[ \t]+", " ", out)
}

test_that("stargazer: pre-assigned summary.feglm default note warns SE type unknown", {
  d    <- setup_alpaca_summary_data()
  mod  <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s    <- summary(mod)
  out  <- note_text(stargazer(s, type = "text"))
  expect_match(out, "SE type not detected", fixed = TRUE)
})

test_that("stargazer: inline summary(mod) call resolves to MLE label", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- note_text(stargazer(summary(mod), type = "text"))
  expect_match(out, "MLE standard errors", fixed = TRUE)
})

test_that("stargazer: summary.feglm sandwich label is 'heteroskedasticity-robust'", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- note_text(stargazer(summary(mod, type = "sandwich"), type = "text"))
  expect_match(out, "heteroskedasticity-robust standard errors", fixed = TRUE)
})

test_that("stargazer: summary.feglm clustered label includes variable name", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- note_text(stargazer(summary(mod, type = "clustered", cluster = ~grp),
                             type = "text"))
  expect_match(out, "standard errors clustered by grp", fixed = TRUE)
})

test_that("stargazer: summary.feglm two-way additive cluster label uses 'and'", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp + grp2, d$logit, binomial("logit"))
  out <- note_text(stargazer(summary(mod, type = "clustered", cluster = ~grp + grp2),
                             type = "text"))
  expect_match(out, "clustered by grp and grp2", fixed = TRUE)
})

test_that("stargazer: summary.feglm interacted cluster label uses 'x'", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp + grp2, d$logit, binomial("logit"))
  out <- note_text(stargazer(summary(mod, type = "clustered", cluster = ~grp^grp2),
                             type = "text"))
  expect_match(out, "clustered by grp x grp2", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Mixed summary.feglm and feglm in same table
# ---------------------------------------------------------------------------

test_that("stargazer: mixed feglm and summary.feglm renders without error", {
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  expect_no_error(
    stargazer(mod, summary(mod, type = "clustered", cluster = ~grp), type = "text")
  )
})

test_that("stargazer: inline summary() call gives per-column SE labels", {
  # SE type is auto-detected when summary() is written inline in stargazer().
  d   <- setup_alpaca_summary_data()
  mod <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  out <- note_text(
    stargazer(mod, summary(mod, type = "clustered", cluster = ~grp), type = "text")
  )
  expect_match(out, "MLE standard errors",              fixed = TRUE)
  expect_match(out, "standard errors clustered by grp", fixed = TRUE)
})

test_that("stargazer: pre-assigned summary.feglm accepts se_label override", {
  # When the summary object is pre-assigned to a variable, call-expression
  # parsing cannot detect the SE type; the user can supply se_label explicitly.
  d    <- setup_alpaca_summary_data()
  mod  <- alpaca::feglm(y ~ x1 + x2 | grp, d$logit, binomial("logit"))
  s_cl <- summary(mod, type = "clustered", cluster = ~grp)
  out  <- note_text(
    stargazer(mod, s_cl, type = "text",
              se_label = c(NA, "standard errors clustered by grp"))
  )
  expect_match(out, "MLE standard errors",              fixed = TRUE)
  expect_match(out, "standard errors clustered by grp", fixed = TRUE)
})
