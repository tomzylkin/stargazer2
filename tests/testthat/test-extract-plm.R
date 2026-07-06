setup_grunfeld <- function() {
  skip_if_not_installed("plm")
  data("Grunfeld", package = "plm")
  Grunfeld
}

tol <- 1e-10

# ---------------------------------------------------------------------------
# Pooled OLS
# ---------------------------------------------------------------------------

test_that("extract_model.plm: pooling — model record contract", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "pooling")
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label, "Pooled OLS")
  expect_equal(rec$se_label, "OLS standard errors")
  expect_equal(rec$fixed_effects,  character(0L))
  expect_equal(rec$random_effects, character(0L))
  expect_true(rec$reports_fe)
  expect_equal(rec$fit$type, "ols")
  expect_equal(rec$dep_var, "inv")
  expect_true("Constant" %in% rec$coef_names)

  expect_equal(rec$coefs, unname(coef(m)),              tolerance = tol)
  expect_equal(rec$se,    unname(sqrt(diag(vcov(m)))),  tolerance = tol)
  expect_equal(rec$tstat, rec$coefs / rec$se,           tolerance = tol)
  expect_equal(rec$pval,  2 * pt(-abs(rec$tstat), df = df.residual(m)), tolerance = tol)
  expect_equal(rec$nobs,  as.integer(nobs(m)))

  s <- summary(m)
  expect_equal(rec$fit$r2,     s$r.squared[[1L]], tolerance = tol)
  expect_equal(rec$fit$adj_r2, s$r.squared[[2L]], tolerance = tol)
  expect_false(is.na(rec$fit$sigma))
  expect_false(is.na(rec$fit$fstat))
  expect_equal(rec$fit$fstat_df1, 2L)
})

# ---------------------------------------------------------------------------
# Within / FE (individual effect)
# ---------------------------------------------------------------------------

test_that("extract_model.plm: within individual — FE row, no constant", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "within")
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label,    "FE")
  expect_equal(rec$fixed_effects,  "firm")
  expect_equal(rec$random_effects, character(0L))
  # No intercept in within model
  expect_false("Constant" %in% rec$coef_names)

  expect_equal(rec$coefs, unname(coef(m)), tolerance = tol)
  expect_equal(rec$se,    unname(sqrt(diag(vcov(m)))), tolerance = tol)
  expect_false(is.na(rec$fit$r2))
  expect_false(is.na(rec$fit$fstat))
  expect_false(is.na(rec$fit$sigma))
})

# ---------------------------------------------------------------------------
# Within / FE (two-way effect)
# ---------------------------------------------------------------------------

test_that("extract_model.plm: within twoways — both FE vars", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"),
                model = "within", effect = "twoways")
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label,    "FE")
  expect_equal(rec$fixed_effects,  c("firm", "year"))
  expect_equal(rec$random_effects, character(0L))
})

# ---------------------------------------------------------------------------
# Random effects
# ---------------------------------------------------------------------------

test_that("extract_model.plm: random — RE row, no FE", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "random")
  rec <- stargazer2:::extract_model(m)

  expect_equal(rec$model_label,    "RE")
  expect_equal(rec$fixed_effects,  character(0L))
  expect_equal(rec$random_effects, "firm")
  expect_true(rec$reports_fe)

  expect_equal(rec$coefs, unname(coef(m)), tolerance = tol)
  expect_equal(rec$se,    unname(sqrt(diag(vcov(m)))), tolerance = tol)
  # RE Wald test: fstat present but df1/df2 are NA
  expect_false(is.na(rec$fit$fstat))
  expect_true(is.na(rec$fit$fstat_df1))
  expect_true(is.na(rec$fit$fstat_df2))
})

# ---------------------------------------------------------------------------
# FD and Between
# ---------------------------------------------------------------------------

test_that("extract_model.plm: fd — model label, no FE/RE rows", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "fd")
  rec <- stargazer2:::extract_model(m)
  expect_equal(rec$model_label,    "FD")
  expect_equal(rec$fixed_effects,  character(0L))
  expect_equal(rec$random_effects, character(0L))
})

test_that("extract_model.plm: between — model label", {
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "between")
  rec <- stargazer2:::extract_model(m)
  expect_equal(rec$model_label, "Between")
})

# ---------------------------------------------------------------------------
# SE overrides
# ---------------------------------------------------------------------------

test_that("extract_model.plm: vcov_override — custom SEs", {
  skip_if_not_installed("plm")
  G <- setup_grunfeld()
  m <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "within")
  V <- plm::vcovHC(m, method = "arellano")
  rec <- stargazer2:::extract_model(m, vcov_override = V)

  expect_equal(rec$se,    unname(sqrt(diag(V))), tolerance = tol)
  expect_equal(rec$coefs, unname(coef(m)),       tolerance = tol)
  expect_true(nchar(rec$se_label) > 0L)
})

test_that("extract_model.plm: se_override — user SE vector", {
  G <- setup_grunfeld()
  m   <- plm::plm(inv ~ value + capital, G, index = c("firm", "year"), model = "within")
  ses <- c(0.05, 0.02)
  rec <- stargazer2:::extract_model(m, se_override = ses)

  expect_equal(rec$se,       ses,                          tolerance = tol)
  expect_equal(rec$se_label, "user-specified standard errors")
})

# ---------------------------------------------------------------------------
# Integration: stargazer() rendering
# ---------------------------------------------------------------------------

test_that("stargazer() renders plm table: FE/RE/pooling columns", {
  skip_if_not_installed("plm")
  G    <- setup_grunfeld()
  m_po <- plm::plm(inv ~ value + capital, G, index = c("firm","year"), model = "pooling")
  m_fe <- plm::plm(inv ~ value + capital, G, index = c("firm","year"), model = "within")
  m_re <- plm::plm(inv ~ value + capital, G, index = c("firm","year"), model = "random")

  out <- stargazer(m_po, m_fe, m_re, type = "text")
  expect_type(out, "character")

  # Model-type labels
  expect_true(any(grepl("Pooled OLS", out)))
  expect_true(any(grepl("\\bFE\\b",   out)))
  expect_true(any(grepl("\\bRE\\b",   out)))

  # FE indicator row
  expect_true(any(grepl("Firm FE", out)))
  # RE indicator row
  expect_true(any(grepl("Firm RE", out)))
  # No Year FE (not in any model)
  expect_false(any(grepl("Year FE", out)))
})

test_that("stargazer() renders two-way FE: both indicator rows appear", {
  skip_if_not_installed("plm")
  G    <- setup_grunfeld()
  m_fe <- plm::plm(inv ~ value + capital, G, index = c("firm","year"),
                   model = "within", effect = "twoways")
  out  <- stargazer(m_fe, type = "text")

  expect_true(any(grepl("Firm FE", out)))
  expect_true(any(grepl("Year FE", out)))
})

test_that("stargazer() RE model: F-stat renders without NA df", {
  skip_if_not_installed("plm")
  G   <- setup_grunfeld()
  m   <- plm::plm(inv ~ value + capital, G, index = c("firm","year"), model = "random")
  out <- stargazer(m, type = "text")

  # F-stat row should not contain "NA"
  fstat_lines <- out[grepl("F Statistic|Wald", out)]
  if (length(fstat_lines) > 0L) {
    expect_false(any(grepl("NA", fstat_lines)))
  }
})
