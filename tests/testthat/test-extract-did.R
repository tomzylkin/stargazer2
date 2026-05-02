# Numerical extraction tests for DiD packages.
#
# Tests cover:
#   - AGGTEobj (did::aggte): ATT, SE, CI, model label, use_ci, nobs
#   - emfx (etwfe::emfx): estimate, SE, CI, model label, use_ci, nobs
#   - staggered_result: estimate, SE, CI, model label, use_ci
#   - as_staggered_result() wrapper
#   - Integration: 4-column DiD comparison table renders without error

tol <- 1e-10

# ---------------------------------------------------------------------------
# Shared setup
# ---------------------------------------------------------------------------

setup_mpdta <- function() {
  skip_if_not_installed("did")
  data("mpdta", package = "did")
  mpdta
}

# ---------------------------------------------------------------------------
# AGGTEobj (Callaway-Sant'Anna)
# ---------------------------------------------------------------------------

test_that("extract_model.AGGTEobj: ATT and SE match did package", {
  skip_if_not_installed("did")
  mpdta <- setup_mpdta()

  cs_out <- did::att_gt(
    yname   = "lemp", tname = "year", idname = "countyreal",
    gname   = "first.treat", xformla = ~lpop, data = mpdta
  )
  cs_agg <- did::aggte(cs_out, type = "simple")

  rec <- pkgload::load_all(quiet = TRUE) |> invisible()
  rec <- stargazer2:::extract_model(cs_agg)

  expect_equal(rec$coef_names, "ATT")
  expect_equal(rec$coefs,      cs_agg$overall.att,            tolerance = tol)
  expect_equal(rec$se,         cs_agg$overall.se,             tolerance = tol)
  expect_equal(rec$ci_lower,   cs_agg$overall.att - 1.96 * cs_agg$overall.se, tolerance = tol)
  expect_equal(rec$ci_upper,   cs_agg$overall.att + 1.96 * cs_agg$overall.se, tolerance = tol)
  expect_true(rec$use_ci)
  expect_equal(rec$model_label, "Callaway-Sant'Anna")
  expect_equal(rec$dep_var,     "lemp")
  expect_equal(rec$nobs,        as.integer(nrow(mpdta)))
  expect_equal(rec$se_label,    "95% confidence intervals")
  expect_length(rec$fixed_effects, 0L)
})

test_that("extract_model.AGGTEobj: p-values derived from SE", {
  skip_if_not_installed("did")
  mpdta <- setup_mpdta()

  cs_out <- did::att_gt(
    yname = "lemp", tname = "year", idname = "countyreal",
    gname = "first.treat", xformla = ~lpop, data = mpdta
  )
  cs_agg <- did::aggte(cs_out, type = "simple")
  rec    <- stargazer2:::extract_model(cs_agg)

  expected_pval <- 2 * pnorm(-abs(cs_agg$overall.att / cs_agg$overall.se))
  expect_equal(rec$pval, expected_pval, tolerance = tol)
})

test_that("extract_model.AGGTEobj show.dynamics: returns egt-indexed rows", {
  skip_if_not_installed("did")
  mpdta <- setup_mpdta()

  cs_out <- did::att_gt(
    yname = "lemp", tname = "year", idname = "countyreal",
    gname = "first.treat", xformla = ~lpop, data = mpdta
  )
  cs_dyn <- did::aggte(cs_out, type = "dynamic")
  rec    <- stargazer2:::extract_model(cs_dyn, show.dynamics = TRUE)

  expect_equal(rec$coef_names, paste0("t = ", cs_dyn$egt))
  expect_equal(rec$coefs,      cs_dyn$att.egt, tolerance = tol)
  expect_equal(rec$se,         cs_dyn$se.egt,  tolerance = tol)
  expect_true(rec$use_ci)
})

# ---------------------------------------------------------------------------
# emfx (Extended TWFE)
# ---------------------------------------------------------------------------

test_that("extract_model.emfx: estimate and SE match emfx output", {
  skip_if_not_installed("etwfe")
  skip_if_not_installed("marginaleffects")
  data("mpdta", package = "did")

  etwfe_mod <- etwfe::etwfe(
    fml = lemp ~ lpop, tvar = year, gvar = first.treat,
    ivar = countyreal, data = mpdta, vcov = ~countyreal
  )
  emfx_mod <- etwfe::emfx(etwfe_mod)
  rec      <- stargazer2:::extract_model(emfx_mod)

  expect_equal(rec$coef_names, "ATT")
  expect_equal(rec$coefs,      emfx_mod$estimate[[1L]],    tolerance = tol)
  expect_equal(rec$se,         emfx_mod$std.error[[1L]],   tolerance = tol)
  expect_equal(rec$ci_lower,   emfx_mod$conf.low[[1L]],    tolerance = tol)
  expect_equal(rec$ci_upper,   emfx_mod$conf.high[[1L]],   tolerance = tol)
  expect_equal(rec$pval,       emfx_mod$p.value[[1L]],     tolerance = tol)
  expect_true(rec$use_ci)
  expect_equal(rec$model_label, "Extended TWFE")
  expect_equal(rec$dep_var,     "lemp")
  expect_false(is.na(rec$nobs))
})

test_that("extract_model.emfx event-study: multi-row output", {
  skip_if_not_installed("etwfe")
  skip_if_not_installed("marginaleffects")
  data("mpdta", package = "did")

  etwfe_mod  <- etwfe::etwfe(
    fml = lemp ~ lpop, tvar = year, gvar = first.treat,
    data = mpdta, vcov = ~countyreal
  )
  emfx_event <- etwfe::emfx(etwfe_mod, type = "event")
  rec        <- stargazer2:::extract_model(emfx_event)

  expect_equal(nrow(emfx_event), length(rec$coefs))
  expect_equal(rec$coef_names, paste0("t = ", emfx_event$event))
  expect_equal(rec$coefs,      emfx_event$estimate,   tolerance = tol)
  expect_equal(rec$se,         emfx_event$std.error,  tolerance = tol)
  expect_true(rec$use_ci)
})

# ---------------------------------------------------------------------------
# staggered_result
# ---------------------------------------------------------------------------

test_that("as_staggered_result adds class and attrs", {
  skip_if_not_installed("staggered")
  data("mpdta", package = "did")

  st_mod <- staggered::staggered(
    df = mpdta, i = "countyreal", t = "year",
    g = "first.treat", y = "lemp", estimand = "simple"
  )
  wrapped <- as_staggered_result(st_mod, dep_var = "lemp", nobs = 2500L)

  expect_s3_class(wrapped, "staggered_result")
  expect_equal(attr(wrapped, "dep_var"),  "lemp")
  expect_equal(attr(wrapped, "nobs_val"), 2500L)
})

test_that("extract_model.staggered_result: estimate and SE", {
  skip_if_not_installed("staggered")
  data("mpdta", package = "did")

  st_mod  <- staggered::staggered(
    df = mpdta, i = "countyreal", t = "year",
    g = "first.treat", y = "lemp", estimand = "simple"
  )
  wrapped <- as_staggered_result(st_mod, dep_var = "lemp", nobs = 2500L)
  rec     <- stargazer2:::extract_model(wrapped)

  expect_equal(rec$coef_names,  "ATT")
  expect_equal(rec$coefs,       st_mod$estimate, tolerance = tol)
  expect_equal(rec$se,          st_mod$se,       tolerance = tol)
  expect_equal(rec$ci_lower,    st_mod$estimate - 1.96 * st_mod$se, tolerance = tol)
  expect_equal(rec$ci_upper,    st_mod$estimate + 1.96 * st_mod$se, tolerance = tol)
  expect_true(rec$use_ci)
  expect_equal(rec$model_label, "Roth-Sant'Anna")
  expect_equal(rec$dep_var,     "lemp")
  expect_equal(rec$nobs,        2500L)
})

# ---------------------------------------------------------------------------
# Integration: 4-column DiD comparison table
# ---------------------------------------------------------------------------

test_that("4-column DiD table renders (ASCII and LaTeX) without error", {
  skip_if_not_installed("did")
  skip_if_not_installed("etwfe")
  skip_if_not_installed("staggered")
  skip_if_not_installed("fixest")
  data("mpdta", package = "did")

  # TWFE with single post-treatment ATT coefficient
  mpdta$treated <- as.integer(mpdta$first.treat != 0 & mpdta$year >= mpdta$first.treat)
  twfe_att <- fixest::feols(
    lemp ~ lpop + treated | countyreal + year,
    data = mpdta, vcov = ~countyreal
  )

  # Callaway-Sant'Anna
  cs_out <- did::att_gt(
    yname = "lemp", tname = "year", idname = "countyreal",
    gname = "first.treat", xformla = ~lpop, data = mpdta
  )
  cs_agg <- did::aggte(cs_out, type = "simple")

  # Extended TWFE
  etwfe_mod <- etwfe::etwfe(
    fml = lemp ~ lpop, tvar = year, gvar = first.treat,
    ivar = countyreal, data = mpdta, vcov = ~countyreal
  )
  emfx_mod <- etwfe::emfx(etwfe_mod)

  # Roth-Sant'Anna
  st_mod  <- staggered::staggered(
    df = mpdta, i = "countyreal", t = "year",
    g = "first.treat", y = "lemp", estimand = "simple"
  )
  st_wrap <- as_staggered_result(st_mod, dep_var = "lemp", nobs = nrow(mpdta))

  expect_no_error({
    ascii_out <- stargazer2::stargazer(
      twfe_att, cs_agg, emfx_mod, st_wrap,
      type = "text", omit = "lpop", covariate.labels = "ATT",
      out = tempfile()
    )
  })
  expect_no_error({
    latex_out <- stargazer2::stargazer(
      twfe_att, cs_agg, emfx_mod, st_wrap,
      type = "latex", omit = "lpop", covariate.labels = "ATT",
      out = tempfile()
    )
  })

  # Per-column SE note distinguishes CI columns.
  # Normalize whitespace first since ASCII output word-wraps the note.
  ascii_norm <- gsub("\\s+", " ", ascii_out)
  expect_true(grepl("95% confidence intervals", ascii_norm, fixed = TRUE))
})
