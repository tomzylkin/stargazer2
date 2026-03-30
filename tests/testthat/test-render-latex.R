# Structural and content tests for the LaTeX rendering pipeline.
#
# These tests verify:
#   - correct column count in the tabular spec
#   - presence of required structural elements
#   - correct number of data rows
#   - FE indicator rows appear for fixest models
#   - SE notes appear in the output
#   - numerical values in the output match model quantities to 3 d.p.

setup_wage1_render <- function() {
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
# Structural tests: lm-only table (Table 2 in test plan)
# ---------------------------------------------------------------------------

test_that("render_latex: four-column lm table has correct tabular spec", {
  wage1 <- setup_wage1_render()
  m1 <- lm(lwage ~ educ + exper + tenure, wage1)
  m2 <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  out <- stargazer(m1, m2, m1, m2, type = "latex")

  # tabular spec: @{\extracolsep{5pt}}lcccc for 4 data columns
  expect_match(out, "@\\{\\\\extracolsep\\{5pt\\}\\}lcccc", perl = TRUE)
})

test_that("render_latex: single-model table has correct tabular spec", {
  wage1 <- setup_wage1_render()
  m1 <- lm(lwage ~ educ + exper + tenure, wage1)
  out <- stargazer(m1, type = "latex")
  expect_match(out, "@\\{\\\\extracolsep\\{5pt\\}\\}lc", perl = TRUE)
})

test_that("render_latex: contains required structural elements", {
  wage1 <- setup_wage1_render()
  m1 <- lm(lwage ~ educ + exper + tenure, wage1)
  out <- stargazer(m1, m1, type = "latex")

  expect_match(out, "\\\\begin\\{table\\}", perl = TRUE)
  expect_match(out, "\\\\end\\{table\\}",   perl = TRUE)
  expect_match(out, "\\\\begin\\{tabular\\}", perl = TRUE)
  expect_match(out, "\\\\end\\{tabular\\}",   perl = TRUE)
  expect_match(out, "\\\\hline",             perl = TRUE)
  expect_match(out, "Dependent variable",    fixed = TRUE)
  expect_match(out, "Observations",          fixed = TRUE)
})

test_that("render_latex: column numbers appear in header", {
  wage1 <- setup_wage1_render()
  m1 <- lm(lwage ~ educ + exper + tenure, wage1)
  m2 <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  out <- stargazer(m1, m2, type = "latex")
  expect_match(out, "(1)", fixed = TRUE)
  expect_match(out, "(2)", fixed = TRUE)
})

test_that("render_latex: significance note present", {
  wage1 <- setup_wage1_render()
  m1 <- lm(lwage ~ educ + exper + tenure, wage1)
  out <- stargazer(m1, type = "latex")
  expect_match(out, "p$<$0.1", fixed = TRUE)
  expect_match(out, "p$<$0.05", fixed = TRUE)
  expect_match(out, "p$<$0.01", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Numerical content tests
# ---------------------------------------------------------------------------

test_that("render_latex: coefficient for educ appears in output (3 d.p.)", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex")

  coef_educ <- coef(m1)["educ"]
  # Format as stargazer2 would: 3 decimal places
  expected  <- formatC(abs(coef_educ), digits = 3L, format = "f")
  expect_match(out, expected, fixed = TRUE)
})

test_that("render_latex: R-squared appears in output (3 d.p.)", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex")

  r2_str <- formatC(summary(m1)$r.squared, digits = 3L, format = "f")
  expect_match(out, r2_str, fixed = TRUE)
})

test_that("render_latex: observation count appears in output", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex")
  expect_match(out, as.character(nobs(m1)), fixed = TRUE)
})

test_that("render_latex: negative coefficients use LaTeX minus sign", {
  # female has a negative coefficient
  wage1 <- setup_wage1_render()
  m2    <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  out   <- stargazer(m2, type = "latex")
  expect_match(out, "$-$", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Fixed-effects rows (fixest)
# ---------------------------------------------------------------------------

test_that("render_latex: FE rows appear for feols model", {
  skip_if_not_installed("fixest")
  wage1 <- setup_wage1_render()
  f3 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  out <- stargazer(f3, type = "latex")

  expect_match(out, "Region FE",     fixed = TRUE)
  expect_match(out, "Occupation FE", fixed = TRUE)
  expect_match(out, "Yes",           fixed = TRUE)
})

test_that("render_latex: interacted FE labelled correctly", {
  skip_if_not_installed("fixest")
  wage1 <- setup_wage1_render()
  f5 <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region^industry,
    data = wage1
  )
  out <- stargazer(f5, type = "latex")
  expect_match(out, "Region x Industry FE", fixed = TRUE)
})

test_that("render_latex: mixed lm + feols shows FE Yes/No correctly", {
  skip_if_not_installed("fixest")
  wage1 <- setup_wage1_render()
  m1  <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  f3  <- fixest::feols(
    lwage ~ educ + exper + tenure + female + married | region + occupation,
    data = wage1
  )
  out <- stargazer(m1, f3, type = "latex")
  # lm col should show "No", feols col should show "Yes"
  expect_match(out, "No",  fixed = TRUE)
  expect_match(out, "Yes", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# SE notes
# ---------------------------------------------------------------------------

test_that("render_latex: OLS SE note suppressed for lm model (matches original stargazer)", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex")
  # OLS standard errors are implicit for lm — suppressed to match original stargazer
  expect_false(grepl("OLS standard errors", out, fixed = TRUE))
  # Significance note still present
  expect_match(out, "p$<$0.1", fixed = TRUE)
})

test_that("render_latex: SE note present when vcov supplied", {
  skip_if_not_installed("sandwich")
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  V     <- sandwich::vcovHC(m1, type = "HC1")
  # Without se_label: sandwich returns a plain matrix (no class info preserved),
  # so a generic SE note appears — the note is non-empty and not the OLS default
  out1 <- stargazer(m1, vcov = list(V), type = "latex")
  expect_match(out1, "standard errors", fixed = TRUE)
  # With se_label: user-specified note appears
  out2 <- stargazer(m1, vcov = list(V), se_label = "HC1-robust",
                    type = "latex")
  expect_match(out2, "HC1-robust", fixed = TRUE)
})

test_that("render_latex: SE note present when vcovCL supplied", {
  skip_if_not_installed("sandwich")
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  V     <- sandwich::vcovCL(m1, cluster = ~industry, data = wage1)
  # Without se_label: sandwich returns a plain matrix (no class info preserved),
  # so a generic SE note appears — the note is non-empty and not the OLS default
  out1 <- stargazer(m1, vcov = list(V), type = "latex")
  expect_match(out1, "standard errors", fixed = TRUE)
  # With se_label: explicit cluster note
  out2 <- stargazer(m1, vcov = list(V),
                    se_label = "Standard errors clustered by industry",
                    type = "latex")
  expect_match(out2, "industry", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Formatting options
# ---------------------------------------------------------------------------

test_that("render_latex: title and label appear in output", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex",
                     title = "My Title", label = "tab:mymodel")
  expect_match(out, "My Title",    fixed = TRUE)
  expect_match(out, "tab:mymodel", fixed = TRUE)
})

test_that("render_latex: covariate.labels replaces coef names", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex",
                     covariate.labels = c("Intercept", "Education",
                                          "Experience", "Tenure"))
  expect_match(out, "Education",  fixed = TRUE)
  expect_match(out, "Experience", fixed = TRUE)
})

test_that("render_latex: omit removes matching covariates", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  # "(Intercept)" is renamed to "Constant" — omit by that name
  out   <- stargazer(m1, type = "latex", omit = "Constant")
  expect_false(grepl("Constant", out, fixed = TRUE))
  expect_match(out, "educ", fixed = TRUE)
})

test_that("render_latex: digits controls decimal places", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out2  <- stargazer(m1, type = "latex", digits = 2L)
  # With digits = 2, the R² should have exactly 2 decimal places
  r2_2dp <- formatC(summary(m1)$r.squared, digits = 2L, format = "f")
  expect_match(out2, r2_2dp, fixed = TRUE)
})

test_that("render_latex: font.size inserts LaTeX size command", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex", font.size = "small")
  expect_match(out, "\\small", fixed = TRUE)
})

test_that("render_latex: custom notes appended correctly", {
  wage1 <- setup_wage1_render()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  out   <- stargazer(m1, type = "latex", notes = "My custom note.")
  expect_match(out, "My custom note.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Gravity model: canonical test case (Table 7 in Key Design Decisions)
# ---------------------------------------------------------------------------

test_that("gravity table: three-column feols/fepois/fenegbin renders without error", {
  skip_if_not_installed("fixest")
  data("trade", package = "fixest")
  gravity_ols    <- fixest::feols(log(Euros) ~ log(dist_km) |
                      Origin + Destination + Product + Year, trade)
  gravity_pois   <- fixest::fepois(Euros ~ log(dist_km) |
                      Origin + Destination + Product + Year, trade)
  gravity_negbin <- fixest::fenegbin(Euros ~ log(dist_km) |
                      Origin + Destination + Product + Year, trade)

  out <- stargazer(gravity_ols, gravity_pois, gravity_negbin, type = "latex")

  # Three data columns
  expect_match(out, "@{\\extracolsep{5pt}}lccc", fixed = TRUE)
  # Single covariate row
  expect_match(out, "log(dist\\_km)", fixed = TRUE)
  # All four FE rows
  expect_match(out, "Origin FE",      fixed = TRUE)
  expect_match(out, "Destination FE", fixed = TRUE)
  expect_match(out, "Product FE",     fixed = TRUE)
  expect_match(out, "Year FE",        fixed = TRUE)
  # All columns show Yes for all four FEs
  n_yes <- lengths(regmatches(out, gregexpr("Yes", out, fixed = TRUE)))
  expect_equal(n_yes, 12L)  # 4 FEs x 3 columns
})
