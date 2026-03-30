# Tests for summary statistics table (data.frame input).

setup_wage1_summary <- function() {
  skip_if_not_installed("wooldridge")
  data("wage1", package = "wooldridge")
  # Use a clean numeric subset matching the canonical test case
  wage1[, c("lwage", "educ", "exper", "tenure", "female", "married")]
}

tol <- 1e-10

# ---------------------------------------------------------------------------
# Routing: data.frame and matrix trigger summary path
# ---------------------------------------------------------------------------

test_that("summary: data.frame input produces output without error", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  expect_type(out, "character")
  expect_true(nchar(out) > 0L)
})

test_that("summary: matrix input produces output without error", {
  df  <- setup_wage1_summary()
  m   <- as.matrix(df)
  out <- stargazer(m, type = "latex")
  expect_type(out, "character")
})

# ---------------------------------------------------------------------------
# Numerical correctness
# ---------------------------------------------------------------------------

test_that("summary: N values match non-missing obs counts", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  n   <- sum(!is.na(df$lwage))
  expect_match(out, as.character(n), fixed = TRUE)
})

test_that("summary: mean of lwage appears to 3 d.p.", {
  df      <- setup_wage1_summary()
  out     <- stargazer(df, type = "latex")
  expected <- formatC(mean(df$lwage), digits = 3L, format = "f")
  expect_match(out, expected, fixed = TRUE)
})

test_that("summary: sd of exper appears to 3 d.p.", {
  df      <- setup_wage1_summary()
  out     <- stargazer(df, type = "latex")
  expected <- formatC(sd(df$exper), digits = 3L, format = "f")
  expect_match(out, expected, fixed = TRUE)
})

test_that("summary: integer min/max shown without decimal places", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  # educ has integer min = 0 and max = 18
  expect_match(out, "0 & 18", fixed = TRUE)
})

test_that("summary: negative min uses LaTeX minus sign in latex output", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  # lwage has negative min
  expect_match(out, "$-$", fixed = TRUE)
})

test_that("summary: negative min shows plain minus in text output", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "text")
  expect_match(out, "-", fixed = TRUE)
  expect_false(grepl("$-$", out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Non-numeric columns silently skipped
# ---------------------------------------------------------------------------

test_that("summary: factor columns are silently skipped", {
  df         <- setup_wage1_summary()
  df$grp     <- factor(rep(c("a", "b"), nrow(df) / 2L))
  out        <- stargazer(df, type = "latex")
  # 'grp' should not appear in the output
  expect_false(grepl("grp", out, fixed = TRUE))
  # numeric cols still present
  expect_match(out, "lwage", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Structure: LaTeX
# ---------------------------------------------------------------------------

test_that("summary latex: has correct tabular spec for 5 default stats", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  expect_match(out, "@{\\extracolsep{5pt}}lccccc", fixed = TRUE)
})

test_that("summary latex: header contains multicolumn cells", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  expect_match(out, "\\multicolumn{1}{c}{N}", fixed = TRUE)
  expect_match(out, "\\multicolumn{1}{c}{Mean}", fixed = TRUE)
  expect_match(out, "\\multicolumn{1}{c}{St. Dev.}", fixed = TRUE)
})

test_that("summary latex: 'Statistic' appears as label column header", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  expect_match(out, "Statistic &", fixed = TRUE)
})

test_that("summary latex: no significance-star note", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex")
  expect_false(grepl("p$<$", out, fixed = TRUE))
})

test_that("summary latex: title and label appear when supplied", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex", title = "My Title", label = "tab:s1")
  expect_match(out, "My Title", fixed = TRUE)
  expect_match(out, "tab:s1", fixed = TRUE)
})

test_that("summary latex: custom notes appear", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex", notes = "Custom note.")
  expect_match(out, "Custom note.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Structure: ASCII
# ---------------------------------------------------------------------------

test_that("summary text: header row contains column names", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "text")
  expect_match(out, "Statistic", fixed = TRUE)
  expect_match(out, "St. Dev.",  fixed = TRUE)
  expect_match(out, "Mean",      fixed = TRUE)
})

test_that("summary text: variable names appear in output", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "text")
  expect_match(out, "lwage",   fixed = TRUE)
  expect_match(out, "married", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Options
# ---------------------------------------------------------------------------

test_that("summary: median=TRUE adds Median column", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex", median = TRUE)
  expect_match(out, "\\multicolumn{1}{c}{Median}", fixed = TRUE)
  # tabular spec now has 7 cols
  expect_match(out, "@{\\extracolsep{5pt}}lcccccc", fixed = TRUE)
  # median of lwage appears
  med_str <- formatC(median(df$lwage), digits = 3L, format = "f")
  expect_match(out, med_str, fixed = TRUE)
})

test_that("summary: summary.stat selects subset of stats", {
  df  <- setup_wage1_summary()
  out <- stargazer(df[, 1:2], type = "latex", summary.stat = c("n", "mean"))
  expect_match(out,  "\\multicolumn{1}{c}{N}",    fixed = TRUE)
  expect_match(out,  "\\multicolumn{1}{c}{Mean}", fixed = TRUE)
  expect_false(grepl("St. Dev.", out, fixed = TRUE))
  expect_false(grepl("Min",      out, fixed = TRUE))
})

test_that("summary: covariate.labels renames variables", {
  df  <- setup_wage1_summary()
  out <- stargazer(df[, 1:2], type = "latex",
                   covariate.labels = c("Log Wage", "Education"))
  expect_match(out, "Log Wage",  fixed = TRUE)
  expect_match(out, "Education", fixed = TRUE)
})

test_that("summary: omit removes matching variable", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex", omit = "female")
  expect_false(grepl("female", out, fixed = TRUE))
  expect_match(out, "lwage", fixed = TRUE)
})

test_that("summary: keep retains only matching variables", {
  df  <- setup_wage1_summary()
  out <- stargazer(df, type = "latex", keep = c("educ", "exper"))
  expect_match(out,  "educ",    fixed = TRUE)
  expect_match(out,  "exper",   fixed = TRUE)
  expect_false(grepl("lwage",   out, fixed = TRUE))
  expect_false(grepl("married", out, fixed = TRUE))
})

test_that("summary: digits=2 formats to 2 decimal places", {
  df  <- setup_wage1_summary()
  out <- stargazer(df[, 1L, drop = FALSE], type = "latex", digits = 2L)
  mean_2dp <- formatC(mean(df$lwage), digits = 2L, format = "f")
  expect_match(out, mean_2dp, fixed = TRUE)
})
