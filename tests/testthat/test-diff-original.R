# Line-by-line diff tests comparing stargazer2 output against the original
# stargazer package.
#
# Each test captures output from both packages and compares line by line,
# reporting exact differences.  Exceptions are documented per test.

# ---------------------------------------------------------------------------
# Setup helpers
# ---------------------------------------------------------------------------

setup_wage1_diff <- function() {
  skip_if_not_installed("wooldridge")
  skip_if_not_installed("stargazer")
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

# Capture the original stargazer output by temporarily detaching stargazer2,
# loading the original package, evaluating the call, then restoring stargazer2.
#
# The original stargazer uses match.call() to resolve variable names, so it
# breaks when called via do.call().  We work around this by assigning models
# to a temporary environment with known names and eval()ing a string call.
capture_original <- function(models, type = "latex") {
  if ("package:stargazer2" %in% search()) {
    detach("package:stargazer2", unload = FALSE, character.only = TRUE)
  }
  suppressMessages(library(stargazer, warn.conflicts = FALSE))

  env <- new.env(parent = globalenv())
  nms <- paste0(".m", seq_along(models))
  for (i in seq_along(models)) assign(nms[i], models[[i]], envir = env)
  call_str <- paste0("stargazer(", paste(nms, collapse = ", "),
                     ", type = '", type, "')")
  out <- capture.output(eval(parse(text = call_str), envir = env))

  detach("package:stargazer", unload = FALSE, character.only = TRUE)
  suppressMessages(library(stargazer2, warn.conflicts = FALSE))
  out
}

# Filter lines: drop % comment lines (original stargazer header),
# and optionally drop note lines (which intentionally differ: stargazer2
# always shows the SE type in the note, the original stargazer suppresses
# the note for default OLS).
filter_lines <- function(lines, drop_whitespace_only = FALSE,
                         drop_note = FALSE,
                         normalise_ws = FALSE) {
  lines <- lines[!grepl("^%", lines)]           # drop % comment header
  lines <- lines[lines != ""]                    # drop leading/trailing blank lines
  if (drop_whitespace_only) {
    lines <- trimws(lines, which = "right")      # trailing spaces don't matter
  }
  if (drop_note) {
    # LaTeX: drop lines containing "Note:"
    # ASCII: drop the closing === border and everything after it, since the
    # note (potentially multi-line after word-wrapping) always follows it.
    latex_note <- grepl("Note:", lines, fixed = TRUE)
    last_dbl   <- max(c(0L, which(grepl("^={3,}", lines))))
    after_dbl  <- seq_len(length(lines)) > last_dbl
    lines <- lines[!(latex_note | after_dbl)]
  }
  if (normalise_ws) {
    # Collapse runs of spaces to one and trim; makes table-width differences
    # (caused by the longer SE note expanding column widths) transparent.
    lines <- trimws(gsub(" +", " ", lines))
    # Drop lines that collapse to pure separator characters (===, ---) since
    # their length is no longer meaningful after whitespace stripping.
    lines <- lines[!grepl("^[= ]+$|^[- ]+$", lines)]
  }
  lines
}

# Build a human-readable diff report between two character vectors.
diff_report <- function(orig, new2, label_orig = "stargazer", label_new = "stargazer2") {
  n <- max(length(orig), length(new2))
  orig <- c(orig, rep(NA_character_, n - length(orig)))
  new2 <- c(new2, rep(NA_character_, n - length(new2)))
  diffs <- which(is.na(orig) | is.na(new2) | orig != new2)
  if (length(diffs) == 0L) return(NULL)
  lines <- character(0L)
  for (i in diffs) {
    lines <- c(lines,
      sprintf("Line %d:", i),
      sprintf("  %-12s [%s]", paste0(label_orig, ":"), if (is.na(orig[i])) "<missing>" else orig[i]),
      sprintf("  %-12s [%s]", paste0(label_new,  ":"), if (is.na(new2[i])) "<missing>" else new2[i])
    )
  }
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# 1. ASCII single-model diff test
# ---------------------------------------------------------------------------

test_that("diff: ASCII single-model output matches original stargazer", {
  wage1 <- setup_wage1_diff()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)

  orig <- capture_original(list(m1), type = "text")
  new2 <- strsplit(stargazer(m1, type = "text"), "\n")[[1L]]

  # drop_note + normalise_ws: stargazer2 intentionally shows "OLS standard
  # errors" in the note, which widens the table.  Drop notes and normalise
  # whitespace so only content (not alignment) is compared.
  orig_f <- filter_lines(orig, drop_note = TRUE, normalise_ws = TRUE)
  new2_f <- filter_lines(new2, drop_note = TRUE, normalise_ws = TRUE)

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("ASCII single-model diff:\n", report))
})

# ---------------------------------------------------------------------------
# 2. ASCII multi-model diff test
# ---------------------------------------------------------------------------

test_that("diff: ASCII multi-model output (m1-m4) matches original stargazer", {
  wage1 <- setup_wage1_diff()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  m2    <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  m3    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation, wage1)
  m4    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation + industry, wage1)

  orig <- capture_original(list(m1, m2, m3, m4), type = "text")
  new2 <- strsplit(stargazer(m1, m2, m3, m4, type = "text"), "\n")[[1L]]

  orig_f <- filter_lines(orig, drop_note = TRUE, normalise_ws = TRUE)
  new2_f <- filter_lines(new2, drop_note = TRUE, normalise_ws = TRUE)

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("ASCII multi-model diff:\n", report))
})

# ---------------------------------------------------------------------------
# 3. LaTeX single-model diff test
# ---------------------------------------------------------------------------

test_that("diff: LaTeX single-model output matches original stargazer", {
  wage1 <- setup_wage1_diff()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)

  orig <- capture_original(list(m1), type = "latex")
  new2 <- strsplit(stargazer(m1, type = "latex"), "\n")[[1L]]

  # Drop % comments; right-strip trailing spaces; treat whitespace-only lines
  # as equivalent; drop Note: lines (intentional divergence: stargazer2 always
  # shows SE type in note).
  normalise_latex <- function(lines) {
    lines <- lines[!grepl("^%", lines)]
    lines <- lines[!grepl("Note:", lines, fixed = TRUE)]
    lines <- trimws(lines, which = "right")
    ifelse(grepl("^\\s*$", lines), "", lines)
  }

  orig_f <- normalise_latex(orig)
  new2_f <- normalise_latex(new2)

  # Drop leading/trailing empty lines
  orig_f <- orig_f[orig_f != "" | cumsum(orig_f != "") > 0L]
  new2_f <- new2_f[new2_f != "" | cumsum(new2_f != "") > 0L]

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("LaTeX single-model diff:\n", report))
})

# ---------------------------------------------------------------------------
# 4. LaTeX multi-model diff test  (most important)
# ---------------------------------------------------------------------------

test_that("diff: LaTeX multi-model output (m1-m4) matches original stargazer", {
  wage1 <- setup_wage1_diff()
  m1    <- lm(lwage ~ educ + exper + tenure, wage1)
  m2    <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
  m3    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation, wage1)
  m4    <- lm(lwage ~ educ + exper + tenure + female + married +
                region + occupation + industry, wage1)

  orig <- capture_original(list(m1, m2, m3, m4), type = "latex")
  new2 <- strsplit(stargazer(m1, m2, m3, m4, type = "latex"), "\n")[[1L]]

  normalise_latex <- function(lines) {
    lines <- lines[!grepl("^%", lines)]
    lines <- lines[!grepl("Note:", lines, fixed = TRUE)]
    lines <- trimws(lines, which = "right")
    ifelse(grepl("^\\s*$", lines), "", lines)
  }

  orig_f <- normalise_latex(orig)
  new2_f <- normalise_latex(new2)

  orig_f <- orig_f[orig_f != "" | cumsum(orig_f != "") > 0L]
  new2_f <- new2_f[new2_f != "" | cumsum(new2_f != "") > 0L]

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("LaTeX multi-model diff:\n", report))
})

# ---------------------------------------------------------------------------
# 5. Summary table ASCII diff test
# ---------------------------------------------------------------------------

test_that("diff: ASCII summary table matches original stargazer", {
  wage1 <- setup_wage1_diff()
  df    <- wage1[, c("lwage", "educ", "exper", "tenure", "female", "married")]

  orig <- capture_original(list(df), type = "text")
  new2 <- strsplit(stargazer(df, type = "text"), "\n")[[1L]]

  orig_f <- filter_lines(orig)
  new2_f <- filter_lines(new2)

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("ASCII summary diff:\n", report))
})

# ---------------------------------------------------------------------------
# 6. Summary table LaTeX diff test
# ---------------------------------------------------------------------------

test_that("diff: LaTeX summary table matches original stargazer", {
  wage1 <- setup_wage1_diff()
  df    <- wage1[, c("lwage", "educ", "exper", "tenure", "female", "married")]

  orig <- capture_original(list(df), type = "latex")
  new2 <- strsplit(stargazer(df, type = "latex"), "\n")[[1L]]

  normalise_latex <- function(lines) {
    lines <- lines[!grepl("^%", lines)]
    lines <- trimws(lines, which = "right")
    ifelse(grepl("^\\s*$", lines), "", lines)
  }

  orig_f <- normalise_latex(orig)
  new2_f <- normalise_latex(new2)

  orig_f <- orig_f[orig_f != "" | cumsum(orig_f != "") > 0L]
  new2_f <- new2_f[new2_f != "" | cumsum(new2_f != "") > 0L]

  report <- diff_report(orig_f, new2_f)
  expect_null(report, label = paste("LaTeX summary diff:\n", report))
})
