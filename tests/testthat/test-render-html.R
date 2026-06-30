# Structural tests for the HTML renderer (render_html).
#
# stargazer2's HTML is clean semantic markup, not a byte-for-byte match of the
# original stargazer HTML, so these tests check structure and escaping rather
# than diffing against the original package.

setup_wage1_html <- function() {
  skip_if_not_installed("wooldridge")
  data("wage1", package = "wooldridge")
  wage1
}

# ---------------------------------------------------------------------------
# Basic structure
# ---------------------------------------------------------------------------

test_that("render_html: emits a single well-formed table skeleton", {
  wage1 <- setup_wage1_html()
  m1 <- lm(lwage ~ educ + exper, wage1)
  m2 <- lm(lwage ~ educ + exper + tenure, wage1)
  out <- stargazer(m1, m2, type = "html")

  expect_equal(lengths(regmatches(out, gregexpr("<table",  out)))[[1]], 1L)
  expect_equal(lengths(regmatches(out, gregexpr("</table>", out)))[[1]], 1L)
  expect_match(out, "<thead>");  expect_match(out, "</thead>")
  expect_match(out, "<tbody>");  expect_match(out, "</tbody>")
  expect_match(out, "<tfoot>");  expect_match(out, "</tfoot>")
  # Every opened row is closed
  n_open  <- lengths(regmatches(out, gregexpr("<tr",  out)))[[1]]
  n_close <- lengths(regmatches(out, gregexpr("</tr>", out)))[[1]]
  expect_equal(n_open, n_close)
})

test_that("render_html: coefficients, stars, and R-squared render as HTML", {
  wage1 <- setup_wage1_html()
  m <- lm(lwage ~ educ + exper, wage1)
  out <- stargazer(m, type = "html")

  expect_match(out, "0.098<sup>***</sup>", fixed = TRUE)  # coef with stars
  expect_match(out, "(0.008)", fixed = TRUE)              # SE
  expect_match(out, "R<sup>2</sup>", fixed = TRUE)        # R-squared label
  # Significance legend with escaped < operator
  expect_match(out, "<sup>*</sup>p&lt;0.1", fixed = TRUE)
})

test_that("render_html: single-model table omits the column-number row", {
  wage1 <- setup_wage1_html()
  m <- lm(lwage ~ educ, wage1)
  out <- stargazer(m, type = "html")
  expect_false(grepl("(1)", out, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Fixed effects
# ---------------------------------------------------------------------------

test_that("render_html: feols FE indicator row appears", {
  skip_if_not_installed("fixest")
  wage1 <- setup_wage1_html()
  wage1$region <- factor(ifelse(wage1$west == 1, "west", "other"))
  f <- fixest::feols(lwage ~ educ + exper | region, wage1)
  out <- stargazer(f, type = "html")
  expect_match(out, ">Region FE</td>")
  expect_match(out, ">Yes</td>")
})

# ---------------------------------------------------------------------------
# Escaping
# ---------------------------------------------------------------------------

test_that("render_html: special characters are HTML-escaped", {
  wage1 <- setup_wage1_html()
  m <- lm(lwage ~ educ + exper, wage1)
  out <- stargazer(m, type = "html",
    title = "R&D (p < 0.05)",
    covariate.labels = c("Educ <yrs>", "Exper & age"),
    notes = "Source: A&B <x>.")

  expect_match(out, "R&amp;D (p &lt; 0.05)", fixed = TRUE)   # title
  expect_match(out, "Educ &lt;yrs&gt;",       fixed = TRUE)   # covariate label
  expect_match(out, "Exper &amp; age",        fixed = TRUE)   # covariate label
  expect_match(out, "Source: A&amp;B &lt;x&gt;.", fixed = TRUE) # custom note
  # No raw unescaped angle brackets from user text leaked through
  expect_false(grepl("Educ <yrs>", out, fixed = TRUE))
})

test_that("render_html: custom notes each render on their own row", {
  wage1 <- setup_wage1_html()
  m <- lm(lwage ~ educ, wage1)
  out <- stargazer(m, type = "html", notes = c("Note one.", "Note two."))
  expect_match(out, ">Note one.</td>")
  expect_match(out, ">Note two.</td>")
})

# ---------------------------------------------------------------------------
# latex_to_html unit checks
# ---------------------------------------------------------------------------

test_that("latex_to_html converts markup and escapes stray characters", {
  f <- stargazer2:::latex_to_html
  expect_equal(f("0.5$^{***}$"),  "0.5<sup>***</sup>")
  expect_equal(f("R$^{2}$"),       "R<sup>2</sup>")
  expect_equal(f("$-$0.5"),        "&minus;0.5")
  expect_equal(f("p$<$0.1"),       "p&lt;0.1")
  expect_equal(f("\\textit{Note:}"), "<em>Note:</em>")
  expect_equal(f("a\\_b"),         "a_b")          # LaTeX-escaped underscore
  expect_equal(f("A & B"),         "A &amp; B")     # stray ampersand
  expect_equal(f("x < y > z"),     "x &lt; y &gt; z")
})
