#' Produce Publication-Quality Regression Tables
#'
#' A drop-in replacement for the \pkg{stargazer} package that supports
#' modern econometrics packages including \pkg{fixest} and \pkg{alpaca}.
#' Outputs LaTeX, plain-text (ASCII), and HTML tables.
#'
#' @param ...  One or more fitted model objects.  Supported classes:
#'   \code{lm}, \code{fixest}.
#'
#' @param type Character; output format.  One of \code{"latex"} (default),
#'   \code{"text"}, or \code{"html"}.
#'
#' @param title Character; table caption.
#' @param label Character; LaTeX cross-reference label (used with
#'   \code{type = "latex"} only).
#' @param dep.var.labels Character vector; custom labels for the dependent
#'   variable(s).  Applied in column order.
#' @param dep.var.caption Character; caption text above the dep-var row.
#'   Default: \code{"\\textit\{Dependent variable:\}"} for LaTeX.
#' @param column.labels Character vector; custom column header labels.
#'   Overrides the auto-detected model-type labels.
#' @param column.separate Integer vector; controls how many consecutive
#'   columns share each \code{column.labels} entry.  Unused in the current
#'   version.
#' @param covariate.labels Character vector; display names for covariates,
#'   applied positionally after \code{omit}/\code{keep} filtering.
#' @param omit Character vector; regex patterns — covariates whose names
#'   match any pattern are excluded from the table.
#' @param keep Character vector; regex patterns — only covariates whose
#'   names match are included (applied after \code{omit}).
#' @param omit.stat Character vector; stat identifiers to suppress.
#'   Recognised values: \code{"n"}, \code{"r2"}, \code{"adj.r2"},
#'   \code{"sigma"}, \code{"f"}, \code{"pr2"}, \code{"ll"}.
#' @param digits Integer; number of decimal places for coefficients, SEs,
#'   and most fit statistics.  Default: \code{3}.
#' @param star.cutoffs Numeric vector of length 3; p-value thresholds for
#'   one, two, and three stars.  Default: \code{c(0.1, 0.05, 0.01)}.
#' @param star.char Character vector of length 3; star symbols.  Default:
#'   \code{c("*", "**", "***")}.
#' @param notes Character vector; additional table notes appended after the
#'   SE-type note.
#' @param notes.append Logical; if \code{FALSE}, the default SE-type and
#'   significance notes are replaced by \code{notes}.  Default: \code{TRUE}.
#' @param notes.align Character; cell alignment for the note row in LaTeX
#'   (\code{"l"}, \code{"c"}, or \code{"r"}).  Default: \code{"r"}.
#' @param notes.label Character; text preceding the note.  Default:
#'   \code{"\\textit\{Note:\} "}.
#' @param font.size Character; LaTeX font-size command (e.g.
#'   \code{"small"}, \code{"footnotesize"}).  \code{NULL} (default) means no
#'   size command is inserted.
#' @param no.space Logical; suppress blank spacer rows in the table body.
#'   Default: \code{FALSE}.
#'
#' @param summary.stat Character vector; which summary statistics to include
#'   when \code{stargazer} is called with a \code{data.frame}.  Recognised
#'   values: \code{"n"}, \code{"mean"}, \code{"sd"}, \code{"min"},
#'   \code{"max"}, \code{"median"}, \code{"p25"}, \code{"p75"}.
#'   Default: \code{c("n","mean","sd","min","max")}.
#' @param median Logical; if \code{TRUE}, add a Median column to summary
#'   tables.  Equivalent to including \code{"median"} in \code{summary.stat}.
#'   Default: \code{FALSE}.
#'
#' @param vcov List of variance-covariance matrices (one per model, or
#'   \code{NULL} for a given model to fall back to auto-extraction).  Takes
#'   priority over \code{se}.  The square root of the diagonal is extracted
#'   internally.
#' @param se   List of numeric vectors of standard errors (one per model,
#'   or \code{NULL} to fall back to auto-extraction).  Used only when
#'   \code{vcov} is not supplied for a given model.
#' @param se_label Character vector (one per model, or a single string
#'   applied to all) overriding the auto-detected SE-type description used
#'   in the table note.  Useful when supplying a \code{vcov} matrix whose
#'   type cannot be detected automatically.  Examples:
#'   \code{"Heteroskedasticity-robust standard errors (HC1)"},
#'   \code{"Standard errors clustered by firm"}.
#'
#' @param out Character; file path.  If provided, output is written to the
#'   file (appending the appropriate extension if absent).  The table string
#'   is also returned invisibly.
#'
#' @return The rendered table as a single character string, returned
#'   invisibly.  Also printed to the console unless \code{out} is specified.
#'
#' @examples
#' \dontrun{
#' m1 <- lm(mpg ~ cyl + hp, mtcars)
#' m2 <- lm(mpg ~ cyl + hp + wt, mtcars)
#' stargazer(m1, m2)
#' stargazer(m1, m2, type = "text")
#' }
#'
#' @export
stargazer <- function(...,
                      type             = "latex",
                      title            = "",
                      label            = "",
                      dep.var.labels   = NULL,
                      dep.var.caption  = NULL,
                      column.labels    = NULL,
                      column.separate  = NULL,
                      covariate.labels = NULL,
                      omit             = NULL,
                      keep             = NULL,
                      omit.stat        = NULL,
                      digits           = 3L,
                      star.cutoffs     = c(0.1, 0.05, 0.01),
                      star.char        = c("*", "**", "***"),
                      notes            = NULL,
                      notes.append     = TRUE,
                      notes.align      = "r",
                      notes.label      = "\\textit{Note:} ",
                      font.size        = NULL,
                      no.space         = FALSE,
                      summary.stat     = NULL,
                      median           = FALSE,
                      vcov             = NULL,
                      se               = NULL,
                      se_label         = NULL,
                      out              = NULL) {

  models <- list(...)
  if (length(models) == 0L) {
    stop("stargazer: no model objects supplied.", call. = FALSE)
  }

  # --- Data frame / matrix input: route to summary statistics table ---
  # Must be checked before the pre-packed list unwrap below, because
  # data.frame objects are also lists and would otherwise be unpacked.
  if (length(models) >= 1L &&
      (is.data.frame(models[[1L]]) || is.matrix(models[[1L]]))) {
    return(stargazer_summary(
      data             = models[[1L]],
      type             = type,
      title            = title,
      label            = label,
      font.size        = font.size,
      covariate.labels = covariate.labels,
      omit             = omit,
      keep             = keep,
      digits           = digits,
      summary.stat     = summary.stat,
      median           = median,
      notes            = notes,
      notes.append     = notes.append,
      notes.align      = notes.align,
      notes.label      = notes.label,
      out              = out
    ))
  }

  # Accept a pre-packed list as the first argument (common pattern)
  if (length(models) == 1L && is.list(models[[1L]]) &&
      !inherits(models[[1L]], c("lm", "fixest"))) {
    models <- models[[1L]]
  }

  n_models <- length(models)

  # --- Validate and normalise vcov / se lists ---
  vcov_list <- normalise_override_list(vcov, n_models, "vcov")
  se_list   <- normalise_override_list(se,   n_models, "se")

  # --- Extract model records ---
  records <- vector("list", n_models)
  for (i in seq_len(n_models)) {
    records[[i]] <- extract_model(
      models[[i]],
      vcov_override = vcov_list[[i]],
      se_override   = se_list[[i]]
    )
  }

  # --- Apply se_label override ---
  if (!is.null(se_label)) {
    if (length(se_label) == 1L) se_label <- rep(se_label, n_models)
    for (i in seq_len(min(length(se_label), n_models))) {
      if (!is.na(se_label[i]) && nchar(se_label[i]) > 0L) {
        records[[i]]$se_label <- se_label[i]
      }
    }
  }

  # --- Apply dep.var.labels override ---
  if (!is.null(dep.var.labels)) {
    n_labels <- min(length(dep.var.labels), n_models)
    for (i in seq_len(n_labels)) {
      records[[i]]$dep_var <- dep.var.labels[[i]]
    }
  }

  # --- Format table ---
  table_data <- format_table(
    records,
    covariate.labels = covariate.labels,
    omit             = omit,
    keep             = keep,
    omit.stat        = omit.stat,
    digits           = digits,
    star.cutoffs     = star.cutoffs,
    star.char        = star.char,
    no.space         = no.space
  )

  # --- Default dep.var.caption per output type ---
  if (is.null(dep.var.caption)) {
    dep.var.caption <- switch(type,
      latex = "\\textit{Dependent variable:}",
      text  = "Dependent variable:",
      html  = "Dependent variable:",
      "Dependent variable:"
    )
  }

  # --- Render ---
  output <- switch(type,
    latex = render_latex(
      table_data,
      title           = title,
      label           = label,
      dep.var.caption = dep.var.caption,
      column.labels   = column.labels,
      font.size       = font.size,
      notes           = notes,
      notes.append    = notes.append,
      notes.align     = notes.align,
      notes.label     = notes.label
    ),
    text = render_ascii(
      table_data,
      title           = title,
      dep.var.caption = dep.var.caption,
      column.labels   = column.labels,
      notes           = notes,
      notes.append    = notes.append
    ),
    stop("stargazer: type must be one of 'latex', 'text', 'html'.", call. = FALSE)
  )

  # --- Output ---
  if (!is.null(out)) {
    writeLines(output, con = out)
  } else {
    cat(output, "\n")
  }

  invisible(output)
}

# ---------------------------------------------------------------------------
# Internal helper: normalise vcov / se argument to a list of length n
# ---------------------------------------------------------------------------

normalise_override_list <- function(x, n, arg_name) {
  if (is.null(x)) return(vector("list", n))

  # Allow a bare matrix / vector to be passed for single-model calls
  if (!is.list(x)) {
    if (n == 1L) {
      return(list(x))
    }
    stop("stargazer: '", arg_name, "' must be a list when multiple models are supplied.",
         call. = FALSE)
  }

  if (length(x) != n) {
    stop("stargazer: '", arg_name, "' must have one entry per model (",
         n, " model(s) supplied, ", length(x), " entry/entries in '",
         arg_name, "').", call. = FALSE)
  }

  x
}
