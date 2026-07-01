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
#' @param coef.rename Named character vector mapping raw coefficient names to
#'   new display names before the column union is computed.  Names are the
#'   existing coefficient names; values are the replacement names.  Useful for
#'   aligning TWFE event-study year indicators with relative-time labels from
#'   other estimators, e.g.
#'   \code{c("year::2004" = "t = -2", "year::2005" = "t = -1")}.
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
#'   Only affects \code{style = "stargazer"}.  Default: \code{FALSE}.
#' @param style Character; table formatting style.  Controls overall layout,
#'   rule style, notes format, and significance-note wording.
#'   \describe{
#'     \item{\code{"stargazer2"}}{(Default) Clean layout: single \code{\\hline}
#'       throughout, no \code{\\cline}, full-width left-aligned notes cell
#'       ending in a period, p-value significance legend.}
#'     \item{\code{"stargazer"}}{Replicates the original \pkg{stargazer}
#'       output byte-for-byte: double top rule with \code{\\\\{[}-1.8ex{]}},
#'       \code{\\cline}, blank spacer rows (controlled by \code{no.space}),
#'       right-aligned notes.}
#'     \item{\code{"aer"}}{American Economic Review style: clean layout, no
#'       \dQuote{Dependent variable:} caption, single bottom rule, left-aligned
#'       notes with text significance descriptions.}
#'     \item{\code{"qje"}}{Quarterly Journal of Economics style: like
#'       \code{"aer"} but double bottom rule, right-aligned notes, and
#'       \code{\\textit\{N\}} for the observations label.}
#'   }
#' @param obs.label Character; overrides the label for the Observations row.
#'   Defaults to the style preset (\code{"Observations"} for most styles,
#'   \code{"\\textit\{N\}"} for \code{"qje"}).
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
#' m1 <- lm(mpg ~ cyl + hp, mtcars)
#' m2 <- lm(mpg ~ cyl + hp + wt, mtcars)
#' stargazer(m1, m2, type = "text")
#' stargazer(m1, m2, type = "latex")
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
                      coef.rename      = NULL,
                      omit             = NULL,
                      keep             = NULL,
                      omit.stat        = NULL,
                      digits           = 3L,
                      star.cutoffs     = c(0.1, 0.05, 0.01),
                      star.char        = c("*", "**", "***"),
                      notes            = NULL,
                      notes.append     = TRUE,
                      notes.align      = NULL,
                      notes.label      = NULL,
                      font.size        = NULL,
                      no.space         = FALSE,
                      style            = "stargazer2",
                      obs.label        = NULL,
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

  # Accept a pre-packed list as the first argument (common pattern).
  # Do NOT unwrap known model/result classes.
  if (length(models) == 1L && is.list(models[[1L]]) &&
      !inherits(models[[1L]], c("lm", "fixest", "feglm", "summary.feglm"))) {
    models <- models[[1L]]
  }

  n_models <- length(models)

  # --- Capture unevaluated ... and vcov expressions for SE label parsing ---
  # sandwich vcov matrices and summary.feglm objects carry no SE type metadata;
  # we recover it by inspecting the unevaluated call.
  mc_dots <- match.call(expand.dots = FALSE)$`...`

  vcov_parsed_labels  <- parse_vcov_labels(
    match.call(expand.dots = FALSE)$vcov,
    n_models
  )
  model_parsed_labels <- parse_summary_feglm_labels(mc_dots, n_models)

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

  # --- Apply expression-parsed SE labels (before user se_label override) ---
  # vcov labels: replace "user-specified standard errors" when recognisable.
  # model labels (summary.feglm): replace "MLE standard errors" placeholder.
  for (i in seq_len(n_models)) {
    vlbl <- vcov_parsed_labels[i]
    if (!is.na(vlbl) && records[[i]]$se_label == "user-specified standard errors") {
      records[[i]]$se_label <- vlbl
    }
    mlbl <- model_parsed_labels[i]
    if (!is.na(mlbl) &&
        records[[i]]$se_label == "SE type not detected (use se_label= to specify)" &&
        inherits(models[[i]], "summary.feglm")) {
      records[[i]]$se_label <- mlbl
    }
  }

  # --- Resolve style defaults for user-nullable parameters ---
  sd <- get_style_defaults(style)
  if (is.null(notes.align)) notes.align <- sd$notes.align
  if (is.null(notes.label)) notes.label <- sd$notes.label
  if (is.null(obs.label))   obs.label   <- sd$obs.label

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
  # dep.var.labels maps to unique dep-var names in order of first occurrence,
  # matching original stargazer behaviour.  A single label relabels all models
  # that share the same underlying dep var name.
  if (!is.null(dep.var.labels)) {
    unique_dvs <- unique(vapply(records, `[[`, character(1L), "dep_var"))
    n_labels   <- min(length(dep.var.labels), length(unique_dvs))
    dv_map     <- stats::setNames(dep.var.labels[seq_len(n_labels)],
                                  unique_dvs[seq_len(n_labels)])
    for (i in seq_len(n_models)) {
      dv <- records[[i]]$dep_var
      if (!is.na(dv_map[dv])) records[[i]]$dep_var <- dv_map[[dv]]
    }
  }

  # --- Format table ---
  table_data <- format_table(
    records,
    covariate.labels = covariate.labels,
    coef.rename      = coef.rename,
    omit             = omit,
    keep             = keep,
    omit.stat        = omit.stat,
    digits           = digits,
    star.cutoffs     = star.cutoffs,
    star.char        = star.char,
    no.space         = no.space,
    obs.label        = obs.label
  )

  # --- Default dep.var.caption: style can suppress it (empty string), or
  #     the user can override; NULL falls through to the type-based default ---
  if (is.null(dep.var.caption)) {
    dep.var.caption <- sd$dep.var.caption   # may be "" (suppressed) or NULL
  }
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
      notes.label     = notes.label,
      style           = style
    ),
    text = render_ascii(
      table_data,
      title           = title,
      dep.var.caption = dep.var.caption,
      column.labels   = column.labels,
      notes           = notes,
      notes.append    = notes.append
    ),
    html = render_html(
      table_data,
      title           = title,
      dep.var.caption = dep.var.caption,
      column.labels   = column.labels,
      notes           = notes,
      notes.append    = notes.append,
      notes.label     = notes.label
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
# Internal helpers: parse SE labels from unevaluated model expressions
# (summary.feglm objects)
# ---------------------------------------------------------------------------

# Inspect the unevaluated ... expressions and return a character vector of
# length n_models.  For positions where the expression is a summary() call
# on a feglm model, return the inferred SE label; otherwise NA_character_.
parse_summary_feglm_labels <- function(dots_exprs, n_models) {
  labels <- rep(NA_character_, n_models)
  if (is.null(dots_exprs) || length(dots_exprs) == 0L) return(labels)

  for (i in seq_len(min(length(dots_exprs), n_models))) {
    expr <- dots_exprs[[i]]
    lbl  <- parse_single_summary_feglm_call(expr)
    if (!is.na(lbl)) labels[i] <- lbl
  }
  labels
}

# Parse one unevaluated model expression.  Returns an SE label string when the
# expression is a summary() call, NA_character_ otherwise.
parse_single_summary_feglm_call <- function(expr) {
  if (!is.call(expr)) return(NA_character_)
  fn <- as.character(expr[[1L]])
  fn <- sub("^.*::", "", fn)
  if (fn != "summary") return(NA_character_)

  type_arg    <- expr[["type"]]
  cluster_arg <- expr[["cluster"]]

  type_str <- if (is.null(type_arg)) "hessian" else as.character(type_arg)

  if (type_str %in% c("hessian", "")) {
    return("MLE standard errors")
  }
  if (type_str == "outer.product") {
    return("outer-product standard errors")
  }
  if (type_str == "sandwich") {
    return("heteroskedasticity-robust standard errors")
  }
  if (type_str == "clustered") {
    if (!is.null(cluster_arg)) {
      vars <- all.vars(cluster_arg)
      # '^' in alpaca cluster formula means interaction (like fixest)
      # all.vars() drops the '^' operator and returns bare variable names
      if (length(vars) == 1L) {
        return(paste0("standard errors clustered by ", vars))
      }
      # Two or more vars: check whether they're interacted (^) or additive (+)
      cl_dep <- deparse(cluster_arg)
      if (grepl("\\^", cl_dep)) {
        cl_str <- paste(vars, collapse = "-")
      } else {
        cl_str <- paste0(paste(vars[-length(vars)], collapse = ", "),
                         " and ", vars[length(vars)])
      }
      return(paste0("standard errors clustered by ", cl_str))
    }
    return("clustered standard errors")
  }

  NA_character_
}

# ---------------------------------------------------------------------------
# Internal helpers: parse SE type labels from unevaluated vcov expressions
# ---------------------------------------------------------------------------

# Parse the unevaluated vcov argument expression (as returned by match.call())
# and return a character vector of length n_models with recognised SE labels,
# or NA where the call is not recognisable.
parse_vcov_labels <- function(vcov_expr, n_models) {
  labels <- rep(NA_character_, n_models)
  if (is.null(vcov_expr)) return(labels)

  # Bare matrix (single-model call without list() wrapper)
  if (!is.call(vcov_expr) ||
      !identical(as.character(vcov_expr[[1L]]), "list")) {
    labels[1L] <- parse_single_vcov_call(vcov_expr)
    return(labels)
  }

  # list(expr1, expr2, ...) — iterate over elements
  elems <- as.list(vcov_expr)[-1L]   # drop the 'list' symbol
  for (i in seq_along(elems)) {
    if (i > n_models) break
    labels[i] <- parse_single_vcov_call(elems[[i]])
  }
  labels
}

# Parse a single unevaluated vcov call expression and return a human-readable
# SE label, or NA_character_ if the call is unrecognised.
parse_single_vcov_call <- function(expr) {
  if (is.null(expr)) return(NA_character_)
  if (is.name(expr) && identical(as.character(expr), "NULL")) return(NA_character_)
  if (!is.call(expr)) return(NA_character_)

  fn <- as.character(expr[[1L]])
  fn <- sub("^.*::", "", fn)   # strip namespace prefix (e.g. sandwich::vcovHC)

  if (fn == "vcovHC") {
    type_arg <- expr[["type"]]
    if (!is.null(type_arg)) {
      return(paste0(as.character(type_arg),
                    " heteroskedasticity-robust standard errors"))
    }
    return("heteroskedasticity-robust standard errors")
  }

  if (fn == "vcovCL") {
    cluster_arg <- expr[["cluster"]]
    if (!is.null(cluster_arg)) {
      vars <- all.vars(cluster_arg)
      cl_str <- if (length(vars) == 1L) {
        vars
      } else if (grepl("\\^", deparse(cluster_arg))) {
        paste(vars, collapse = "-")
      } else {
        paste0(paste(vars[-length(vars)], collapse = ", "),
               " and ", vars[length(vars)])
      }
      return(paste0("standard errors clustered by ", cl_str))
    }
    return("clustered standard errors")
  }

  NA_character_
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

# ---------------------------------------------------------------------------
# Internal helper: style-specific parameter defaults
# ---------------------------------------------------------------------------

get_style_defaults <- function(style) {
  switch(style,
    stargazer2 = list(
      dep.var.caption = NULL,
      notes.align     = "l",
      notes.label     = "\\textit{Note:} ",
      obs.label       = "Observations"
    ),
    stargazer = list(
      dep.var.caption = NULL,
      notes.align     = "r",
      notes.label     = "\\textit{Note:} ",
      obs.label       = "Observations"
    ),
    aer = list(
      dep.var.caption = "",
      notes.align     = "l",
      notes.label     = "\\textit{Notes:} ",
      obs.label       = "Observations"
    ),
    qje = list(
      dep.var.caption = "",
      notes.align     = "r",
      notes.label     = "\\textit{Notes:} ",
      obs.label       = "\\textit{N}"
    ),
    stop("stargazer: unknown style '", style,
         "'. Must be one of: 'stargazer2', 'stargazer', 'aer', 'qje'.",
         call. = FALSE)
  )
}
