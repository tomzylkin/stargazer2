# Table-formatting layer for stargazer2.
#
# format_table() takes a list of model_records (from extract.R) plus
# user-supplied formatting options and returns a table_data list that
# render_latex() / render_ascii() / render_html() consume.
#
# table_data fields
# -----------------
# n_cols         integer
# dep_vars       character[n_cols]
# model_labels   character[n_cols]  estimator names
# col_numbers    character[n_cols]  "(1)", "(2)", ...
# show_model_row logical            TRUE when model types or dep vars differ
# coef_rows      list of coef_row
# fe_rows        list of fe_row     empty list if no FEs
# stat_rows      list of stat_row
# se_notes       character          unique SE-type descriptions
# star_note      character          significance-level legend
# ci_col_widths  integer[n_cols]    minimum col width from CI bracket strings
#
# coef_row / fe_row / stat_row: list(label, values, se_values)
#   values and se_values are character vectors of length n_cols.
#   se_values is NULL for non-coefficient rows.
#   NA entries become "" in the rendered output.

# ---------------------------------------------------------------------------
# Main function
# ---------------------------------------------------------------------------

#' Assemble table data from a list of model records
#'
#' Internal; called by \code{\link{stargazer}}.
#'
#' @param records   List of model_record objects from \code{extract_model}.
#' @param covariate.labels Character vector of display names for covariates,
#'   applied positionally after \code{omit}/\code{keep} filtering.
#' @param coef.rename Named character vector mapping raw coefficient names to
#'   display names before the union is computed (e.g.
#'   \code{c("year::2004" = "t = -2")}). Applied to all models.
#' @param omit  Character vector of regex patterns; matching covariates are
#'   dropped.
#' @param keep  Character vector of regex patterns; only matching covariates
#'   are kept (applied after \code{omit}).
#' @param omit.stat Character vector of stat identifiers to suppress.
#' @param digits      Integer; decimal places for coefficients and SEs.
#' @param star.cutoffs Numeric vector of p-value thresholds (ascending).
#' @param star.char    Character vector of star strings.
#' @param no.space     Logical; suppress blank spacer rows.
#' @param obs.label    Character; label for the Observations row.  Default
#'   \code{"Observations"}; set to \code{"\\\\textit\{N\}"} for QJE style.
#' @keywords internal
format_table <- function(records,
                         covariate.labels = NULL,
                         coef.rename      = NULL,
                         omit             = NULL,
                         keep             = NULL,
                         omit.stat        = NULL,
                         digits           = 3L,
                         star.cutoffs     = c(0.1, 0.05, 0.01),
                         star.char        = c("*", "**", "***"),
                         no.space         = FALSE,
                         obs.label        = "Observations") {

  n_cols <- length(records)

  dep_vars     <- vapply(records, `[[`, character(1L), "dep_var")
  model_labels <- vapply(records, `[[`, character(1L), "model_label")
  col_numbers  <- paste0("(", seq_len(n_cols), ")")

  # Show an explicit model-type row when types or dep vars vary across columns
  show_model_row <- (length(unique(model_labels)) > 1L) ||
                    (length(unique(dep_vars)) > 1L)

  # --- Coefficient rows ---
  coef_rows <- build_coef_rows(
    records, omit, keep, covariate.labels, digits, star.cutoffs, star.char,
    coef.rename = coef.rename
  )

  # --- Fixed-effects indicator rows ---
  fe_rows <- build_fe_rows(records)

  # --- Fit-statistic rows ---
  stat_rows <- build_stat_rows(records, omit.stat, digits, star.cutoffs, star.char,
                               obs.label)

  # --- SE notes: keep full per-column vector for grouped note formatting ---
  se_labels <- vapply(records, `[[`, character(1L), "se_label")
  se_notes  <- se_labels

  # Standard significance note (p-value format, matches original stargazer)
  star_note <- paste0(
    "$^{*}$p$<$", star.cutoffs[1L], "; ",
    "$^{**}$p$<$", star.cutoffs[2L], "; ",
    "$^{***}$p$<$", star.cutoffs[3L]
  )

  # Text-format significance note for AER/QJE styles
  star_note_text <- paste(
    vapply(rev(seq_along(star.cutoffs)), function(i) {
      pct <- round(star.cutoffs[i] * 100)
      paste0("$^{", star.char[i], "}$Significant at the ", pct, " percent level.")
    }, character(1L)),
    collapse = " "
  )

  # Mixed SE/CI format legend (only when both types appear in the table)

  # --- Minimum column widths from CI bracket strings ---
  # format_ci() now uses sprintf() so these widths exactly match the stored
  # se_values strings.  Computed from raw bounds so that a CI-only record at
  # the bottom of the coef_rows list (e.g. staggered_result ATT) is accounted
  # for even before the row-scanning loop in compute_col_widths() reaches it.
  ci_col_widths <- integer(n_cols)
  for (i in seq_len(n_cols)) {
    rec <- records[[i]]
    if (!isTRUE(rec$use_ci) || length(rec$ci_lower) == 0L) next
    fmt <- sprintf("[%.*f, %.*f]", digits, rec$ci_lower, digits, rec$ci_upper)
    w   <- max(nchar(fmt), na.rm = TRUE)
    ci_col_widths[i] <- max(ci_col_widths[i], w, na.rm = TRUE)
  }

  list(
    n_cols          = n_cols,
    dep_vars        = dep_vars,
    model_labels    = model_labels,
    col_numbers     = col_numbers,
    show_model_row  = show_model_row,
    coef_rows       = coef_rows,
    fe_rows         = fe_rows,
    stat_rows       = stat_rows,
    se_notes        = se_notes,
    star_note       = star_note,
    star_note_text  = star_note_text,
    ci_col_widths   = ci_col_widths,
    no_space        = no.space
  )
}

# ---------------------------------------------------------------------------
# Coefficient rows
# ---------------------------------------------------------------------------

build_coef_rows <- function(records, omit, keep, covariate.labels,
                            digits, star.cutoffs, star.char,
                            coef.rename = NULL) {
  n_cols <- length(records)

  # Apply coef.rename: rename raw coefficient names before building the union.
  # This allows different models to share a display name (e.g. "year::2004"
  # renamed to "t = -2" to align with CS event-study labels "t = -2").
  if (!is.null(coef.rename)) {
    old_names <- names(coef.rename)
    new_names <- unname(coef.rename)
    for (i in seq_along(records)) {
      m     <- match(old_names, records[[i]]$coef_names)
      valid <- which(!is.na(m))
      if (length(valid) > 0L) {
        records[[i]]$coef_names[m[valid]] <- new_names[valid]
      }
    }
  }

  # Union of all covariate names, preserving encounter order
  all_names <- character(0L)
  for (rec in records) {
    new <- setdiff(rec$coef_names, all_names)
    all_names <- c(all_names, new)
  }

  # Apply omit / keep filters
  if (!is.null(omit)) {
    pattern <- paste(omit, collapse = "|")
    all_names <- all_names[!grepl(pattern, all_names)]
  }
  if (!is.null(keep)) {
    pattern <- paste(keep, collapse = "|")
    all_names <- all_names[grepl(pattern, all_names)]
  }

  # Sort event-time labels ("t = k") numerically so that periods appear in
  # chronological order even when different estimators omit some periods
  # (e.g. a reference period in one model that another model shows).
  t_mask <- grepl("^t = -?\\d+$", all_names)
  if (any(t_mask)) {
    t_vals    <- as.integer(sub("^t = ", "", all_names[t_mask]))
    all_names <- c(all_names[t_mask][order(t_vals)], all_names[!t_mask])
  }

  # Match original stargazer: move "Constant" to the end
  const_idx <- which(all_names == "Constant")
  if (length(const_idx) > 0L) {
    all_names <- c(all_names[-const_idx], all_names[const_idx])
  }

  # Build one row per covariate
  rows <- vector("list", length(all_names))
  for (j in seq_along(all_names)) {
    nm      <- all_names[[j]]
    vals    <- character(n_cols)
    se_vals <- character(n_cols)

    for (i in seq_len(n_cols)) {
      rec <- records[[i]]
      idx <- match(nm, rec$coef_names)
      if (is.na(idx)) {
        vals[i]    <- ""
        se_vals[i] <- ""
      } else {
        val_str  <- format_num(rec$coefs[idx], digits)
        val_str  <- add_stars(val_str, rec$pval[idx], star.cutoffs, star.char)
        vals[i]    <- val_str
        se_vals[i] <- if (isTRUE(rec$use_ci)) {
          format_ci(rec$ci_lower[idx], rec$ci_upper[idx], digits)
        } else {
          format_se(rec$se[idx], digits)
        }
      }
    }

    label <- latex_escape(nm)  # default display name
    rows[[j]] <- list(label = label, values = vals, se_values = se_vals)
  }

  # Apply covariate.labels (positional replacement)
  if (!is.null(covariate.labels)) {
    n_labels <- min(length(covariate.labels), length(rows))
    for (j in seq_len(n_labels)) {
      rows[[j]]$label <- covariate.labels[[j]]
    }
  }

  # Merge rows that share the same display label (fix 1).
  # This handles mixed SE/CI tables where "treated" → "ATT" (TWFE) and
  # the native "ATT" (DiD estimators) land on separate rows before merging.
  rows <- merge_same_label_rows(rows)

  rows
}

# Merge rows with identical display labels into a single row by combining
# non-empty cells.  First non-empty value wins for each column.
merge_same_label_rows <- function(rows) {
  if (length(rows) <= 1L) return(rows)
  label_seen <- character(0L)
  result     <- list()
  for (j in seq_along(rows)) {
    lbl <- rows[[j]]$label
    k   <- match(lbl, label_seen)
    if (!is.na(k)) {
      for (col in seq_along(rows[[j]]$values)) {
        if (nchar(result[[k]]$values[col]) == 0L &&
            nchar(rows[[j]]$values[col]) > 0L) {
          result[[k]]$values[col] <- rows[[j]]$values[col]
        }
        if (!is.null(rows[[j]]$se_values) &&
            nchar(result[[k]]$se_values[col]) == 0L &&
            nchar(rows[[j]]$se_values[col]) > 0L) {
          result[[k]]$se_values[col] <- rows[[j]]$se_values[col]
        }
      }
    } else {
      label_seen <- c(label_seen, lbl)
      result     <- c(result, list(rows[[j]]))
    }
  }
  result
}

# ---------------------------------------------------------------------------
# Fixed-effects indicator rows
# ---------------------------------------------------------------------------

build_fe_rows <- function(records) {
  # Collect all unique FE variable names in encounter order
  all_fes <- character(0L)
  for (rec in records) {
    new <- setdiff(rec$fixed_effects, all_fes)
    all_fes <- c(all_fes, new)
  }
  if (length(all_fes) == 0L) return(list())

  n_cols <- length(records)
  rows   <- vector("list", length(all_fes))

  for (j in seq_along(all_fes)) {
    fe_var <- all_fes[[j]]
    label  <- format_fe_label(fe_var)
    values <- vapply(records, function(rec) {
      if (fe_var %in% rec$fixed_effects) {
        "Yes"
      } else if (isTRUE(rec$reports_fe)) {
        # Model uses explicit FE absorption but not this particular variable
        "No"
      } else {
        # Model does not report FEs (e.g. CS, Roth-Sant'Anna):
        # leave blank rather than showing misleading "No"
        ""
      }
    }, character(1L))
    rows[[j]] <- list(label = label, values = values, se_values = NULL)
  }

  rows
}

# ---------------------------------------------------------------------------
# Fit-statistic rows
# ---------------------------------------------------------------------------

# Stat identifiers recognised by omit.stat:
#   "n"      Observations
#   "r2"     R² and Within R² (all model types)
#   "adj.r2" Adjusted R² and Adjusted Within R² (OLS only)
#   "sigma"  Residual Std. Error  (lm / Gaussian glm only)
#   "f"      F Statistic          (lm / Gaussian glm only)
#   "ll"     Log Likelihood       (glm only)
#   "aic"    Akaike Inf. Crit.    (glm only)
#   "theta"  Dispersion parameter (fenegbin only)

build_stat_rows <- function(records, omit.stat, digits, star.cutoffs, star.char,
                           obs.label = "Observations") {
  n_cols <- length(records)

  should_show <- function(id) {
    if (is.null(omit.stat)) return(TRUE)
    !any(omit.stat == id)
  }

  rows <- list()

  # Helper: safely get a numeric field from fit, returning NA if missing
  fit_val <- function(fit, field) {
    v <- fit[[field]]
    if (is.null(v) || length(v) == 0L) NA_real_ else v
  }

  # --- Observations ---
  if (should_show("n")) {
    vals <- vapply(records, function(r) format_nobs(r$nobs), character(1L))
    rows <- c(rows, list(list(label = obs.label, values = vals, se_values = NULL)))
  }

  # --- R²: overall (lm/feols) or squared-correlation (GLM) ---
  if (should_show("r2")) {
    vals <- vapply(records, function(r) {
      v <- fit_val(r$fit, "r2")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(label = "R$^{2}$", values = vals, se_values = NULL)))
    }
  }

  # --- Adjusted R² (OLS models only; no standard adjusted form for GLM corr-R²) ---
  if (should_show("adj.r2")) {
    vals <- vapply(records, function(r) {
      if (!identical(r$fit$type, "ols")) return("")
      v <- fit_val(r$fit, "adj_r2")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Adjusted R$^{2}$", values = vals, se_values = NULL
      )))
    }
  }

  # --- Within R² (feols with FEs, and GLM with FEs) ---
  if (should_show("r2")) {
    vals <- vapply(records, function(r) {
      v <- fit_val(r$fit, "wr2")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Within R$^{2}$", values = vals, se_values = NULL
      )))
    }
  }

  # --- Adjusted Within R² (OLS models with FEs only) ---
  if (should_show("adj.r2")) {
    vals <- vapply(records, function(r) {
      if (!identical(r$fit$type, "ols")) return("")
      v <- fit_val(r$fit, "adj_wr2")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Adjusted Within R$^{2}$", values = vals, se_values = NULL
      )))
    }
  }

  # --- Theta: negative binomial dispersion parameter ---
  if (should_show("theta")) {
    vals <- vapply(records, function(r) {
      v <- fit_val(r$fit, "theta")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Theta", values = vals, se_values = NULL
      )))
    }
  }

  # --- Residual Std. Error (lm only) ---
  if (should_show("sigma")) {
    vals <- vapply(records, function(r) {
      fit <- r$fit
      if (is.null(fit$sigma) || is.na(fit$sigma)) return("")
      paste0(formatC(fit$sigma, digits = digits, format = "f"),
             " (df = ", fit$df_residual, ")")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Residual Std. Error", values = vals, se_values = NULL
      )))
    }
  }

  # --- F Statistic (lm / Gaussian glm only) ---
  if (should_show("f")) {
    vals <- vapply(records, function(r) {
      fit <- r$fit
      if (is.null(fit$fstat) || is.na(fit$fstat)) return("")
      fstr <- add_stars(
        formatC(fit$fstat, digits = digits, format = "f"),
        fit$fstat_pval, star.cutoffs, star.char
      )
      paste0(fstr, " (df = ", fit$fstat_df1, "; ", fit$fstat_df2, ")")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "F Statistic", values = vals, se_values = NULL
      )))
    }
  }

  # --- Log Likelihood (glm only) ---
  if (should_show("ll")) {
    vals <- vapply(records, function(r) {
      v <- fit_val(r$fit, "ll")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Log Likelihood", values = vals, se_values = NULL
      )))
    }
  }

  # --- Akaike Inf. Crit. (glm only) ---
  if (should_show("aic")) {
    vals <- vapply(records, function(r) {
      v <- fit_val(r$fit, "aic")
      if (is.na(v)) "" else formatC(v, digits = digits, format = "f")
    }, character(1L))
    if (any(vals != "")) {
      rows <- c(rows, list(list(
        label = "Akaike Inf. Crit.", values = vals, se_values = NULL
      )))
    }
  }

  rows
}
