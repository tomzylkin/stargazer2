# Internal utility functions for stargazer2.
# None of these are user-facing; do not export.

# Escape characters that are special in LaTeX.
# Applied to strings that come from R (variable names, dep var labels,
# user-supplied text) before being embedded in LaTeX output.
latex_escape <- function(x) {
  # Backslash must be first to avoid double-escaping subsequent replacements.
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("_",  "\\_",               x, fixed = TRUE)
  x <- gsub("%",  "\\%",               x, fixed = TRUE)
  x <- gsub("&",  "\\&",               x, fixed = TRUE)
  x <- gsub("#",  "\\#",               x, fixed = TRUE)
  x <- gsub("$",  "\\$",               x, fixed = TRUE)
  x
}

# Format a single number to a fixed number of decimal places.
# Negative sign is rendered as LaTeX math-mode $-$ to match stargazer.
format_num <- function(x, digits) {
  if (is.na(x)) return("")
  fmt <- formatC(abs(x), digits = digits, format = "f")
  if (x < 0) paste0("$-$", fmt) else fmt
}

# Append LaTeX significance-star markup to a formatted number string.
# star_cutoffs: ascending p-value thresholds, e.g. c(0.1, 0.05, 0.01).
# star_char:    corresponding star strings, e.g. c("*", "**", "***").
add_stars <- function(val_str, pval,
                      star_cutoffs = c(0.1, 0.05, 0.01),
                      star_char    = c("*", "**", "***")) {
  if (is.na(pval) || val_str == "") return(val_str)
  stars <- ""
  for (i in rev(seq_along(star_cutoffs))) {
    if (pval < star_cutoffs[i]) {
      stars <- star_char[i]
      break
    }
  }
  if (nchar(stars) > 0) paste0(val_str, "$^{", stars, "}$") else val_str
}

# Format a standard error in parentheses: "(0.037)".
format_se <- function(x, digits) {
  if (is.na(x)) return("")
  paste0("(", formatC(x, digits = digits, format = "f"), ")")
}

# Format a 95% CI in brackets: "[-0.075, -0.026]".
# Uses plain sprintf() so both bounds always have the same number of decimal
# places regardless of sign or magnitude, and the result is exactly
# nchar(sprintf("[%%.Xf, %%.Xf]", lo, hi)) characters -- no LaTeX markup.
# This matches format_se() convention and keeps ci_col_widths exact.
format_ci <- function(lo, hi, digits) {
  if (is.na(lo) || is.na(hi)) return("")
  sprintf("[%.*f, %.*f]", digits, lo, digits, hi)
}

# Format an observation count with a thousands separator.
format_nobs <- function(n) {
  format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Infer a human-readable SE-type label from a sandwich vcov matrix.
# Falls back to a generic label if class information is unavailable.
se_label_from_vcov <- function(vcov_mat) {
  # alpaca vcov helpers set custom classes so detection is reliable.
  if (inherits(vcov_mat, "vcovAlpacaSandwich")) {
    return("heteroskedasticity-robust standard errors")
  }
  if (inherits(vcov_mat, "vcovAlpacaCL")) {
    cl <- attr(vcov_mat, "cluster")
    if (!is.null(cl)) {
      vars    <- all.vars(cl)
      cl_str  <- if (grepl("\\^", deparse(cl))) {
        paste(vars, collapse = " x ")
      } else if (length(vars) == 1L) {
        vars
      } else {
        paste0(paste(vars[-length(vars)], collapse = ", "), " and ", vars[length(vars)])
      }
      return(paste0("standard errors clustered by ", cl_str))
    }
    return("clustered standard errors")
  }
  # sandwich::vcovHC / vcovCL (class not set by sandwich itself; handled by
  # expression parsing in stargazer(), but kept here for documentation).
  if (inherits(vcov_mat, "vcovCL")) {
    cl <- attr(vcov_mat, "cluster")
    if (!is.null(cl)) {
      vars <- if (inherits(cl, "formula")) all.vars(cl) else as.character(cl)
      cl_str <- if (length(vars) == 1L) {
        vars
      } else {
        paste0(paste(vars[-length(vars)], collapse = ", "), " and ", vars[length(vars)])
      }
      return(paste0("standard errors clustered by ", cl_str))
    }
    return("clustered standard errors")
  }
  if (inherits(vcov_mat, "vcovHC")) {
    type_str <- attr(vcov_mat, "method")
    if (is.null(type_str)) type_str <- attr(vcov_mat, "type")
    if (!is.null(type_str)) {
      return(paste0(type_str, " heteroskedasticity-robust standard errors"))
    }
    return("heteroskedasticity-robust standard errors")
  }
  "user-specified standard errors"
}

# Return the appropriate SE label for a fixest IID (default) vcov,
# which varies by model type: feols -> OLS, fepois -> heteroskedasticity-robust,
# fenegbin/feglm -> MLE.
fixest_iid_se_label <- function(method) {
  switch(method,
    feols    = "OLS standard errors",
    fepois   = "Heteroskedasticity-robust standard errors",
    fenegbin = "MLE standard errors",
    feglm    = "MLE standard errors",
    "OLS standard errors"
  )
}

# Infer a human-readable SE-type label from a fixest_vcov matrix (output of
# vcov() on a fixest model).  Uses the 'vcov_type' attribute set by fixest.
# method: the fixest estimation method string (model$method), used to
# distinguish OLS / Poisson / NegBin defaults when vcov_type == "IID".
se_label_from_fixest_vcov <- function(V, method = "feols", vcov_call = NULL) {
  vt <- attr(V, "vcov_type")
  if (is.null(vt)) return(fixest_iid_se_label(method))

  if (vt == "IID") return(fixest_iid_se_label(method))

  if (vt == "Heteroskedasticity-robust") {
    # If the user passed a recognised HC variant string to feols(), preserve it
    # in the note.  fixest treats "HC1", "HC2", "HC3" as aliases for its
    # heteroskedasticity-robust estimator but does not record the alias in the
    # vcov matrix attributes, so we recover it from the model call.
    hc_str <- if (is.character(vcov_call) && length(vcov_call) == 1L &&
                  grepl("^HC[0-9]+$", vcov_call, ignore.case = FALSE)) {
      paste0(vcov_call, " ")
    } else {
      ""
    }
    return(paste0(hc_str, "heteroskedasticity-robust standard errors"))
  }

  if (startsWith(vt, "Clustered (")) {
    vars_str <- sub("^Clustered \\((.+)\\)$", "\\1", vt)
    # Clean up internal fixest interaction notation, e.g.
    # combine_fixef_keep_names(Origin, Destination) -> Origin x Destination
    vars_str <- gsub(
      "combine_fixef_keep_names\\(([^,]+),\\s*([^)]+)\\)",
      "\\1 x \\2", vars_str
    )
    # Two-way: "X & Y" -> "X and Y"
    vars_str <- gsub(" & ", " and ", vars_str, fixed = TRUE)
    return(paste0("standard errors clustered by ", vars_str))
  }

  # Other fixest vcov types (Driscoll-Kraay, Bootstrap, …)
  paste0(tolower(vt), " standard errors")
}

# Format a fixest FE variable name for display in the table.
# "region"          -> "Region FE"
# "region^industry" -> "Region x Industry FE"
format_fe_label <- function(fe_var) {
  parts <- strsplit(fe_var, "^", fixed = TRUE)[[1]]
  formatted <- vapply(parts, function(p) {
    p <- trimws(p)
    if (nchar(p) == 0L) return(p)
    paste0(toupper(substr(p, 1L, 1L)), substr(p, 2L, nchar(p)))
  }, character(1L), USE.NAMES = FALSE)
  paste0(paste(formatted, collapse = " x "), " FE")
}

# Build the SE-type portion of the table note.
#
# se_labels:   character vector of length n_cols (one per column)
# col_numbers: character vector like c("(1)", "(2)", ...)
#
# Returns a single string when all columns share the same SE type.
# Returns a grouped per-column string when SE types differ across columns,
# e.g. "(1) HC1 heteroskedasticity-robust standard errors; (2)-(3) OLS standard errors".
format_se_note <- function(se_labels, col_numbers) {
  # All columns share the same SE type
  if (length(unique(se_labels)) == 1L) {
    return(se_labels[1L])
  }

  # Multiple SE types: group consecutive columns with the same SE type
  groups <- list()
  i <- 1L
  while (i <= length(se_labels)) {
    j <- i
    while (j < length(se_labels) && se_labels[j + 1L] == se_labels[i]) {
      j <- j + 1L
    }
    groups <- c(groups, list(list(cols = col_numbers[i:j], label = se_labels[i])))
    i <- j + 1L
  }

  parts <- vapply(groups, function(g) {
    col_str <- if (length(g$cols) == 1L) {
      g$cols[1L]
    } else {
      paste0(g$cols[1L], "-", g$cols[length(g$cols)])
    }
    paste0(col_str, " ", g$label)
  }, character(1L))

  paste(parts, collapse = "; ")
}
