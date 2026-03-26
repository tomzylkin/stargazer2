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

# Format an observation count with a thousands separator.
format_nobs <- function(n) {
  format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Infer a human-readable SE-type label from a sandwich vcov matrix.
# Falls back to a generic label if class information is unavailable.
se_label_from_vcov <- function(vcov_mat) {
  if (inherits(vcov_mat, "vcovCL")) {
    cl <- attr(vcov_mat, "cluster")
    if (!is.null(cl)) {
      if (inherits(cl, "formula")) {
        vars <- all.vars(cl)
      } else {
        vars <- as.character(cl)
      }
      cl_str <- paste(vars, collapse = " x ")
      return(paste0("Standard errors clustered by ", cl_str))
    }
    return("Clustered standard errors")
  }
  if (inherits(vcov_mat, "vcovHC")) {
    type_str <- attr(vcov_mat, "method")
    if (is.null(type_str)) type_str <- attr(vcov_mat, "type")
    if (!is.null(type_str)) {
      return(paste0("Heteroskedasticity-robust standard errors (", type_str, ")"))
    }
    return("Heteroskedasticity-robust standard errors")
  }
  "Standard errors"
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

# Infer a human-readable SE-type label from a fixest model's call.
se_label_fixest_model <- function(model) {
  vcov_arg <- model$call$vcov
  if (is.null(vcov_arg)) {
    return("IID standard errors")
  }
  vcov_str <- trimws(deparse(vcov_arg))
  # Strip surrounding quotes if it was passed as a string literal
  vcov_str <- gsub('^["\']|["\']$', "", vcov_str)

  if (vcov_str %in% c("iid", "standard", "IID")) {
    return("IID standard errors")
  }
  if (vcov_str %in% c("hetero", "HC1", "hc1", "robust", "Robust")) {
    return("Heteroskedasticity-robust standard errors")
  }
  # Clustering formula: starts with ~
  if (grepl("^~", vcov_str)) {
    cluster_vars <- trimws(sub("^~", "", vcov_str))
    # region^industry -> "region x industry"
    cluster_vars <- gsub("\\^", " x ", cluster_vars)
    cluster_vars <- gsub("\\s+", " ", trimws(cluster_vars))
    return(paste0("Standard errors clustered by ", cluster_vars))
  }
  "Standard errors"
}
