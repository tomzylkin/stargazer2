# Model extraction layer for stargazer2.
#
# Each method returns a model_record: a named list with a standardised
# structure that the formatting layer consumes.  Adding support for a
# new package means writing a new extract_model() method here; the
# formatting and rendering layers are untouched.
#
# model_record fields
# -------------------
# coef_names    character  display names for coefficients (not LaTeX-escaped)
# coefs         numeric    coefficient estimates
# se            numeric    standard errors
# tstat         numeric    t / z statistics
# pval          numeric    two-sided p-values
# nobs          integer    number of observations
# fit           list       fit statistics (content varies by model type)
# fixed_effects character  FE variable names; character(0) if none
# se_label      character  SE type description for table note
# model_label   character  estimator name for column header
# dep_var       character  dependent variable name (not LaTeX-escaped)

# ---------------------------------------------------------------------------
# S3 generic
# ---------------------------------------------------------------------------

#' Extract a standardised model record from a regression object
#'
#' Internal S3 generic consumed by \code{\link{format_table}}.  Not intended
#' for direct use.
#'
#' @param model A fitted model object.
#' @param vcov_override A pre-computed variance-covariance matrix, or
#'   \code{NULL} to auto-extract.
#' @param se_override A named numeric vector of standard errors, or
#'   \code{NULL} to auto-extract.
#' @param ... Currently unused.
#' @keywords internal
extract_model <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  UseMethod("extract_model")
}

# ---------------------------------------------------------------------------
# lm method
# ---------------------------------------------------------------------------

extract_model.lm <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  s <- summary(model)
  coefs <- coef(model)
  # Match original stargazer: rename "(Intercept)" to "Constant"
  coef_names <- sub("^\\(Intercept\\)$", "Constant", names(coefs))

  # --- Standard errors (three-tier precedence) ---
  if (!is.null(vcov_override)) {
    se_vals  <- sqrt(diag(vcov_override))
    se_label <- se_label_from_vcov(vcov_override)
  } else if (!is.null(se_override)) {
    se_vals  <- se_override
    se_label <- "user-specified standard errors"
  } else {
    se_vals  <- sqrt(diag(vcov(model)))
    se_label <- "OLS standard errors"
  }

  # t-statistics and two-sided p-values
  tstat <- coefs / se_vals
  df_r  <- df.residual(model)
  pvals <- 2 * pt(-abs(tstat), df = df_r)

  # --- Fit statistics ---
  fstat     <- s$fstatistic   # c(value = , numdf = , dendf = )
  fstat_p   <- if (!is.null(fstat)) {
    pf(fstat["value"], fstat["numdf"], fstat["dendf"], lower.tail = FALSE)
  } else NA_real_

  fit <- list(
    type       = "ols",
    r2         = s$r.squared,
    adj_r2     = s$adj.r.squared,
    wr2        = NA_real_,
    adj_wr2    = NA_real_,
    sigma      = s$sigma,
    df_residual = as.integer(df_r),
    fstat      = if (!is.null(fstat)) unname(fstat["value"]) else NA_real_,
    fstat_df1  = if (!is.null(fstat)) as.integer(fstat["numdf"]) else NA_integer_,
    fstat_df2  = if (!is.null(fstat)) as.integer(fstat["dendf"]) else NA_integer_,
    fstat_pval = unname(fstat_p)
  )

  # --- Dependent variable name ---
  dep_var <- deparse(formula(model)[[2L]])

  list(
    coef_names    = coef_names,
    coefs         = unname(coefs),
    se            = unname(se_vals),
    tstat         = unname(tstat),
    pval          = unname(pvals),
    nobs          = as.integer(nobs(model)),
    fit           = fit,
    fixed_effects = character(0L),
    se_label      = se_label,
    model_label   = "OLS",
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# fixest method
# ---------------------------------------------------------------------------

extract_model.fixest <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    stop("Package 'fixest' is required but not installed.", call. = FALSE)
  }

  coefs      <- coef(model)
  coef_names <- names(coefs)

  # --- Standard errors (three-tier precedence) ---
  if (!is.null(vcov_override)) {
    se_vals  <- sqrt(diag(vcov_override))
    se_label <- se_label_from_vcov(vcov_override)
  } else if (!is.null(se_override)) {
    se_vals  <- se_override
    se_label <- "user-specified standard errors"
  } else {
    # Use the vcov baked into the model at estimation time;
    # read the vcov_type attribute fixest sets on the returned matrix.
    V        <- vcov(model)
    se_vals  <- sqrt(diag(V))
    method   <- if (!is.null(model$method)) model$method else "feols"
    se_label <- se_label_from_fixest_vcov(V, method = method)
  }

  # t / z statistics and p-values
  tstat <- unname(coefs) / unname(se_vals)
  pvals <- pvals_fixest(model, tstat)

  # --- Fixed effects ---
  fixed_effects <- get_fixef_vars(model)

  # --- Fit statistics ---
  fit <- fit_stats_fixest(model)

  # --- Dependent variable ---
  dep_var <- deparse(formula(model)[[2L]])

  # --- Model label ---
  model_label <- fixest_model_label(model)

  list(
    coef_names    = coef_names,
    coefs         = unname(coefs),
    se            = unname(se_vals),
    tstat         = tstat,
    pval          = pvals,
    nobs          = as.integer(nobs(model)),
    fit           = fit,
    fixed_effects = fixed_effects,
    se_label      = se_label,
    model_label   = model_label,
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# alpaca feglm method
# ---------------------------------------------------------------------------

extract_model.feglm <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  if (!requireNamespace("alpaca", quietly = TRUE)) {
    stop("Package 'alpaca' is required but not installed.", call. = FALSE)
  }

  coefs      <- coef(model)
  coef_names <- names(coefs)

  # --- Standard errors (three-tier precedence) ---
  if (!is.null(vcov_override)) {
    se_vals  <- sqrt(diag(vcov_override))
    se_label <- se_label_from_vcov(vcov_override)
  } else if (!is.null(se_override)) {
    se_vals  <- se_override
    se_label <- "user-specified standard errors"
  } else {
    V        <- vcov(model)
    se_vals  <- sqrt(diag(V))
    se_label <- "MLE standard errors"
  }

  # z-statistics and two-sided p-values (GLM uses asymptotic normal)
  tstat <- unname(coefs) / unname(se_vals)
  pvals <- 2 * pnorm(-abs(tstat))

  # --- Model label from family / link ---
  fam <- model$family$family
  lnk <- model$family$link
  model_label <- if (identical(fam, "binomial") && identical(lnk, "logit")) {
    "Logit"
  } else if (identical(fam, "binomial") && identical(lnk, "probit")) {
    "Probit"
  } else if (identical(fam, "poisson")) {
    "Poisson"
  } else {
    paste0(toupper(substr(fam, 1L, 1L)), substr(fam, 2L, nchar(fam)),
           " (", lnk, ")")
  }

  # --- Fixed effects: names of lvls.k ---
  fixed_effects <- names(model$lvls.k)
  if (is.null(fixed_effects)) fixed_effects <- character(0L)

  # --- Dependent variable ---
  dep_var <- deparse(formula(model)[[2L]])

  # --- Observations ---
  n_obs <- as.integer(model$nobs[["nobs"]])

  # --- Fit statistics: squared-correlation R² = cor(y, fitted)² ---
  fitted_vals <- tryCatch(fitted(model), error = function(e) NULL)
  y_vals      <- tryCatch(model$data[[dep_var]], error = function(e) NULL)
  corr_r2 <- if (!is.null(fitted_vals) && !is.null(y_vals) &&
                  length(fitted_vals) == length(y_vals) && length(fitted_vals) > 1L) {
    cor(as.numeric(fitted_vals), as.numeric(y_vals))^2
  } else NA_real_

  fit <- list(
    nobs = n_obs,
    r2   = corr_r2,
    type = "glm"
  )

  list(
    coef_names    = coef_names,
    coefs         = unname(coefs),
    se            = unname(se_vals),
    tstat         = tstat,
    pval          = pvals,
    nobs          = n_obs,
    fit           = fit,
    fixed_effects = fixed_effects,
    se_label      = se_label,
    model_label   = model_label,
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# alpaca summary.feglm method
# ---------------------------------------------------------------------------
#
# Users obtain alternative SEs from alpaca via:
#   s <- summary(mod, type = "clustered", cluster = ~X)
# and then pass s directly to stargazer().  The summary.feglm object
# contains the coefficient matrix (cm) with the desired SEs already
# baked in.  SE type is not stored in the object; stargazer() infers it
# by parsing the unevaluated model expression (see parse_summary_feglm_labels
# in stargazer.R) and writes the result into se_label before rendering.

extract_model.summary.feglm <- function(model, vcov_override = NULL,
                                        se_override = NULL, ...) {
  if (!requireNamespace("alpaca", quietly = TRUE)) {
    stop("Package 'alpaca' is required but not installed.", call. = FALSE)
  }

  cm         <- model$cm
  coef_names <- rownames(cm)
  coefs      <- cm[, "Estimate"]

  # --- Standard errors (three-tier precedence) ---
  if (!is.null(vcov_override)) {
    se_vals  <- sqrt(diag(vcov_override))
    se_label <- se_label_from_vcov(vcov_override)
    tstat    <- unname(coefs) / unname(se_vals)
    pvals    <- 2 * pnorm(-abs(tstat))
  } else if (!is.null(se_override)) {
    se_vals  <- se_override
    se_label <- "user-specified standard errors"
    tstat    <- unname(coefs) / unname(se_vals)
    pvals    <- 2 * pnorm(-abs(tstat))
  } else {
    # SEs, z-stats, and p-values are already computed by summary.feglm.
    # se_label is set to a placeholder here; stargazer() overwrites it by
    # parsing the unevaluated summary() call (parse_summary_feglm_labels).
    se_vals  <- cm[, "Std. error"]
    tstat    <- cm[, "z value"]
    pvals    <- cm[, "Pr(> |z|)"]
    se_label <- "MLE standard errors"
  }

  # --- Model label from family / link ---
  fam <- model$family$family
  lnk <- model$family$link
  model_label <- if (identical(fam, "binomial") && identical(lnk, "logit")) {
    "Logit"
  } else if (identical(fam, "binomial") && identical(lnk, "probit")) {
    "Probit"
  } else if (identical(fam, "poisson")) {
    "Poisson"
  } else {
    paste0(toupper(substr(fam, 1L, 1L)), substr(fam, 2L, nchar(fam)),
           " (", lnk, ")")
  }

  # --- Fixed effects: names of lvls.k ---
  fixed_effects <- names(model$lvls.k)
  if (is.null(fixed_effects)) fixed_effects <- character(0L)

  # --- Dependent variable ---
  dep_var <- deparse(model$formula[[2L]])

  # --- Observations ---
  n_obs <- as.integer(model$nobs[["nobs"]])

  # --- Fit statistics ---
  # summary.feglm does not retain fitted values, so squared-correlation R²
  # cannot be computed here.  Leave r2 as NA; the table column will be blank.
  fit <- list(
    nobs = n_obs,
    r2   = NA_real_,
    type = "glm"
  )

  list(
    coef_names    = coef_names,
    coefs         = unname(coefs),
    se            = unname(se_vals),
    tstat         = unname(tstat),
    pval          = unname(pvals),
    nobs          = n_obs,
    fit           = fit,
    fixed_effects = fixed_effects,
    se_label      = se_label,
    model_label   = model_label,
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# default method — informative error
# ---------------------------------------------------------------------------

extract_model.default <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  cls <- paste(class(model), collapse = ", ")
  stop(
    "stargazer2 does not know how to extract results from an object of class: ",
    cls, ".\n",
    "Supported classes: lm, fixest, feglm (alpaca), summary.feglm (alpaca).",
    call. = FALSE
  )
}

# ---------------------------------------------------------------------------
# fixest helpers (internal)
# ---------------------------------------------------------------------------

# Return the FE variable names for a fixest model.
get_fixef_vars <- function(model) {
  # fixef_vars is the most direct route
  fvars <- model$fixef_vars
  if (!is.null(fvars) && length(fvars) > 0L) {
    return(as.character(fvars))
  }

  # Fallback: parse the fixef formula
  fml_fe <- tryCatch(
    fixest::formula(model, type = "fixef"),
    error = function(e) NULL
  )
  if (!is.null(fml_fe)) {
    terms_fe <- attr(stats::terms(fml_fe), "term.labels")
    if (length(terms_fe) > 0L) return(terms_fe)
  }

  character(0L)
}

# Compute two-sided p-values for a fixest model.
# feols uses a t-distribution; GLM-family models use z (normal).
pvals_fixest <- function(model, tstat) {
  method <- model$method
  if (is.null(method)) method <- "feols"

  if (method == "feols") {
    df_r <- tryCatch(
      as.numeric(fixest::degrees_freedom(model, type = "t")),
      error = function(e) Inf
    )
    if (length(df_r) != 1L || is.na(df_r)) df_r <- Inf
    2 * pt(-abs(tstat), df = df_r)
  } else {
    # Poisson, NegBin, logit, probit, etc. — asymptotic z
    2 * pnorm(-abs(tstat))
  }
}

# Extract fit statistics from a fixest model.
fit_stats_fixest <- function(model) {
  method <- if (!is.null(model$method)) model$method else "feols"
  n      <- as.integer(nobs(model))

  if (method == "feols") {
    r2_vals <- tryCatch(fixest::r2(model), error = function(e) NULL)
    list(
      nobs    = n,
      r2      = if (!is.null(r2_vals)) unname(r2_vals["r2"])    else NA_real_,
      adj_r2  = if (!is.null(r2_vals)) unname(r2_vals["ar2"])   else NA_real_,
      wr2     = if (!is.null(r2_vals)) unname(r2_vals["wr2"])   else NA_real_,
      adj_wr2 = if (!is.null(r2_vals)) unname(r2_vals["war2"])  else NA_real_,
      type    = "ols"
    )
  } else {
    # Count / GLM models: squared-correlation R², within R², and (for negbin) theta.
    # McFadden pseudo-R² is omitted — it is scale-dependent for PPML when the
    # dependent variable is not a count (Green & Santos Silva, Stata Journal 2025).
    r2_vals <- tryCatch(fixest::r2(model), error = function(e) NULL)

    # Squared correlation R² = corr(y, fitted)^2: scale-invariant measure of fit
    fitted  <- tryCatch(model$fitted.values, error = function(e) NULL)
    y_vals  <- tryCatch(model$y,             error = function(e) NULL)
    corr_r2 <- if (!is.null(fitted) && !is.null(y_vals) &&
                    length(fitted) == length(y_vals) && length(fitted) > 1L) {
      cor(as.numeric(fitted), as.numeric(y_vals))^2
    } else NA_real_

    # Theta: negative binomial overdispersion parameter
    theta <- if (method == "fenegbin") {
      tryCatch(as.numeric(model$theta), error = function(e) NA_real_)
    } else NA_real_

    list(
      nobs    = n,
      r2      = corr_r2,
      wr2     = if (!is.null(r2_vals)) unname(r2_vals["wpr2"]) else NA_real_,
      adj_wr2 = if (!is.null(r2_vals)) unname(r2_vals["wapr2"]) else NA_real_,
      theta   = theta,
      type    = "glm"
    )
  }
}

# Map fixest method string to a display label for column headers.
fixest_model_label <- function(model) {
  method <- if (!is.null(model$method)) model$method else "feols"
  switch(method,
    feols    = "OLS",
    fepois   = "Poisson",
    fenegbin = "Neg. Binomial",
    feglm    = {
      fam <- tryCatch(model$family$family, error = function(e) "GLM")
      if (is.null(fam)) "GLM" else fam
    },
    toupper(method)  # fallback
  )
}
