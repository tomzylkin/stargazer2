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
    reports_fe    = TRUE,
    se_label      = se_label,
    model_label   = "OLS",
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# glm method
# ---------------------------------------------------------------------------

# Internal helper shared by extract_model.glm.
glm_model_label <- function(fam, lnk) {
  if (identical(fam, "gaussian")  && identical(lnk, "identity")) return("OLS")
  if (identical(fam, "binomial")  && identical(lnk, "logit"))    return("Logit")
  if (identical(fam, "binomial")  && identical(lnk, "probit"))   return("Probit")
  if (identical(fam, "poisson")   && identical(lnk, "log"))      return("Poisson")
  fam_cap <- paste0(toupper(substr(fam, 1L, 1L)), substr(fam, 2L, nchar(fam)))
  paste0(fam_cap, " (", lnk, ")")
}

extract_model.glm <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  fam <- family(model)$family
  lnk <- family(model)$link
  is_gaussian_ols <- identical(fam, "gaussian") && identical(lnk, "identity")

  coefs      <- coef(model)
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
    se_label <- if (is_gaussian_ols) "OLS standard errors" else "MLE standard errors"
  }

  # --- t (Gaussian OLS) vs z (all other GLMs) statistics ---
  df_r  <- df.residual(model)
  tstat <- unname(coefs) / unname(se_vals)
  pvals <- if (is_gaussian_ols) {
    2 * pt(-abs(tstat), df = df_r)
  } else {
    2 * pnorm(-abs(tstat))
  }

  # --- Log-likelihood and AIC ---
  ll_val  <- tryCatch(as.numeric(logLik(model)), error = function(e) NA_real_)
  aic_val <- tryCatch(AIC(model), error = function(e) NA_real_)

  # --- Fit statistics ---
  if (is_gaussian_ols) {
    y   <- model$y
    if (is.null(y)) y <- fitted(model) + residuals(model)
    ss_res <- sum(residuals(model)^2)
    ss_tot <- sum((y - mean(y))^2)
    r2     <- 1 - ss_res / ss_tot
    adj_r2 <- 1 - (1 - r2) * (length(y) - 1L) / as.integer(df_r)
    sigma  <- sqrt(ss_res / as.integer(df_r))
    k      <- sum(names(coefs) != "(Intercept)")
    f_val  <- if (k > 0L) (r2 / k) / ((1 - r2) / as.integer(df_r)) else NA_real_
    f_pval <- if (!is.na(f_val)) pf(f_val, k, as.integer(df_r), lower.tail = FALSE) else NA_real_
    fit <- list(
      type        = "ols",
      r2          = r2,
      adj_r2      = adj_r2,
      wr2         = NA_real_,
      adj_wr2     = NA_real_,
      sigma       = sigma,
      df_residual = as.integer(df_r),
      fstat       = if (k > 0L) unname(f_val) else NA_real_,
      fstat_df1   = if (k > 0L) as.integer(k)  else NA_integer_,
      fstat_df2   = as.integer(df_r),
      fstat_pval  = unname(f_pval),
      ll          = ll_val,
      aic         = aic_val
    )
  } else {
    fit <- list(
      type    = "glm",
      r2      = NA_real_,
      adj_r2  = NA_real_,
      wr2     = NA_real_,
      adj_wr2 = NA_real_,
      sigma   = NA_real_,
      ll      = ll_val,
      aic     = aic_val
    )
  }

  dep_var <- deparse(formula(model)[[2L]])

  list(
    coef_names    = coef_names,
    coefs         = unname(coefs),
    se            = unname(se_vals),
    tstat         = tstat,
    pval          = pvals,
    nobs          = as.integer(nobs(model)),
    fit           = fit,
    fixed_effects = character(0L),
    reports_fe    = TRUE,
    se_label      = se_label,
    model_label   = glm_model_label(fam, lnk),
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
    # Use the vcov baked into the model at estimation time.  The SE *values*
    # come straight from vcov() (correct on every fixest version); the SE-type
    # *label* is read via fixest_vcov_type(), which knows where each fixest
    # version records it (matrix attribute on >= 0.14, summary attribute on
    # older versions).  Relying on vcov()'s attribute alone mislabeled
    # clustered SEs as "OLS" on fixest < 0.14.
    V        <- vcov(model)
    se_vals  <- sqrt(diag(V))
    method   <- if (!is.null(model$method)) model$method else "feols"
    vcov_call <- tryCatch(as.character(model$call$vcov), error = function(e) NULL)
    se_label <- se_label_from_fixest_type(
      fixest_vcov_type(model), method = method, vcov_call = vcov_call)
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
    reports_fe    = TRUE,
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
    reports_fe    = TRUE,
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
    # The SE type is not stored in the object, so we cannot detect it
    # automatically unless the summary() call was written inline in
    # stargazer() (which allows expression parsing).  Use a placeholder
    # that surfaces in the table note; the user can override with se_label=.
    se_vals  <- cm[, "Std. error"]
    tstat    <- cm[, "z value"]
    pvals    <- cm[, "Pr(> |z|)"]
    se_label <- "SE type not detected (use se_label= to specify)"
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
    reports_fe    = TRUE,
    se_label      = se_label,
    model_label   = model_label,
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# plm method
# ---------------------------------------------------------------------------

plm_model_label <- function(model_type) {
  switch(model_type,
    within  = "FE",
    random  = "RE",
    pooling = "Pooled OLS",
    fd      = "FD",
    between = "Between",
    toupper(model_type)
  )
}

extract_model.plm <- function(model, vcov_override = NULL, se_override = NULL, ...) {
  if (!requireNamespace("plm", quietly = TRUE)) {
    stop("Package 'plm' is required but not installed.", call. = FALSE)
  }

  s          <- summary(model)
  coefs      <- coef(model)
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

  # t-statistics and p-values
  df_r  <- df.residual(model)
  tstat <- unname(coefs) / unname(se_vals)
  pvals <- 2 * pt(-abs(tstat), df = df_r)

  # --- Model type and effect ---
  model_type <- model$args$model
  effect     <- if (!is.null(model$args$effect)) model$args$effect else "individual"

  # --- Panel index variable names ---
  idx      <- attr(model$model, "index")
  idx_vars <- names(idx)
  ind_var  <- idx_vars[1L]
  time_var <- idx_vars[2L]

  # --- Fixed effects (within models only) ---
  fixed_effects <- if (identical(model_type, "within")) {
    if (identical(effect, "twoways")) {
      c(ind_var, time_var)
    } else if (identical(effect, "time")) {
      time_var
    } else {
      ind_var
    }
  } else {
    character(0L)
  }

  # --- Random effects (RE models only) ---
  random_effects <- if (identical(model_type, "random")) {
    if (identical(effect, "twoways")) {
      c(ind_var, time_var)
    } else if (identical(effect, "time")) {
      time_var
    } else {
      ind_var
    }
  } else {
    character(0L)
  }

  # --- Fit statistics ---
  r2_vec <- s$r.squared
  r2     <- if (!is.null(r2_vec) && length(r2_vec) >= 1L) r2_vec[[1L]] else NA_real_
  adj_r2 <- if (!is.null(r2_vec) && length(r2_vec) >= 2L) r2_vec[[2L]] else NA_real_

  fstat_h    <- s$fstatistic
  fstat_val  <- if (!is.null(fstat_h)) unname(fstat_h$statistic) else NA_real_
  fstat_df1  <- if (!is.null(fstat_h) && "df1" %in% names(fstat_h$parameter)) {
    as.integer(fstat_h$parameter["df1"])
  } else NA_integer_
  fstat_df2  <- if (!is.null(fstat_h) && "df2" %in% names(fstat_h$parameter)) {
    as.integer(fstat_h$parameter["df2"])
  } else NA_integer_
  fstat_pval <- if (!is.null(fstat_h)) fstat_h$p.value else NA_real_

  sigma <- tryCatch(
    sqrt(sum(residuals(model)^2) / df_r),
    error = function(e) NA_real_
  )

  fit <- list(
    type        = "ols",
    r2          = r2,
    adj_r2      = adj_r2,
    wr2         = NA_real_,
    adj_wr2     = NA_real_,
    sigma       = sigma,
    df_residual = as.integer(df_r),
    fstat       = fstat_val,
    fstat_df1   = fstat_df1,
    fstat_df2   = fstat_df2,
    fstat_pval  = fstat_pval
  )

  dep_var <- deparse(formula(model)[[2L]])

  list(
    coef_names     = coef_names,
    coefs          = unname(coefs),
    se             = unname(se_vals),
    tstat          = unname(tstat),
    pval           = unname(pvals),
    nobs           = as.integer(nobs(model)),
    fit            = fit,
    fixed_effects  = fixed_effects,
    random_effects = random_effects,
    reports_fe     = TRUE,
    se_label       = se_label,
    model_label    = plm_model_label(model_type),
    dep_var        = dep_var
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
    "Supported classes: lm, glm, fixest, feglm (alpaca), summary.feglm (alpaca), plm.",
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

  # Fallback: parse the fixef formula.  Use the stats::formula generic, which
  # dispatches to fixest's formula.fixest method (fixest does not export a
  # 'formula' object of its own).
  fml_fe <- tryCatch(
    stats::formula(model, type = "fixef"),
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
