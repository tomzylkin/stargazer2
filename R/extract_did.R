# DiD extraction methods for stargazer2.
#
# Supported classes:
#   AGGTEobj        -- did::aggte() output (Callaway-Sant'Anna)
#   emfx            -- etwfe::emfx() output (Extended TWFE marginal effects)
#   staggered_result -- wrapper around staggered::staggered() output
#
# All DiD extractors set use_ci = TRUE and populate ci_lower / ci_upper.
# The formatting layer renders these as [lower, upper] brackets instead of
# parenthetical SEs.  Stars are still derived from p-values (computed from
# the SE), so three levels of significance are preserved.

# ---------------------------------------------------------------------------
# as_staggered_result(): user-facing wrapper
# ---------------------------------------------------------------------------

#' Wrap a \code{staggered::staggered()} result for use with \code{stargazer}
#'
#' \code{staggered::staggered()} returns a plain \code{data.frame}, which
#' \code{stargazer} would otherwise route to the summary-statistics path.
#' This wrapper adds a \code{"staggered_result"} class so that the correct
#' extraction method is dispatched.
#'
#' @param x      A \code{data.frame} returned by \code{staggered::staggered()}.
#'   Must contain columns \code{estimate} and \code{se}.
#' @param dep_var Character; name of the outcome variable shown in the table
#'   header.  Defaults to \code{""}.
#' @param nobs   Integer; number of observations to display in the fit-stat
#'   section.  Pass the number of panel units ×time periods (or total rows)
#'   as appropriate.  Defaults to \code{NA} (blank in the table).
#'
#' @return \code{x} with class \code{c("staggered_result", "data.frame")} and
#'   attributes \code{dep_var} and \code{nobs_val}.
#'
#' @export
as_staggered_result <- function(x, dep_var = "", nobs = NA_integer_) {
  if (!is.data.frame(x)) {
    stop(
      "as_staggered_result: 'x' must be a data.frame ",
      "(the output of staggered::staggered())",
      call. = FALSE
    )
  }
  if (!all(c("estimate", "se") %in% names(x))) {
    stop(
      "as_staggered_result: 'x' must contain columns 'estimate' and 'se'",
      call. = FALSE
    )
  }
  attr(x, "dep_var")  <- as.character(dep_var)
  attr(x, "nobs_val") <- as.integer(nobs)
  class(x) <- c("staggered_result", "data.frame")
  x
}

# ---------------------------------------------------------------------------
# AGGTEobj (did::aggte)
# ---------------------------------------------------------------------------

extract_model.AGGTEobj <- function(model, vcov_override = NULL,
                                   se_override = NULL, ...) {
  if (!requireNamespace("did", quietly = TRUE)) {
    stop("Package 'did' is required but not installed.", call. = FALSE)
  }

  show_dyn <- isTRUE(list(...)$show.dynamics)
  dep_var  <- model$DIDparams$yname

  if (show_dyn && identical(model$type, "dynamic")) {
    # Event-study rows: use egt values directly as labels (relative event times)
    coef_names <- as.character(model$egt)
    coefs      <- model$att.egt
    se_vals    <- model$se.egt
  } else {
    # Single overall ATT (default; also used for type = "simple")
    coef_names <- "ATT"
    coefs      <- model$overall.att
    se_vals    <- model$overall.se
  }

  tstat    <- coefs / se_vals
  pvals    <- 2 * pnorm(-abs(tstat))
  ci_lower <- coefs - 1.96 * se_vals
  ci_upper <- coefs + 1.96 * se_vals

  nobs_val <- tryCatch(
    as.integer(nrow(model$DIDparams$data)),
    error = function(e) NA_integer_
  )

  list(
    coef_names    = coef_names,
    coefs         = coefs,
    se            = se_vals,
    tstat         = tstat,
    pval          = pvals,
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    use_ci        = TRUE,
    nobs          = nobs_val,
    fit           = list(type = "did", nobs = nobs_val),
    fixed_effects = character(0L),
    se_label      = "95% confidence intervals",
    model_label   = "Callaway-Sant'Anna",
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# emfx (etwfe::emfx)
# ---------------------------------------------------------------------------

extract_model.emfx <- function(model, vcov_override = NULL,
                                se_override = NULL, ...) {
  if (!requireNamespace("etwfe", quietly = TRUE)) {
    stop("Package 'etwfe' is required but not installed.", call. = FALSE)
  }

  etwfe_attr <- attr(model, "etwfe")
  dep_var    <- if (!is.null(etwfe_attr$yvar)) etwfe_attr$yvar else ""

  # nobs from the underlying fixest model stored in the marginaleffects slot
  me_internal <- attr(model, "marginaleffects")
  nobs_val <- tryCatch(
    as.integer(nobs(me_internal@model)),
    error = function(e) NA_integer_
  )

  has_event_col <- "event" %in% names(model)

  if (has_event_col && nrow(model) > 1L) {
    # event-study rows: emfx(mod, type = "event"); use event values as labels
    coef_names <- as.character(model$event)
    coefs      <- model$estimate
    se_vals    <- model$std.error
    tstat      <- model$statistic
    pvals      <- model$p.value
    ci_lower   <- model$conf.low
    ci_upper   <- model$conf.high
  } else {
    # Single aggregated ATT: emfx(mod) default
    coef_names <- "ATT"
    coefs      <- model$estimate[[1L]]
    se_vals    <- model$std.error[[1L]]
    tstat      <- model$statistic[[1L]]
    pvals      <- model$p.value[[1L]]
    ci_lower   <- model$conf.low[[1L]]
    ci_upper   <- model$conf.high[[1L]]
  }

  # Fixed effects: pull from the underlying fixest model (cohort + time FEs)
  fixed_effects <- tryCatch(
    get_fixef_vars(me_internal@model),
    error = function(e) character(0L)
  )

  list(
    coef_names    = coef_names,
    coefs         = coefs,
    se            = se_vals,
    tstat         = tstat,
    pval          = pvals,
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    use_ci        = TRUE,
    nobs          = nobs_val,
    fit           = list(type = "did", nobs = nobs_val),
    fixed_effects = fixed_effects,
    se_label      = "95% confidence intervals",
    model_label   = "Extended TWFE",
    dep_var       = dep_var
  )
}

# ---------------------------------------------------------------------------
# staggered_result (staggered::staggered wrapped via as_staggered_result)
# ---------------------------------------------------------------------------

extract_model.staggered_result <- function(model, vcov_override = NULL,
                                            se_override = NULL, ...) {
  if (!requireNamespace("staggered", quietly = TRUE)) {
    stop("Package 'staggered' is required but not installed.", call. = FALSE)
  }

  coefs    <- model$estimate
  se_vals  <- model$se
  tstat    <- coefs / se_vals
  pvals    <- 2 * pnorm(-abs(tstat))
  ci_lower <- coefs - 1.96 * se_vals
  ci_upper <- coefs + 1.96 * se_vals

  dep_var  <- attr(model, "dep_var")
  if (is.null(dep_var)) dep_var <- ""

  nobs_val <- attr(model, "nobs_val")
  if (is.null(nobs_val) || (length(nobs_val) == 1L && is.na(nobs_val))) {
    nobs_val <- NA_integer_
  }

  list(
    coef_names    = "ATT",
    coefs         = coefs,
    se            = se_vals,
    tstat         = tstat,
    pval          = pvals,
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    use_ci        = TRUE,
    nobs          = as.integer(nobs_val),
    fit           = list(type = "did", nobs = as.integer(nobs_val)),
    fixed_effects = character(0L),
    se_label      = "95% confidence intervals",
    model_label   = "Roth-Sant'Anna",
    dep_var       = dep_var
  )
}
