# Convenience vcov helpers for alpaca feglm models.
#
# alpaca does not expose heteroskedasticity-robust or cluster-robust vcov
# matrices directly; users obtain alternative SEs via summary(mod, type=...).
# These helpers wrap that interface and return a diagonal vcov matrix with a
# custom class so that se_label_from_vcov() can automatically detect the SE
# type — identical in spirit to sandwich::vcovHC / vcovCL.
#
# Usage:
#   stargazer(mod, vcov = list(alpaca_vcovSandwich(mod)))
#   stargazer(mod, vcov = list(alpaca_vcovCL(mod, ~firm)))
#   stargazer(mod, vcov = list(alpaca_vcovCL(mod, ~firm^year)))  # two-way

#' Heteroskedasticity-robust variance-covariance matrix for alpaca feglm
#'
#' Wraps \code{summary(mod, type = "sandwich")} and returns a diagonal
#' variance-covariance matrix that \code{\link{stargazer}} recognises and
#' labels as "heteroskedasticity-robust standard errors".
#'
#' @param mod A fitted \code{feglm} object from the \pkg{alpaca} package.
#' @return A square diagonal matrix of class \code{"vcovAlpacaSandwich"} with
#'   squared sandwich standard errors on the diagonal.
#' @examples
#' \donttest{
#' if (requireNamespace("alpaca", quietly = TRUE)) {
#'   d <- data.frame(
#'     y   = rbinom(200, 1, 0.5),
#'     x1  = rnorm(200),
#'     grp = factor(rep(1:10, 20))
#'   )
#'   mod <- alpaca::feglm(y ~ x1 | grp, d, binomial("logit"))
#'   V   <- alpaca_vcovSandwich(mod)
#'   stargazer(mod, vcov = list(V), type = "text")
#' }
#' }
#' @export
alpaca_vcovSandwich <- function(mod) {
  if (!requireNamespace("alpaca", quietly = TRUE)) {
    stop("Package 'alpaca' is required but not installed.", call. = FALSE)
  }
  s  <- summary(mod, type = "sandwich")
  se <- s$cm[, "Std. error"]
  V  <- diag(se^2, nrow = length(se))
  dimnames(V) <- list(names(se), names(se))
  class(V) <- c("vcovAlpacaSandwich", "matrix", "array")
  V
}

#' Cluster-robust variance-covariance matrix for alpaca feglm
#'
#' Wraps \code{summary(mod, type = "clustered", cluster = cluster)} and
#' returns a diagonal variance-covariance matrix that \code{\link{stargazer}}
#' recognises and labels with the appropriate "clustered by ..." description.
#'
#' @param mod     A fitted \code{feglm} object from the \pkg{alpaca} package.
#' @param cluster A one-sided formula identifying the clustering variable(s).
#'   Use \code{~X} for one-way, \code{~X + Y} for two-way additive, or
#'   \code{~X^Y} for two-way interaction clustering.
#' @return A square diagonal matrix of class \code{"vcovAlpacaCL"} with a
#'   \code{"cluster"} attribute containing the cluster formula.
#' @examples
#' \donttest{
#' if (requireNamespace("alpaca", quietly = TRUE)) {
#'   d <- data.frame(
#'     y   = rbinom(200, 1, 0.5),
#'     x1  = rnorm(200),
#'     grp = factor(rep(1:10, 20))
#'   )
#'   mod <- alpaca::feglm(y ~ x1 | grp, d, binomial("logit"))
#'   V   <- alpaca_vcovCL(mod, cluster = ~grp)
#'   stargazer(mod, vcov = list(V), type = "text")
#' }
#' }
#' @export
alpaca_vcovCL <- function(mod, cluster) {
  if (!requireNamespace("alpaca", quietly = TRUE)) {
    stop("Package 'alpaca' is required but not installed.", call. = FALSE)
  }
  s  <- summary(mod, type = "clustered", cluster = cluster)
  se <- s$cm[, "Std. error"]
  V  <- diag(se^2, nrow = length(se))
  dimnames(V) <- list(names(se), names(se))
  attr(V, "cluster") <- cluster
  class(V) <- c("vcovAlpacaCL", "matrix", "array")
  V
}
