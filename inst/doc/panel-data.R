## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "",
  message  = FALSE,
  warning  = FALSE
)
library(stargazer2)

## ----models, eval = requireNamespace("plm", quietly = TRUE)-------------------
library(plm)
data("Grunfeld", package = "plm")

m_pool <- plm(inv ~ value + capital, Grunfeld,
              index = c("firm", "year"), model = "pooling")

m_fe   <- plm(inv ~ value + capital, Grunfeld,
              index = c("firm", "year"), model = "within")

m_twfe <- plm(inv ~ value + capital, Grunfeld,
              index = c("firm", "year"), model = "within", effect = "twoways")

m_re   <- plm(inv ~ value + capital, Grunfeld,
              index = c("firm", "year"), model = "random")

## ----default-text, eval = requireNamespace("plm", quietly = TRUE)-------------
stargazer(m_pool, m_fe, m_twfe, m_re, type = "text")

## ----robust-text, eval = requireNamespace("plm", quietly = TRUE)--------------
V_fe   <- vcovHC(m_fe,   method = "arellano")
V_twfe <- vcovHC(m_twfe, method = "arellano")

stargazer(m_fe, m_twfe,
          type     = "text",
          vcov     = list(V_fe, V_twfe),
          se_label = "Arellano cluster-robust standard errors")

## ----formatted-latex, eval = requireNamespace("plm", quietly = TRUE)----------
stargazer(m_pool, m_fe, m_twfe, m_re,
          type             = "latex",
          title            = "Investment Equations: Grunfeld Panel Data",
          label            = "tab:grunfeld",
          dep.var.labels   = "Investment",
          covariate.labels = c("Market Value", "Capital Stock"),
          column.labels    = c("Pooled OLS", "FE", "Two-way FE", "RE"))

