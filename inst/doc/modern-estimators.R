## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "",
  message  = FALSE,
  warning  = FALSE
)
library(stargazer2)

## ----wage1-setup, eval = requireNamespace("wooldridge", quietly = TRUE)-------
library(wooldridge)
data(wage1)

wage1$region <- factor(
  ifelse(wage1$northcen == 1, "northcen",
  ifelse(wage1$south    == 1, "south",
  ifelse(wage1$west     == 1, "west", "northeast"))),
  levels = c("northeast", "northcen", "south", "west")
)

wage1$occupation <- factor(
  ifelse(wage1$profocc == 1, "professional",
  ifelse(wage1$clerocc == 1, "clerical",
  ifelse(wage1$servocc == 1, "service", "other"))),
  levels = c("other", "professional", "clerical", "service")
)

wage1$industry <- factor(
  ifelse(wage1$construc == 1, "construction",
  ifelse(wage1$ndurman  == 1, "nondurable_manuf",
  ifelse(wage1$trcommpu == 1, "transport",
  ifelse(wage1$trade    == 1, "trade",
  ifelse(wage1$services == 1, "services",
  ifelse(wage1$profserv == 1, "prof_services", "other")))))),
  levels = c("other", "construction", "nondurable_manuf",
             "transport", "trade", "services", "prof_services")
)

## ----fixest-models, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("fixest", quietly = TRUE)----
library(fixest)

f1 <- feols(lwage ~ educ + exper + tenure + female + married |
              region,               wage1, vcov = ~region^industry)
f2 <- feols(lwage ~ educ + exper + tenure + female + married |
              occupation,           wage1, vcov = ~region^industry)
f3 <- feols(lwage ~ educ + exper + tenure + female + married |
              region + occupation,  wage1, vcov = ~region^industry)
f4 <- feols(lwage ~ educ + exper + tenure + female + married |
              region + occupation + industry,
                                    wage1, vcov = ~region^industry)
f5 <- feols(lwage ~ educ + exper + tenure + female + married |
              region^industry,      wage1, vcov = ~region^industry)

## ----fixest-table-text, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("fixest", quietly = TRUE)----
stargazer(f1, f2, f3, f4, f5, type = "text")

## ----fixest-table-latex, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("fixest", quietly = TRUE)----
stargazer(f1, f2, f3, f4, f5,
          type             = "latex",
          title            = "Log Wages: Varying Fixed Effects",
          label            = "tab:fe-wages",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure",
                               "Female", "Married"))

## ----gravity-models, eval = requireNamespace("fixest", quietly = TRUE)--------
data(trade, package = "fixest")

gravity_ols    <- feols(log(Euros) ~ log(dist_km) |
                          Origin + Destination + Product + Year, trade)
gravity_pois   <- fepois(Euros ~ log(dist_km) |
                           Origin + Destination + Product + Year, trade)
gravity_negbin <- fenegbin(Euros ~ log(dist_km) |
                             Origin + Destination + Product + Year, trade)

## ----gravity-clustered, eval = requireNamespace("fixest", quietly = TRUE)-----
gravity_pois1  <- fepois(Euros ~ log(dist_km) |
                           Origin + Destination + Product + Year,
                         trade, vcov = ~Origin^Destination)
gravity_pois2  <- fepois(Euros ~ log(dist_km) |
                           Origin + Destination + Product + Year,
                         trade, vcov = ~Origin + Destination)

## ----gravity-table-default, eval = requireNamespace("fixest", quietly = TRUE)----
stargazer(gravity_ols, gravity_pois, gravity_negbin,
          gravity_pois1, gravity_pois2,
          type = "text")

## ----gravity-table, eval = requireNamespace("fixest", quietly = TRUE)---------
stargazer(gravity_ols, gravity_pois, gravity_negbin,
          gravity_pois1, gravity_pois2,
          type  = "latex",
          title = "Gravity Equation for Trade Flows",
          label = "tab:gravity")

## ----alpaca-table, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("alpaca", quietly = TRUE)----
library(alpaca)

# Logit model: P(married) as a function of wages and human capital,
# with occupation and industry fixed effects.
# industry must be in the FE specification for clustering by industry.
m_alp <- feglm(married ~ lwage + educ + exper | occupation + industry,
               wage1, binomial("logit"))

V_robust    <- alpaca_vcovSandwich(m_alp)
V_clustered <- alpaca_vcovCL(m_alp, cluster = ~industry)

stargazer(m_alp, m_alp,
          type             = "text",
          dep.var.labels   = "Married (0/1)",
          covariate.labels = c("log(Wage)", "Education", "Experience"),
          column.labels    = c("Sandwich-robust", "Industry-clustered"),
          vcov             = list(V_robust, V_clustered))

