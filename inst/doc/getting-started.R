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

## ----lm-models, eval = requireNamespace("wooldridge", quietly = TRUE)---------
m1 <- lm(lwage ~ educ + exper + tenure, wage1)
m2 <- lm(lwage ~ educ + exper + tenure + female + married, wage1)
m3 <- lm(lwage ~ educ + exper + tenure + female + married +
            region + occupation, wage1)
m4 <- lm(lwage ~ educ + exper + tenure + female + married +
            region + occupation + industry, wage1)

## ----basic-table, eval = requireNamespace("wooldridge", quietly = TRUE)-------
stargazer(m1, m2, m3, m4,
          type             = "text",
          title            = "Determinants of Log Wages",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure",
                               "Female", "Married"),
          omit             = c("region", "occupation", "industry"),
          column.labels    = c("Baseline", "Demographics",
                               "Region/Occ.", "Full"),
          notes.append     = FALSE,
          notes            = "Controls for region, occupation, and industry in (3) and (4).")

## ----latex-output, eval = requireNamespace("wooldridge", quietly = TRUE)------
stargazer(m1, m2,
          type             = "latex",
          title            = "Determinants of Log Wages",
          label            = "tab:wage-ols",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure"))

## ----html-output, eval = requireNamespace("wooldridge", quietly = TRUE), results = "asis"----
stargazer(m1, m2,
          type             = "html",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure"))

## ----robust-se, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("sandwich", quietly = TRUE)----
library(sandwich)
stargazer(m1, m2, m3, m4,
          type             = "text",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure",
                               "Female", "Married"),
          omit             = c("region", "occupation", "industry"),
          vcov             = list(vcovHC(m1, type = "HC1"),
                                  vcovHC(m2, type = "HC1"),
                                  vcovHC(m3, type = "HC1"),
                                  vcovHC(m4, type = "HC1")))

## ----clustered-se, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("sandwich", quietly = TRUE)----
stargazer(m1, m2, m3, m4,
          type             = "text",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure",
                               "Female", "Married"),
          omit             = c("region", "occupation", "industry"),
          vcov             = list(vcovCL(m1, cluster = ~industry, data = wage1),
                                  vcovCL(m2, cluster = ~industry, data = wage1),
                                  vcovCL(m3, cluster = ~industry, data = wage1),
                                  vcovCL(m4, cluster = ~industry, data = wage1)))

## ----mixed-se, eval = requireNamespace("wooldridge", quietly = TRUE) && requireNamespace("sandwich", quietly = TRUE)----
stargazer(m1, m2, m3, m4,
          type             = "latex",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure",
                               "Female", "Married"),
          omit             = c("region", "occupation", "industry"),
          column.labels    = c("Baseline", "Demographics",
                               "Region/Occ.", "Full"),
          vcov             = list(vcovHC(m1, type = "HC1"),
                                  vcovCL(m2, cluster = ~industry, data = wage1),
                                  vcovCL(m3, cluster = ~industry, data = wage1),
                                  vcovCL(m4, cluster = ~industry, data = wage1)))

## ----style-stargazer2, eval = requireNamespace("wooldridge", quietly = TRUE)----
stargazer(m1, m2,
          type             = "latex",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure"),
          style            = "stargazer2")   # default

## ----style-aer, eval = requireNamespace("wooldridge", quietly = TRUE)---------
stargazer(m1, m2,
          type             = "latex",
          dep.var.labels   = "log(Wage)",
          covariate.labels = c("Education", "Experience", "Tenure"),
          style            = "aer")

## ----summary-stats, eval = requireNamespace("wooldridge", quietly = TRUE)-----
stargazer(
  wage1[, c("lwage", "educ", "exper", "tenure", "female", "married")],
  type             = "text",
  covariate.labels = c("log(Wage)", "Education", "Experience",
                       "Tenure", "Female", "Married")
)

