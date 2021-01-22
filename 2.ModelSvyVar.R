
##########################################################
#
# 2. Model key survey variables by auxiliary variables
#
#    Updated on 20/01/2021
#
##########################################################

library(dplyr)
library(MASS)  ## stepwise regression
library(DescTools)  ## pseudo R^2
library(ResourceSelection)  ## hoslem.test
library(stargazer)  ## LaTex output


# -------------------------------------------------
# 2.1 Auxiliary variables summary statistics
# -------------------------------------------------
table(data2017$Leeftijdsklasse13)  ## age group
table(data2017$Geslacht)  ## sex
table(data2017$Inkomensklasse)  ## income class
table(data2017$BurgerlijkeStaat)  ## marital status
table(data2017$Stedelijkheid)  ## urbanization level of areas of neighborhood
table(data2017$HerkomstGeneratie)  ## migration background
table(data2017$TypeHH)  ## type of household
table(data2017$OplNivHB)  ## level of education
table(data2017$InschrCWI)  ## receive rent benefit


# -------------------------------------------------
# 2.2 Survey variables summary statistics
# -------------------------------------------------
table(data2017$Health)  ## general health
table(data2017$Smoke)  ## smoke
table(data2017$Obese)  ## obesity


# --------------------------------------------------------
# 2.3 Model key survey variables with auxiliary variables
#     with stepwise probit models
# --------------------------------------------------------
## General health
resHealth <- glm(Health ~ Leeftijdsklasse13 + Geslacht + Inkomensklasse +
                   BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                   TypeHH + OplNivHB + InschrCWI, 
                 family = binomial(link = "probit"), data = data2017) %>%
  stepAIC(trace = F)
summary(resHealth)

## pseudo R^2
PseudoR2(resHealth, c("Nagelkerke", "CoxSnell"))

## Hosmer-Lemeshow test
Health <- data2017$Health[!is.na(data2017$Health)]
Health <- ifelse(Health == "healthy", 1, 0)
hoslem.test(Health, fitted(resHealth), g = 10)

## get prediction on respondents and non-respondents
data2017$Health.prob <- predict(resHealth, data2017, type = "response")
summary(data2017$Health.prob)


# Smoke
resSmoke <- glm(Smoke ~ Leeftijdsklasse13 + Geslacht + Inkomensklasse + 
                  BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                  TypeHH + OplNivHB + InschrCWI, 
                family = binomial(link = "probit"), data = data2017) %>%
  stepAIC(trace = F)
summary(resSmoke)

## pseudo R^2
PseudoR2(resSmoke, c("Nagelkerke", "CoxSnell"))

## Hosmer-Lemeshow test
Smoke <- data2017$Smoke[!is.na(data2017$Smoke)]
Smoke <- ifelse(Smoke == "smoke", 1, 0)
hoslem.test(Smoke, fitted(resSmoke), g = 10)

## get prediction on respondents and non-respondents
data2017$Smoke.prob <- predict(resSmoke, data2017, type = "response")
summary(data2017$Smoke.prob)


# Obese
resObese <- glm(Obese ~ Leeftijdsklasse13 + Geslacht + Inkomensklasse + 
                  BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                  TypeHH + OplNivHB + InschrCWI, 
                family = binomial(link = "probit"), data = data2017) %>%
  stepAIC(trace = F)
summary(resObese)

## pseudo R^2
PseudoR2(resObese, c("Nagelkerke", "CoxSnell"))

## Hosmer-Lemeshow test
Obese <- data2017$Obese[!is.na(data2017$Obese)]
Obese <- ifelse(Obese == "obese", 1, 0)
hoslem.test(Obese, fitted(resObese), g = 10)

## get prediction on respondents and non-respondents
data2017$Obese.prob <- predict(resObese, data2017, type = "response")
summary(data2017$Obese.prob)


# -------------------------------------------------
# 2.4 LaTex output of modeling results
# -------------------------------------------------
fileConn <- file("reg_output.txt")
writeLines(stargazer(resHealth, resSmoke, resObese, title = "Probit models of survey variables predicted by auxiliary data",
                     align = T, dep.var.labels = c("General Health", "Smoking", "Obesity"),
                     no.space = T), fileConn)
close(fileConn)
