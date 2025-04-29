
######################################################
#
# 2. Model key survey variables by auxiliary variables
#
#    Updated on 29/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies Bayesian probit regression to model
# three key survey variables.

# Model specification is described in manuscript section 5.3.
# Model results are shown in manuscript appendix B

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# -------------------------------------------------
# 2.1 Auxiliary variables summary statistics
# -------------------------------------------------

table(data2017$Leeftijdsklasse12)  ## age group
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
# --------------------------------------------------------

## Create empty matrix to store posteriors
p.health <- matrix(NA, nrow = nrow(data2017), ncol = 5000)
p.smoke <- matrix(NA, nrow = nrow(data2017), ncol = 5000)
p.obese <- matrix(NA, nrow = nrow(data2017), ncol = 5000)


## Modeling General health
## ---------------------------
X.health <- model.matrix(~ Leeftijdsklasse12 +  Geslacht + Inkomensklasse + 
                           HerkomstGeneratie + BurgerlijkeStaat +
                           TypeHH + OplNivHB + InschrCWI, data = data2017)

### run two MCMC chains (start from MLE and 0)
res.health <- MCMCprobit(Health ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                           HerkomstGeneratie + BurgerlijkeStaat +
                           TypeHH + OplNivHB + InschrCWI, data = data2017, 
                         burnin = 1000, mcmc = 5000, seed = 12345,
                         b0 = 0, B0 = 0.001)
res.health.c2 <- MCMCprobit(Health ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                              HerkomstGeneratie + BurgerlijkeStaat +
                              TypeHH + OplNivHB + InschrCWI, data = data2017, 
                            burnin = 1000, mcmc = 5000, seed = 12345,
                            beta.start = 0,
                            b0 = 0, B0 = 0.001)
samples.health <- list(as.matrix(res.health), as.matrix(res.health.c2))

### DIC for model selection
#DIC(as.matrix(res.health), X.health, data2017$Health)

### posterior predictive distributions
for (i in 1:5000) {
  p.health[, i] <- pnorm(X.health %*% res.health[i, ])
}

### calculate expected values
data2017$Health.prob <- rowMeans(p.health)
summary(data2017$Health.prob)

### Bayesian R-squared
var.fit.health <- apply(p.health, 2, var)
var.res.health <- rep(NA, 5000)
for (i in 1:5000) {
  var.res.health[i] <- mean(p.health[, i] * (1 - p.health[, i]))
}
rsq.health <- var.fit.health/(var.fit.health + var.res.health)
quantile(rsq.health, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
# 0.121 0.13 0.135 0.14 0.149

# 8331
# 13197


## Modeling Smoking
## --------------------
X.smoke <- model.matrix(~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                          BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                          TypeHH + OplNivHB + InschrCWI, data = data2017)

### run two MCMC chains (start from MLE and 0)
res.smoke <- MCMCprobit(Smoke ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                          BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                          TypeHH + OplNivHB + InschrCWI, data = data2017, 
                        burnin = 1000, mcmc = 5000, seed = 12345,
                        b0 = 0, B0 = 0.001)
res.smoke.c2 <- MCMCprobit(Smoke ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                             BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                             TypeHH + OplNivHB + InschrCWI, data = data2017, 
                           burnin = 1000, mcmc = 5000, seed = 12345,
                           beta.start = 0,
                           b0 = 0, B0 = 0.001)
samples.smoke <- list(as.matrix(res.smoke), as.matrix(res.smoke.c2))

### DIC for model selection
#DIC(as.matrix(res.smoke), X.smoke, data2017$Smoke)

### posterior predictive distributions
for (i in 1:5000) {
  p.smoke[, i] <- pnorm(X.smoke %*% res.smoke[i, ])
}

### calculate expected values
data2017$Smoke.prob <- rowMeans(p.smoke)
summary(data2017$Smoke.prob)

### Bayesian R-squared
var.fit.smoke <- apply(p.smoke, 2, var)
var.res.smoke <- rep(NA, 5000)
for (i in 1:5000) {
  var.res.smoke[i] <- mean(p.smoke[, i] * (1 - p.smoke[, i]))
}
rsq.smoke <- var.fit.smoke/(var.fit.smoke + var.res.smoke)
quantile(rsq.smoke, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
# 0.0843 0.0925 0.097 0.1016 0.1105
# 8297


## Obese
## ----------------------
X.obese <- model.matrix(~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                        data = data2017)

### run two MCMC chains (start from MLE and 0)
res.obese <- MCMCprobit(Obese ~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                        data = data2017, burnin = 1000, mcmc = 5000, seed = 12345,
                        b0 = 0, B0 = 0.001)
res.obese.c2 <- MCMCprobit(Obese ~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                           data = data2017, burnin = 1000, mcmc = 5000, seed = 12345,
                           beta.start = 0,
                           b0 = 0, B0 = 0.001)
samples.obese <- list(as.matrix(res.obese), as.matrix(res.obese.c2))

### DIC for model selection
#DIC(as.matrix(res.obese), X.obese, data2017$Obese)

### posterior predictive distributions
for (i in 1:5000) {
  p.obese[, i] <- pnorm(X.obese %*% res.obese[i, ])
}

### calculate expected values
data2017$Obese.prob <- rowMeans(p.obese)
summary(data2017$Obese.prob)

### Bayesian R-squared
var.fit.obese <- apply(p.obese, 2, var)
var.res.obese <- rep(NA, 5000)
for (i in 1:5000) {
  var.res.obese[i] <- mean(p.obese[, i] * (1 - p.obese[, i]))
}
rsq.obese <- var.fit.obese/(var.fit.obese + var.res.obese)
quantile(rsq.obese, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))
# 0.0312 0.0355 0.0378 0.0402 0.045
# 8178
