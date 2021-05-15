
######################################################
#
# 2. Model key survey variables by auxiliary variables
#
#    (SECTION 5.3, APPENDIX B & C in manuscript)
#
#    Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies Bayesian probit regression to model
# three key survey variables.

# Model specification is described in manuscript section 5.3.
# Model results are shown in manuscript appendix B

# Table B.1 in manuscript records the model results.

# Figure C.1 in manuscript shows the trace plot of Gibbs draws.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(MCMCpack)  ## Bayesian probit model


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

### MLE estimates as starting values
mle.health <- glm(Health ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                    HerkomstGeneratie + BurgerlijkeStaat +
                    TypeHH + OplNivHB + InschrCWI,
                 family = binomial(link = "probit"), data = data2017)

### supply starting values
init.health <- list(matrix(rep(0, ncol(X.health)), ncol = 1), 
                    matrix(unname(mle.health$coefficients), ncol = 1))

### run two MCMC chains
res.health <- MCMCprobit(Health ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                           HerkomstGeneratie + BurgerlijkeStaat +
                           TypeHH + OplNivHB + InschrCWI, data = data2017, 
                         burnin = 1000, mcmc = 5000, seed = 123,
                         b0 = init.health[[1]],
                         B0 = diag(0.001, ncol(X.health)))
res.health.c2 <- MCMCprobit(Health ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                           HerkomstGeneratie + BurgerlijkeStaat +
                           TypeHH + OplNivHB + InschrCWI, data = data2017, 
                         burnin = 1000, mcmc = 5000, seed = 123,
                         b0 = init.health[[2]],
                         B0 = diag(0.001, ncol(X.health)))
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
var.res.health <- mean(data2017$Health.prob * (1 - data2017$Health.prob))
rsq.health <- var.fit.health/(var.fit.health + var.res.health)
quantile(rsq.health, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))




## Modeling Smoking
## --------------------
X.smoke <- model.matrix(~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                          BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                          TypeHH + OplNivHB + InschrCWI, data = data2017)

### MLE estimates as starting values
mle.smoke <- glm(Smoke ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                   BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                   TypeHH + OplNivHB + InschrCWI, 
                family = binomial(link = "probit"), data = data2017)

### supply starting values
init.smoke <- list(matrix(rep(0, ncol(X.smoke)), ncol = 1), 
                   matrix(unname(mle.smoke$coefficients), ncol = 1))

### run two MCMC chains
res.smoke <- MCMCprobit(Smoke ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                          BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                          TypeHH + OplNivHB + InschrCWI, data = data2017, 
                        burnin = 1000, mcmc = 5000, seed = 123,
                        b0 = init.smoke[[1]],
                        B0 = diag(0.001, ncol(X.smoke)))
res.smoke.c2 <- MCMCprobit(Smoke ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse + 
                          BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie + 
                          TypeHH + OplNivHB + InschrCWI, data = data2017, 
                        burnin = 1000, mcmc = 5000, seed = 123,
                        b0 = init.smoke[[2]],
                        B0 = diag(0.001, ncol(X.smoke)))
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
var.res.smoke <- mean(data2017$Smoke.prob * (1 - data2017$Smoke.prob))
rsq.smoke <- var.fit.smoke/(var.fit.smoke + var.res.smoke)
quantile(rsq.smoke, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))




## Obese
## ----------------------
X.obese <- model.matrix(~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                        data = data2017)

### MLE estimates as starting values
mle.obese <- glm(Obese ~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB,
                 family = binomial(link = "probit"), data = data2017)

### supply starting values
init.obese <- list(matrix(rep(0, ncol(X.obese)), ncol = 1), 
                   matrix(unname(mle.obese$coefficients), ncol = 1))

### run two MCMC chains
res.obese <- MCMCprobit(Obese ~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                        data = data2017, burnin = 1000, mcmc = 5000, seed = 123,
                        b0 = init.obese[[1]],
                        B0 = diag(0.001, ncol(X.obese)))
res.obese.c2 <- MCMCprobit(Obese ~ Leeftijdsklasse12 + Inkomensklasse + OplNivHB, 
                        data = data2017, burnin = 1000, mcmc = 5000, seed = 123,
                        b0 = init.obese[[2]],
                        B0 = diag(0.001, ncol(X.obese)))
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
var.res.obese <- mean(data2017$Obese.prob * (1 - data2017$Obese.prob))
rsq.obese <- var.fit.obese/(var.fit.obese + var.res.obese)
quantile(rsq.obese, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))


# --------------------------------------------------------
# 2.4 Store model results to build Table B.1
# --------------------------------------------------------

TableB1 <- file("Tables/TableB1.txt")
sink(TableB1, type = "output")
print(summary(res.health))
print(summary(res.smoke))
print(summary(res.obese))
sink()
close(TableB1)


# ------------------------------------------------------------
# Figure C.1 Survey variable model parameters
# ------------------------------------------------------------

png("Figures/FigureC1.png", width = 8, height = 9, units = "in", res = 400)

par(mfrow = c(3, 1))
# trace plot for health model
plot(samples.health[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.health[[2]][, "HerkomstGeneratieEerste generatie niet-Westers"]), 
              max(samples.health[[2]][, 1])),
     main = "General Health")
lines(samples.health[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.health[[1]][, "Leeftijdsklasse123"], type = "l", col = "coral")
lines(samples.health[[2]][, "Leeftijdsklasse123"], type = "l", col = "deepskyblue")
lines(samples.health[[1]][, "HerkomstGeneratieEerste generatie niet-Westers"], type = "l", col = "coral")
lines(samples.health[[2]][, "HerkomstGeneratieEerste generatie niet-Westers"], type = "l", col = "deepskyblue")
lines(samples.health[[1]][, "TypeHH2. 2"], type = "l", col = "coral")
lines(samples.health[[2]][, "TypeHH2. 2"], type = "l", col = "deepskyblue")


# trace plot for smoke model
plot(samples.smoke[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.smoke[[1]][, 1]), 
              max(samples.smoke[[2]][, "Leeftijdsklasse123"])),
     main = "Smoking")
lines(samples.smoke[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.smoke[[1]][, "Leeftijdsklasse123"], type = "l", col = "coral")
lines(samples.smoke[[2]][, "Leeftijdsklasse123"], type = "l", col = "deepskyblue")
lines(samples.smoke[[1]][, "HerkomstGeneratieEerste generatie niet-Westers"], type = "l", col = "coral")
lines(samples.smoke[[2]][, "HerkomstGeneratieEerste generatie niet-Westers"], type = "l", col = "deepskyblue")
lines(samples.smoke[[1]][, "GeslachtV"], type = "l", col = "coral")
lines(samples.smoke[[2]][, "GeslachtV"], type = "l", col = "deepskyblue")


# trace plot for obese model
plot(samples.obese[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.obese[[1]][, 1]), 
              max(samples.obese[[2]][, "Leeftijdsklasse123"])),
     main = "Obesity")
lines(samples.obese[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.obese[[1]][, "Leeftijdsklasse123"], type = "l", col = "coral")
lines(samples.obese[[2]][, "Leeftijdsklasse123"], type = "l", col = "deepskyblue")
lines(samples.obese[[1]][, "Inkomensklasse5. 80-100% perc"], type = "l", col = "coral")
lines(samples.obese[[2]][, "Inkomensklasse5. 80-100% perc"], type = "l", col = "deepskyblue")
lines(samples.obese[[1]][, "OplNivHB5. Master HBO-WO"], type = "l", col = "coral")
lines(samples.obese[[2]][, "OplNivHB5. Master HBO-WO"], type = "l", col = "deepskyblue")

dev.off()
