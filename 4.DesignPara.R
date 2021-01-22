
######################################################
#
# 4. Bayesian analysis of design parameters
#
#    Updated on 20/01/2021
#
######################################################


# ---------------------------------------------
# 4.1 Create response indicator for each phase
# ---------------------------------------------
## Response and number of visits
## 8 implausible units:
## 3 CAWI respondents have aantalBezoeken > 0  ==> consider as phase 1 respondents
## 5 CAPI respondents were not visited         ==> consider as phase 2 respondents
table(data2017$ModeRespons, data2017$aantalBezoeken)

## Phase 1: All units are approached by CAWI
data2017$ResponsePhase1 <- 1*(data2017$ModeRespons == "CAWI")
table(data2017$ResponsePhase1, data2017$aantalBezoeken)

## Phase 2: Phase 1 nonrespondents are followed up by CAPI-short
data2017$ResponsePhase2 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken <= 3, 1, 0)
data2017[data2017$ResponsePhase1 == 1, "ResponsePhase2"] <- NA
table(data2017$ResponsePhase2, data2017$aantalBezoeken)

## Phase 3: Phase 2 nonrespondents are followed up by CAPI-extended
data2017$ResponsePhase3 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken > 3, 1, 0)
data2017[data2017$aantalBezoeken <= 3, "ResponsePhase3"] <- NA
table(data2017$ResponsePhase3, data2017$aantalBezoeken)


# -----------------------------------------------
# 4.2 Sampling by Gibbs Sampler
# -----------------------------------------------
## fit regression models to response indicators
res.s1 <- glm(ResponsePhase1 ~ Health.prob + Smoke.prob + Obese.prob, 
              family = binomial(link = "probit"), data = data2017)
res.s2 <- glm(ResponsePhase2 ~ Health.prob + Smoke.prob + Obese.prob, 
              family = binomial(link = "probit"), data = data2017)
res.s3 <- glm(ResponsePhase3 ~ Health.prob + Smoke.prob + Obese.prob, 
              family = binomial(link = "probit"), data = data2017)

## use the resulting estimated parameters as starting values
init.s1 <- list(unname(res.s1$coefficients), rep(0, 4))
init.s2 <- list(unname(res.s2$coefficients), rep(0, 4))
init.s3 <- list(unname(res.s3$coefficients), rep(0, 4))

## generate all possible allocation strategies
allocation.matrix <- as.matrix(expand.grid(rep(list(1:3), 6)))

## trigger Gibbs Sampler
set.seed(123)
samples <- GibbsTerry(response = data2017[, c("ResponsePhase1", "ResponsePhase2", "ResponsePhase3")], 
                      covariates = data2017[, c("Health.prob", "Smoke.prob", "Obese.prob")],
                      strata = matrix(data2017[, "strata"], ncol = 1),
                      allocation = allocation.matrix,
                      n.covariates = 3, n.iter = 5000, n.burn = 1000, n.chain = 1,
                      inits.s1 = init.s1, inits.s2 = init.s2, inits.s3 = init.s3)

## assess convergence
plotSamples(samples$samples.s1)
plotSamples(samples$samples.s2)
plotSamples(samples$samples.s3)

## summary statistics of models of response indicators for three phases
summaryGibbs(samples$samples.s1)
summaryGibbs(samples$samples.s2)
summaryGibbs(samples$samples.s3)


# -------------------------------------------------------------
# 4.3 Estimated response propensities per stratum and strategy
# -------------------------------------------------------------
RespPropensity(data2017, samples$prop.g.s1[[1]], samples$prop.g.s2[[1]], samples$prop.g.s3[[1]])

