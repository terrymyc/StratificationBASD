
####################################################
#
# Functions for Gibbs Sampler
#
# Updated on 13/07/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This code file defines 3 functions:
# 1. Gibbs Sampler for estimating design parameters
# 2. Gibbs Sampler for deriving quality and cost indicators
# 3. Gibbs sampler for stratification-assessed propensity models

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(MCMCpack)  ## sampling from inverse gamma distribution
library(truncnorm) ## sampling from truncated normal distribution

## parallel programming requirements
library(foreach)
library(parallel)
library(doParallel)


# --------------------------------------------------------
# 1. Gibbs Sampler for estimating design parameters
# --------------------------------------------------------
DesignParaEst <- function(data, strata, n.iter, n.burn, n.chain = 1, seed = 12345) {
  # This function estimates response propensity and cost per stratum and strategy.
  # @param data: data contain response, cost, covariates, strata variables
  # @param strata: column name of the specified stratum indicator variable
  # @param n.iter: number of iterations
  # @param n.burn: number of burn-in iterations
  # @param n.chain: number of MCMC chains (either 1 or 2)
  # @param seed: seed of random number generator
  
  # ------------------------------------------------------
  # Preparation
  # ------------------------------------------------------
  
  ## sample size
  n.sample <- nrow(data)
  
  ## number of strata
  n.strata <- length(unique(strata))

  ## model matrix
  X.strata <- model.matrix(~ strata, data = data)
  #svy.prob <- data[, c("Health.prob", "Smoke.prob", "Obese.prob")]
  #X <- cbind(rep(1, nrow(svy.prob)), as.matrix(svy.prob))
  
  # ------------------------------------------------------
  # Fit response propensity and cost models
  # ------------------------------------------------------

  fit.prop.s1 <- MCMCprobit(FinalRespPhase1 ~ strata, data = data,
  #fit.prop.s1 <- MCMCprobit(FinalRespPhase1 ~ Health.prob + Smoke.prob + Obese.prob, data = data,
                            burnin = n.burn, mcmc = n.iter, seed = seed,
                            b0 = 0, B0 = 0.001)
  fit.prop.s2 <- MCMCprobit(FinalRespPhase2 ~ strata, data = data,
  #fit.prop.s2 <- MCMCprobit(FinalRespPhase2 ~ Health.prob + Smoke.prob + Obese.prob, data = data,
                            burnin = n.burn, mcmc = n.iter, seed = seed,
                            b0 = 0, B0 = 0.001)
  fit.prop.s3 <- MCMCprobit(FinalRespPhase3 ~ strata, data = data,
  #fit.prop.s3 <- MCMCprobit(FinalRespPhase3 ~ Health.prob + Smoke.prob + Obese.prob, data = data,
                            burnin = n.burn, mcmc = n.iter, seed = seed,
                            b0 = 0, B0 = 0.001)
  fit.cost.s2 <- MCMCregress(kostenTotaalPhase2 ~ strata, data = data,
                             burnin = n.burn, mcmc = n.iter, seed = seed,
                             b0 = 0, B0 = 0.001)
  fit.cost.s3 <- MCMCregress(kostenTotaalPhase23 ~ strata, data = data,
                             burnin = n.burn, mcmc = n.iter, seed = seed,
                             b0 = 0, B0 = 0.001)
  if (n.chain == 2) {
    fit.prop.s1.start0 <- MCMCprobit(FinalRespPhase1 ~ strata, data = data,
                                     burnin = n.burn, mcmc = n.iter, seed = seed,
                                     beta.start = 0, b0 = 0, B0 = 0.001)
    fit.prop.s2.start0 <- MCMCprobit(FinalRespPhase2 ~ strata, data = data,
                                     burnin = n.burn, mcmc = n.iter, seed = seed,
                                     beta.start = 0, b0 = 0, B0 = 0.001)
    fit.prop.s3.start0 <- MCMCprobit(FinalRespPhase3 ~ strata, data = data,
                                     burnin = n.burn, mcmc = n.iter, seed = seed,
                                     beta.start = 0, b0 = 0, B0 = 0.001)
    fit.cost.s2.start0 <- MCMCregress(kostenTotaalPhase2 ~ strata, data = data,
                                      burnin = n.burn, mcmc = n.iter, seed = seed,
                                      beta.start = 0, b0 = 0, B0 = 0.001)
    fit.cost.s3.start0 <- MCMCregress(kostenTotaalPhase23 ~ strata, data = data,
                                      burnin = n.burn, mcmc = n.iter, seed = seed,
                                      beta.start = 0, b0 = 0, B0 = 0.001)
    samples.prop.s1 <- list(as.matrix(fit.prop.s1), as.matrix(fit.prop.s1.start0))
    samples.prop.s2 <- list(as.matrix(fit.prop.s2), as.matrix(fit.prop.s2.start0))
    samples.prop.s3 <- list(as.matrix(fit.prop.s3), as.matrix(fit.prop.s3.start0))
    samples.cost.s2 <- list(as.matrix(fit.cost.s2), as.matrix(fit.cost.s2.start0))
    samples.cost.s3 <- list(as.matrix(fit.cost.s3), as.matrix(fit.cost.s3.start0))
  }
  
  # ------------------------------------------------------
  # Estimate response propensity per stratum and strategy
  # ------------------------------------------------------
  
  p.g.s1 <- foreach(i = 1:n.iter, .combine = "rbind") %dopar%
    {
      tapply(t(pnorm(X.strata %*% fit.prop.s1[i, ])), strata, mean)
      #t(unique(pnorm(X.strata %*% fit.prop.s1[i, ])))
      #tapply(t(pnorm(X %*% fit.prop.s1[i, ])), strata, mean)
    }
  p.g.s2 <- foreach(i = 1:n.iter, .combine = "rbind") %dopar%
    {
      tapply(t(pnorm(X.strata %*% fit.prop.s2[i, ])), strata, mean)
      #t(unique(pnorm(X.strata %*% fit.prop.s2[i, ])))
      #tapply(t(pnorm(X %*% fit.prop.s2[i, ])), strata, mean)
    }
  p.g.s3 <- foreach(i = 1:n.iter, .combine = "rbind") %dopar%
    {
      tapply(t(pnorm(X.strata %*% fit.prop.s3[i, ])), strata, mean)
      #t(unique(pnorm(X.strata %*% fit.prop.s3[i, ])))
      #tapply(t(pnorm(X %*% fit.prop.s3[i, ])), strata, mean)
    }

  # ------------------------------------------------------
  # return models or estimates
  # ------------------------------------------------------

  if (n.chain == 2) {
    final.list <- list(samples.prop.s1 = samples.prop.s1, 
                       samples.prop.s2 = samples.prop.s2,
                       samples.prop.s3 = samples.prop.s3,
                       samples.cost.s2 = samples.cost.s2,
                       samples.cost.s3 = samples.cost.s3)
  } else {
    final.list <- list(p.g.s1 = p.g.s1, p.g.s2 = p.g.s2, p.g.s3 = p.g.s3)
  }
  return(final.list)
}










# --------------------------------------------------------
# 2. Gibbs Sampler for deriving quality and cost indicators
# --------------------------------------------------------

IndicatorsEst <- function(data, strata, allocation, n.iter, n.burn, seed = 12345) {
  # This function estimates response rate, coefficient of variation of response rate,
  # and budget per respondent for every design solution.
  # @param data: data contain response, cost, covariates, strata variables
  # @param strata: column name of the specified stratum indicator variable
  # @param allocation: matrix of all design solutions
  # @param n.iter: number of iterations
  # @param n.burn: number of burn-in iterations
  # @param seed: seed of random number generator

  # ------------------------------------------------------
  # Preparation
  # ------------------------------------------------------

  ## sample size
  n.sample <- nrow(data)
  
  ## number of strata
  n.strata <- length(unique(strata))
  
  ## number of solutions
  n.allocation <- nrow(allocation)
  
  ## create empty matrix to store allocated outcomes
  resp.allocated <- matrix(NA, nrow = n.sample, ncol = n.allocation)
  cost.allocated <- matrix(NA, nrow = n.sample, ncol = n.allocation)
  
  ## create empty matrix to store predicted outcomes
  cost.tot <- matrix(NA, nrow = n.sample, ncol = n.iter)
  B <- matrix(NA, nrow = n.iter, ncol = n.allocation)
  
  ## observed response outcome and cost per strategy
  resp <- cbind(data$FinalRespPhase1, data$FinalRespPhase2, data$FinalRespPhase3)
  cost <- cbind(rep(NA, n.sample), data$kostenTotaalPhase2, data$kostenTotaalPhase23)
  
  ## stratum indicator
  strata.indicator <- strata
  
  ## model matrix
  ## matrix of covariates for each phase
  X.strata <- model.matrix(~ strata, data = data)
  #svy.prob <- data[, c("Health.prob", "Smoke.prob", "Obese.prob")]
  #X <- cbind(rep(1, nrow(svy.prob)), as.matrix(svy.prob))

  ## construct response outcome and cost under the specific allocation
  for (a in 1:n.allocation) {
    for (i in 1:n.sample) {
      strategy.index <- allocation[a, strata.indicator[i]]
      resp.allocated[i, a] <- resp[i, strategy.index]
      cost.allocated[i, a] <- cost[i, strategy.index]
    }
  }
  
  # ------------------------------------------------------
  # Fit response propensity and cost models under each allocation
  # ------------------------------------------------------
  
  ## fit response propensity models
  fit.prop <- foreach(a = 1:n.allocation, .packages = "MCMCpack") %dopar%
    {
      MCMCprobit(resp.allocated[, a] ~ strata,
      #MCMCprobit(resp.allocated[, a] ~ data$Health.prob + data$Smoke.prob + data$Obese.prob,
                 burnin = n.burn, mcmc = n.iter, seed = seed,
                 b0 = 0, B0 = 0.001)
    }
  
  ## formatting function
  comb <- function(x, ...) {
    mapply(rbind, x, ..., SIMPLIFY = F)
    #lapply(seq_along(x),
    #       function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  
  ## compute RR and CV
  RR_CV <- foreach(f = 1:length(fit.prop), .combine = "cbind") %:%
    foreach(i = 1:n.iter, .combine = "comb") %dopar%
    {
      prop <- pnorm(X.strata %*% fit.prop[[f]][i, ])
      #prop <- pnorm(X %*% fit.prop[[f]][i, ])
      RR <- apply(prop, 2, mean)
      CV <- apply(prop, 2, sd)/RR
      list(RR, CV)
    }
  RR <- do.call(cbind, RR_CV[1, ])
  CV <- do.call(cbind, RR_CV[2, ])

  ## fit cost models
  for (a in 1:n.allocation) {
    ### index of units included in the cost model
    index.s23 <- !is.na(cost.allocated[, a])
    index.s1 <- is.na(cost.allocated[, a])
    
    ### only fit cost models when at least two strata receive F2F treatment
    ### make sure stratum indicator variable has at least two levels
    if (sum(allocation[a, ] == 1) < n.strata - 1) {
      ### strata indicator dedicated to the specific allocation
      strata.cost <- droplevels(strata.indicator[index.s23])
      ### fit cost model under the specific allocation
      X.strata.cost <- model.matrix( ~ strata.cost)
      fit.cost <- MCMCregress(cost.allocated[, a] ~ strata,
                              burnin = n.burn, mcmc = n.iter, seed = seed,
                              b0 = 0, B0 = 0.001)
      ### estimate costs under the specific allocation
      for (i in 1:n.iter) {
        set.seed(seed)
        cost.tot[index.s23, i] <- 1.23 +
          rtruncnorm(nrow(X.strata.cost), 
                     mean = X.strata.cost %*% fit.cost[i, -length(fit.cost[i, ])], 
                     sd = sqrt(fit.cost[i, length(fit.cost[i, ])]),
                     a = 0, b = Inf)
        cost.tot[index.s1, i] <- 1.23
      }
      rm(.Random.seed, envir = .GlobalEnv)
    ### estimate costs by sampling for solutions that cannot be modeled
    } else {
      for (i in 1:n.iter) {
        set.seed(i)
        cost.tot[index.s23, i] <- 1.23 +
          rtruncnorm(sum(na.omit(index.s23)), 
                     mean = mean(cost.allocated[index.s23, a]), 
                     sd = sd(cost.allocated[index.s23, a]), 
                     a = 0, b = Inf)
        cost.tot[index.s1, i] <- 1.23
      }
      rm(.Random.seed, envir = .GlobalEnv)
    }

    B[, a] <- apply(cost.tot, 2, mean)/RR[, a]
  }
  
  # ------------------------------------------------------
  # return quality and cost indicators
  # ------------------------------------------------------

  final.list <- list(RR = RR, CV = CV, B = B)
  
  return(final.list)
}










# -------------------------------------------------------------------------------
# 3. Gibbs sampler for stratification-assessed propensity models
# -------------------------------------------------------------------------------

AssessStratification <- function(data, strata, allocation, n.iter, n.burn, n.chain = 1, seed = 12345) {
  # This function models individual response propensities with respect to predicted survey variables.
  # @param data: data contain response, cost, covariates, strata variables
  # @param strata: column name of the specified stratum indicator variable
  # @param allocation: matrix of all design solutions
  # @param n.iter: number of iterations
  # @param n.burn: number of burn-in iterations
  # @param n.chain: number of MCMC chains (either 1 or 2)
  # @param seed: seed of random number generator
  
  # ------------------------------------------------------
  # Preparation
  # ------------------------------------------------------

  ## sample size
  n.sample <- nrow(data)
  
  ## number of solutions
  n.allocation <- nrow(allocation)
  
  ## create empty matrix to store response outcome and cost
  resp.allocated <- matrix(NA, nrow = n.sample, ncol = n.allocation)
  
  ## observed response outcome and cost per strategy
  resp <- cbind(data$FinalRespPhase1, data$FinalRespPhase2, data$FinalRespPhase3)
  
  ## matrix of covariates for each phase
  svy.prob <- data[, c("Health.prob", "Smoke.prob", "Obese.prob")]
  X <- cbind(rep(1, nrow(svy.prob)), as.matrix(svy.prob))

  ## stratum indicator
  strata.indicator <- strata

  ## construct response outcome under the specific allocation
  for (a in 1:n.allocation) {
    for (i in 1:n.sample) {
      strategy.index <- allocation[a, strata.indicator[i]]
      resp.allocated[i, a] <- resp[i, strategy.index]
    }
  }
  
  ## create empty matrix to store predicted outcomes
  prop <- matrix(NA, nrow = n.sample, ncol = n.iter)
  RR <- matrix(NA, nrow = n.iter, ncol = n.allocation)
  CV <- matrix(NA, nrow = n.iter, ncol = n.allocation)
  
  # ------------------------------------------------------
  # Fit response propensity models under each allocation
  # ------------------------------------------------------
  
  ## fit response propensity models
  fit.prop <- foreach(a = 1:n.allocation, .packages = "MCMCpack") %dopar%
    {
      MCMCprobit(resp.allocated[, a] ~ data$Health.prob + data$Smoke.prob + data$Obese.prob,
                 burnin = n.burn, mcmc = n.iter, seed = seed,
                 b0 = 0, B0 = 0.001)
    }
  
  if (n.chain == 2) {
    fit.prop.start0 <- foreach(a = 1:n.allocation, .packages = "MCMCpack") %dopar%
      {
        MCMCprobit(resp.allocated[, a] ~ data$Health.prob + data$Smoke.prob + data$Obese.prob,
                   burnin = n.burn, mcmc = n.iter, seed = seed,
                   beta.start = 0, b0 = 0, B0 = 0.001)
      }
  }

  ## formatting function
  comb <- function(x, ...) {
    mapply(rbind, x, ..., SIMPLIFY = F)
    #lapply(seq_along(x),
    #       function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  }
  
  ## compute RR and CV
  RR_CV <- foreach(f = 1:length(fit.prop), .combine = "cbind") %:%
    foreach(i = 1:n.iter, .combine = "comb") %dopar%
    {
      prop <- pnorm(X %*% fit.prop[[f]][i, ])
      RR <- apply(prop, 2, mean)
      CV <- apply(prop, 2, sd)/RR
      list(RR, CV)
    }
  RR <- do.call(cbind, RR_CV[1, ])
  CV <- do.call(cbind, RR_CV[2, ])
  
  # ------------------------------------------------------
  # return models or RR and CV
  # ------------------------------------------------------

  if (n.chain == 2) {
    final.list <- list(fit.prop.c1 = fit.prop,
                       fit.prop.c2 = fit.prop.start0)
  } else {
    final.list <- list(RR = RR, CV = CV)
  }
  
  return(final.list)
}


