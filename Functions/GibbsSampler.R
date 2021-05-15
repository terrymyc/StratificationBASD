
####################################################
#
# Functions for Gibbs Sampler
#
# Updated on 10/05/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This code file defines 3 functions:
# 1. Gibbs Sampler for design parameters with optimization
# 2. Gibbs Sampler for design parameters without optimization
# 3. Gibbs sampler for stratification-assessed propensity models

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(mvtnorm)  ## sampling from multivariate normal distribution
library(truncnorm)  ## sampling from truncated normal distribution
library(MCMCpack)  ## sampling from inverse gamma distribution
library(fastDummies)  ## dummy coding of categorical variables


# --------------------------------------------------------
# 1. Gibbs Sampler for design parameters with optimization
# --------------------------------------------------------
GibbsTerry <- function(data, n.iter, n.burn, n.chain, allocation,
                       inits.s1, inits.s2, inits.s3,
                       inits.s2.cost, inits.s3.cost,
                       # prior specification for response propensities models
                       mu = rep(0, length(unique(data$strata))), 
                       sigma = diag(1000, length(unique(data$strata))),
                       # prior specification for cost models
                       mu.cost = rep(0, length(unique(data$strata))),
                       sigma.cost = diag(1000, length(unique(data$strata))),
                       a = 0.001, b = 0.001) {
  # data: data contain response, cost, covariates, strata variables
  # allocation: a 81*6 matrix of solutions that allocate strategies to 6 strata
  # n.iter: number of iterations
  # n.chain: number of chains
  # inits.s1: initial values of parameters for modeling phase 1 response
  # inits.s2: initial values of parameters for modeling phase 2 response
  # inits.s3: initial values of parameters for modeling phase 3 response
  # inits.s2.cost: initial values of parameters for modeling phase 2 total costs
  # inits.s3.cost: initial values of parameters for modeling phase 3 total costs
  # mu/mu.cost/sigma/sigma.cost/a/b: hyperparameter of prior distributions, non-informative by default
  
  
  # vector of response for each phase
  response <- data[, c("FinalRespPhase1", "FinalRespPhase2", "FinalRespPhase3")]
  y.s1 <- as.matrix(c(na.omit(response[, 1])))  # all 11565 units reach phase 1
  y.s2 <- as.matrix(c(na.omit(response[, 2])))  # only contain 7153 units reaching phase 2
  y.s3 <- as.matrix(c(na.omit(response[, 3])))  # only contain 1438 units reaching phase 3
  
  # vector of cost
  cost <- data[, c("kostenTotaalPhase2", "kostenTotaalPhase23")]
  c.s2 <- as.matrix(c(na.omit(cost[, 1])))
  c.s3 <- as.matrix(c(na.omit(cost[, 2])))
  
  # vector of strata for each phase
  # indicate which stratum a unit is in
  strata.indicator = matrix(data[, "strata"], ncol = 1)
 
  # dummy coding strata as covariates
  strata <- dummy_cols(data, select_columns = "strata", 
                       remove_first_dummy = T)[, (ncol(data) + 1):(ncol(data) + length(unique(strata.indicator)) - 1)]
  X <- cbind(rep(1, nrow(response)), as.matrix(strata))
  X.s2.cost <- cbind(rep(1, length(c.s2)), as.matrix(strata[!is.na(cost[, 1]), ]))
  X.s3.cost <- cbind(rep(1, length(c.s3)), as.matrix(strata[!is.na(cost[, 2]), ]))
  
  # posterior covariance matrix of regression coefficients for each phase
  # based on formula A2 in JSSM 2018 paper
  sigma.full.s1 <- solve(solve(sigma) + crossprod(X, X))
  sigma.full.s2 <- solve(solve(sigma) + crossprod(X, X))
  sigma.full.s3 <- solve(solve(sigma) + crossprod(X, X))
  
  # posterior shape parameter of variance of error term
  # based on formula A13 in JSSM 2018 paper
  a.full.s2.cost <- a + length(c.s2)/2
  a.full.s3.cost <- a + length(c.s3)/2
  
  # create empty lists to store different chains
  # Gibbs samples for estimating regression parameters in each phase
  samples.s1 <- vector(mode = "list", length = n.chain)
  samples.s2 <- vector(mode = "list", length = n.chain)
  samples.s3 <- vector(mode = "list", length = n.chain)
  samples.s2.cost <- vector(mode = "list", length = n.chain)
  samples.s3.cost <- vector(mode = "list", length = n.chain)
  # Gibbs samples for estimating response propensities per stratum and strategy
  # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
  prop.g.s1 <- vector(mode = "list", length = n.chain)  # web
  prop.g.s2 <- vector(mode = "list", length = n.chain)  # web -> f2f-short
  prop.g.s3 <- vector(mode = "list", length = n.chain)  # web -> f2f-short -> f2f-extended
  # based on Gibbs samples of response propensities, calculating the mean and CV
  response.rate <- vector(mode = "list", length = n.chain)
  cv.response <- vector(mode = "list", length = n.chain)
  budget <- vector(mode = "list", length = n.chain)
  
  for (c in 1:n.chain) {
    
    # -----------------------------
    # Preparation before start sampling
    # -----------------------------
    
    # supply the initial values of regression parameters
    beta.s1 <- inits.s1[[c]]
    beta.s2 <- inits.s2[[c]]
    beta.s3 <- inits.s3[[c]]
    mu.z.s1 <- X %*% beta.s1
    mu.z.s2 <- X %*% beta.s2
    mu.z.s3 <- X %*% beta.s3
    beta.s2.cost <- inits.s2.cost[[c]]
    beta.s3.cost <- inits.s3.cost[[c]]
    
    # create an empty matrix to store estimated regression parameters and RR & CV IN each phase
    res.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s2.cost <- matrix(NA, nrow = n.iter + n.burn, ncol = 2 + length(strata))
    res.s3.cost <- matrix(NA, nrow = n.iter + n.burn, ncol = 2 + length(strata))
   
    if (length(strata) == 5) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                      "strata6", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 6) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 7) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 8) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 9) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    }
    
    # create an empty vector to store sampled latent variable for each phase
    # updated in every iteration
    z.s1 <- rep(NA, length(y.s1))
    z.s2 <- rep(NA, length(y.s2))
    z.s3 <- rep(NA, length(y.s3))
    
    # create an empty matrix to store sampled response propensities per stratum and strategy
    # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
    # at the end of sampling per chain, these matrices will be assigned to the empty lists created before sampling starts
    p.g.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web
    p.g.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web -> f2f-short
    p.g.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web -> f2f-short -> f2f-extended
    
    # ---------------------------------------------
    # Additional preparation for optimization
    # ---------------------------------------------
    
    # create an empty matrix to store sampled response propensities per stratum and strategy
    # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
    # updated in every iteration
    p.g.s <- matrix(NA, nrow = 3, ncol = length(unique(strata.indicator)))
    
    # create an empty matrix to store sampled total costs per stratum and strategy
    c.g.s <- matrix(NA, nrow = 3, ncol = length(unique(strata.indicator)))
    c.g.s[1,] <- 1.23
    
    # sample size per stratum
    n.g <- as.numeric(table(strata.indicator))
    
    # response propensities and total costs per stratum under an allocated strategy
    rho.g <- rep(NA, length(unique(strata.indicator)))
    cost.g <- rep(NA, length(unique(strata.indicator)))
    
    # create an empty matrix to store RR and CV for each solution
    RR <- matrix(NA, nrow = n.iter, ncol = nrow(allocation))
    CV <- matrix(NA, nrow = n.iter, ncol = nrow(allocation))
    B <- matrix(NA, nrow = n.iter, ncol = nrow(allocation))
    
    
    # -----------------------------
    # START SAMPLING
    # -----------------------------
    for (i in 1:(n.iter + n.burn)) {
      
      # --------------------------------
      # Modeling response propensities
      # --------------------------------
      
      # sample latent variables from truncated normal posterior distributions for each phase
      z.s1[y.s1 == 0] <- rtruncnorm(length(y.s1) - sum(y.s1), mean = mu.z.s1[y.s1 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s1[y.s1 == 1] <- rtruncnorm(sum(y.s1), mean = mu.z.s1[y.s1 == 1], sd = 1,
                                    a = 0, b = Inf)
      z.s2[y.s2 == 0] <- rtruncnorm(length(y.s2) - sum(y.s2), mean = mu.z.s2[y.s2 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s2[y.s2 == 1] <- rtruncnorm(sum(y.s2), mean = mu.z.s2[y.s2 == 1], sd = 1,
                                    a = 0, b = Inf)
      z.s3[y.s3 == 0] <- rtruncnorm(length(y.s3) - sum(y.s3), mean = mu.z.s3[y.s3 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s3[y.s3 == 1] <- rtruncnorm(sum(y.s3), mean = mu.z.s3[y.s3 == 1], sd = 1,
                                    a = 0, b = Inf)
      
      # posterior mean vector of regression coefficients for each phase
      # based on formula A3 in JSSM 2018 paper
      mu.full.s1 <- sigma.full.s1 %*% (solve(sigma) %*% mu + crossprod(X, z.s1))
      mu.full.s2 <- sigma.full.s2 %*% (solve(sigma) %*% mu + crossprod(X, z.s2))
      mu.full.s3 <- sigma.full.s3 %*% (solve(sigma) %*% mu + crossprod(X, z.s3))
      
      # sample regression coefficients from multivariate normal posterior distribution
      # based on formula A1 in JSSM 2018 paper
      beta.s1 <- as.numeric(rmvnorm(1, mu.full.s1, sigma.full.s1))
      beta.s2 <- as.numeric(rmvnorm(1, mu.full.s2, sigma.full.s2))
      beta.s3 <- as.numeric(rmvnorm(1, mu.full.s3, sigma.full.s3))
      
      # store sampled regression coefficients
      res.s1[i, 1:(ncol(X))] <- beta.s1
      res.s2[i, 1:(ncol(X))] <- beta.s2
      res.s3[i, 1:(ncol(X))] <- beta.s3
      
      # posterior mean vector of latent variable for each phase
      mu.z.s1 <- X %*% beta.s1
      mu.z.s2 <- X %*% beta.s2
      mu.z.s3 <- X %*% beta.s3
      
      # compute response propensities per unit IN each phase
      p.s1 <- pnorm(mu.z.s1)  # all units reach phase 1
      p.s2 <- pnorm(mu.z.s2)  # restrict to units reaching phase 2
      p.s3 <- pnorm(mu.z.s3)  # restrict to units reaching phase 3
      
      # compute RR and CV IN each phase
      # just for comparison with results of MLE models
      res.s1[i, "RR"] <- mean(p.s1)
      res.s2[i, "RR"] <- mean(p.s2)
      res.s3[i, "RR"] <- mean(p.s3)
      res.s1[i, "CV"] <- sd(p.s1)/mean(p.s1)
      res.s2[i, "CV"] <- sd(p.s2)/mean(p.s2)
      res.s3[i, "CV"] <- sd(p.s3)/mean(p.s3)
      
      # compute response propensities per stratum and strategy
      # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
      # based on formula 1 & 2 in my report
      for (s in 1:length(unique(strata.indicator))) {
        # s indicates stratum
        # web
        p.g.s1[i, s] <- mean(p.s1[strata.indicator == s])
        # web -> f2f-short
        p.g.s2[i, s] <- mean(p.s2[strata.indicator == s])
        # web -> f2f-short -> f2f-extended
        p.g.s3[i, s] <- mean(p.s3[strata.indicator == s])
      }
      
      # --------------------------------
      # Modeling total costs
      # --------------------------------
      
      # posterior scale parameter of variance of error term
      # based on formula A14 in JSSM 2018 paper
      b.full.s2.cost <- b + 1/2*crossprod(c.s2 - X.s2.cost%*%beta.s2.cost, c.s2 - X.s2.cost%*%beta.s2.cost)
      b.full.s3.cost <- b + 1/2*crossprod(c.s3 - X.s3.cost%*%beta.s3.cost, c.s3 - X.s3.cost%*%beta.s3.cost)
      
      # sample variance parameter from inverse Gamma distribution
      # based on formula A12 in JSSM 2018 paper
      var.s2.cost <- rinvgamma(1, shape = a.full.s2.cost, scale = b.full.s2.cost)
      var.s3.cost <- rinvgamma(1, shape = a.full.s3.cost, scale = b.full.s3.cost)
      
      # posterior covariance matrix of regression coefficients
      # based on formula A10 in JSSM 2018 paper
      sigma.full.s2.cost <- solve((1/var.s2.cost)*crossprod(X.s2.cost, X.s2.cost)+solve(sigma.cost%*%sigma.cost))
      sigma.full.s3.cost <- solve((1/var.s3.cost)*crossprod(X.s3.cost, X.s3.cost)+solve(sigma.cost%*%sigma.cost))
      
      # posterior mean vector of regression coefficients
      # based on formula A11 in JSSM 2018 paper
      mu.full.s2.cost <- sigma.full.s2.cost%*%((1/var.s2.cost)*t(X.s2.cost)%*%c.s2+solve(sigma.cost%*%sigma.cost)%*%mu.cost)
      mu.full.s3.cost <- sigma.full.s3.cost%*%((1/var.s3.cost)*t(X.s3.cost)%*%c.s3+solve(sigma.cost%*%sigma.cost)%*%mu.cost)
      
      # sample slope parameters from normal distribution
      # based on formula A9 in JSSM 2018 paper
      beta.s2.cost <- as.numeric(rmvnorm(1, mu.full.s2.cost, sigma.full.s2.cost))
      beta.s3.cost <- as.numeric(rmvnorm(1, mu.full.s3.cost, sigma.full.s3.cost))
      
      # store estimated slope parameters and variance of error term
      res.s2.cost[i, 1:length(unique(strata.indicator))] <- beta.s2.cost
      res.s3.cost[i, 1:length(unique(strata.indicator))] <- beta.s3.cost
      res.s2.cost[i, (length(unique(strata.indicator))+1)] <- var.s2.cost
      res.s3.cost[i, (length(unique(strata.indicator))+1)] <- var.s3.cost


      # -----------------------------------------------------------------------
      # OPTIMIZATION based on response propensities computed in each iteration after burn-in
      # -----------------------------------------------------------------------
      if (i > n.burn) {
        
        # predictive total costs per unit through ALL phases
        c.s2.pred <- rnorm(nrow(X), mean = X %*% beta.s2.cost, sd = sqrt(var.s2.cost))
        c.s3.pred <- rnorm(nrow(X), mean = X %*% beta.s3.cost, sd = sqrt(var.s3.cost))

        # tabulate response propensities per stratum and strategy
        # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
        for (s in 1:length(unique(strata.indicator))) {
          # s indicates stratum
          # web
          p.g.s[1, s] <- p.g.s1[i, s]
          # web -> f2f-short
          p.g.s[2, s] <- p.g.s2[i, s]
          c.g.s[2, s] <- 1.23*p.g.s[1, s] + mean(c.s2.pred[strata.indicator == s])*(1 - p.g.s[1, s])
          # web -> f2f-short -> f2f-extended
          p.g.s[3, s] <- p.g.s3[i, s]
          c.g.s[3, s] <- 1.23*p.g.s[1, s] + mean(c.s3.pred[strata.indicator == s])*(1 - p.g.s[1, s])
        }
        
        for (j in 1:nrow(allocation)) {
          
          # choose allocation of strategies to strata
          strategy.chosen <- unname(allocation[j,])
          
          # response propensities per stratum under an allocated strategy
          for (s in 1:length(unique(strata.indicator))) {
            rho.g[s] <- p.g.s[strategy.chosen[s], s]
            cost.g[s] <- c.g.s[strategy.chosen[s], s]
          }
         
          rho <- sum(n.g * rho.g)/sum(n.g)
          var.rho <- sum(n.g * (rho.g - rho)^2)/sum(n.g)
          cost.tot <- sum(n.g * cost.g)
          cost.per <- weighted.mean(x = (n.g * cost.g)/(rho.g * n.g), w = n.g/nrow(response))
          # store RR and CV of chosen allocation
          RR[(i - n.burn), j] <- rho
          CV[(i - n.burn), j] <- sqrt(var.rho)/rho
          B[(i - n.burn), j] <- cost.per
          
        }
      }
    }
    
    # burn-in period
    res.s1 <- res.s1[-(1:n.burn),]
    res.s2 <- res.s2[-(1:n.burn),]
    res.s3 <- res.s3[-(1:n.burn),]
    res.s2.cost <- res.s2.cost[-(1:n.burn),]
    res.s3.cost <- res.s3.cost[-(1:n.burn),]
    p.g.s1 <- p.g.s1[-(1:n.burn),]
    p.g.s2 <- p.g.s2[-(1:n.burn),]
    p.g.s3 <- p.g.s3[-(1:n.burn),]
    
    # posterior samples of regression parameters and RR & CV IN each phase
    samples.s1[[c]] <- res.s1
    samples.s2[[c]] <- res.s2
    samples.s3[[c]] <- res.s3
    samples.s2.cost[[c]] <- res.s2.cost
    samples.s3.cost[[c]] <- res.s3.cost
    
    # posterior samples of response propensities per stratum and strategy
    prop.g.s1[[c]] <- p.g.s1  # web
    prop.g.s2[[c]] <- p.g.s2  # web -> f2f-short
    prop.g.s3[[c]] <- p.g.s3  # web -> f2f-short -> f2f-extended
    
    # posterior samples of overall RR & CV for each allocation
    response.rate[[c]] <- RR
    cv.response[[c]] <- CV
    budget[[c]] <- B
    
  }
  
  # return results
  final.list <- list(samples.s1 = samples.s1, samples.s2 = samples.s2, samples.s3 = samples.s3,
                     prop.g.s1 = prop.g.s1, prop.g.s2 = prop.g.s2, prop.g.s3 = prop.g.s3,
                     samples.s2.cost = samples.s2.cost, samples.s3.cost = samples.s3.cost,
                     response.rate = response.rate, cv.response = cv.response, budget = budget)
  return(final.list)
}


# ------------------------------------------------------------
# 2. Gibbs Sampler for design parameters without optimization
# ------------------------------------------------------------
GibbsTerryWoOpt <- function(data, n.iter, n.burn, n.chain,
                       inits.s1, inits.s2, inits.s3,
                       inits.s2.cost, inits.s3.cost,
                       # prior specification for response propensities models
                       mu = rep(0, length(unique(data$strata))), 
                       sigma = diag(1000, length(unique(data$strata))),
                       # prior specification for cost models
                       mu.cost = rep(0, length(unique(data$strata))),
                       sigma.cost = diag(1000, length(unique(data$strata))),
                       a = 0.001, b = 0.001) {
  # data: data contain response, cost, covariates, strata variables
  # n.iter: number of iterations
  # n.chain: number of chains
  # inits.s1: initial values of parameters for modeling phase 1 response
  # inits.s2: initial values of parameters for modeling phase 2 response
  # inits.s3: initial values of parameters for modeling phase 3 response
  # inits.s2.cost: initial values of parameters for modeling phase 2 total costs
  # inits.s3.cost: initial values of parameters for modeling phase 3 total costs
  # mu/mu.cost/sigma/sigma.cost/a/b: hyperparameter of prior distributions, non-informative by default
  
  
  # vector of response for each phase
  response <- data[, c("FinalRespPhase1", "FinalRespPhase2", "FinalRespPhase3")]
  y.s1 <- as.matrix(c(na.omit(response[, 1])))  # all 11565 units reach phase 1
  y.s2 <- as.matrix(c(na.omit(response[, 2])))  # only contain 7153 units reaching phase 2
  y.s3 <- as.matrix(c(na.omit(response[, 3])))  # only contain 1438 units reaching phase 3
  
  # vector of cost
  cost <- data[, c("kostenTotaalPhase2", "kostenTotaalPhase23")]
  c.s2 <- as.matrix(c(na.omit(cost[, 1])))
  c.s3 <- as.matrix(c(na.omit(cost[, 2])))
  
  # vector of strata for each phase
  # indicate which stratum a unit is in
  strata.indicator = matrix(data[, "strata"], ncol = 1)
  
  # dummy coding strata as covariates
  strata <- dummy_cols(data, select_columns = "strata", 
                       remove_first_dummy = T)[, (ncol(data) + 1):(ncol(data) + length(unique(strata.indicator)) - 1)]
  X <- cbind(rep(1, nrow(response)), as.matrix(strata))
  X.s2.cost <- cbind(rep(1, length(c.s2)), as.matrix(strata[!is.na(cost[, 1]), ]))
  X.s3.cost <- cbind(rep(1, length(c.s3)), as.matrix(strata[!is.na(cost[, 2]), ]))
  
  # posterior covariance matrix of regression coefficients for each phase
  # based on formula A2 in JSSM 2018 paper
  sigma.full.s1 <- solve(solve(sigma) + crossprod(X, X))
  sigma.full.s2 <- solve(solve(sigma) + crossprod(X, X))
  sigma.full.s3 <- solve(solve(sigma) + crossprod(X, X))
  
  # posterior shape parameter of variance of error term
  # based on formula A13 in JSSM 2018 paper
  a.full.s2.cost <- a + length(c.s2)/2
  a.full.s3.cost <- a + length(c.s3)/2
  
  # create empty lists to store different chains
  # Gibbs samples for estimating regression parameters in each phase
  samples.s1 <- vector(mode = "list", length = n.chain)
  samples.s2 <- vector(mode = "list", length = n.chain)
  samples.s3 <- vector(mode = "list", length = n.chain)
  samples.s2.cost <- vector(mode = "list", length = n.chain)
  samples.s3.cost <- vector(mode = "list", length = n.chain)
  # Gibbs samples for estimating response propensities per stratum and strategy
  # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
  prop.g.s1 <- vector(mode = "list", length = n.chain)  # web
  prop.g.s2 <- vector(mode = "list", length = n.chain)  # web -> f2f-short
  prop.g.s3 <- vector(mode = "list", length = n.chain)  # web -> f2f-short -> f2f-extended
  
  for (c in 1:n.chain) {
    
    # -----------------------------
    # Preparation before start sampling
    # -----------------------------
    
    # supply the initial values of regression parameters
    beta.s1 <- inits.s1[[c]]
    beta.s2 <- inits.s2[[c]]
    beta.s3 <- inits.s3[[c]]
    mu.z.s1 <- X %*% beta.s1
    mu.z.s2 <- X %*% beta.s2
    mu.z.s3 <- X %*% beta.s3
    beta.s2.cost <- inits.s2.cost[[c]]
    beta.s3.cost <- inits.s3.cost[[c]]
    
    # create an empty matrix to store estimated regression parameters and RR & CV IN each phase
    res.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + length(strata))
    res.s2.cost <- matrix(NA, nrow = n.iter + n.burn, ncol = 2 + length(strata))
    res.s3.cost <- matrix(NA, nrow = n.iter + n.burn, ncol = 2 + length(strata))
    
    if (length(strata) == 5) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 6) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 7) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 8) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else if (length(strata) == 9) {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    } else {
      resp.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "RR", "CV")
      cost.para.names <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                           "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "variance")
      colnames(res.s1) <- resp.para.names
      colnames(res.s2) <- resp.para.names
      colnames(res.s3) <- resp.para.names
      colnames(res.s2.cost) <- cost.para.names
      colnames(res.s3.cost) <- cost.para.names
    }
    
    # create an empty vector to store sampled latent variable for each phase
    # updated in every iteration
    z.s1 <- rep(NA, length(y.s1))
    z.s2 <- rep(NA, length(y.s2))
    z.s3 <- rep(NA, length(y.s3))
    
    # create an empty matrix to store sampled response propensities per stratum and strategy
    # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
    # at the end of sampling per chain, these matrices will be assigned to the empty lists created before sampling starts
    p.g.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web
    p.g.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web -> f2f-short
    p.g.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = length(unique(strata.indicator)))  # web -> f2f-short -> f2f-extended
    
    # -----------------------------
    # START SAMPLING
    # -----------------------------
    for (i in 1:(n.iter + n.burn)) {
      
      # --------------------------------
      # Modeling response propensities
      # --------------------------------
      
      # sample latent variables from truncated normal posterior distributions for each phase
      z.s1[y.s1 == 0] <- rtruncnorm(length(y.s1) - sum(y.s1), mean = mu.z.s1[y.s1 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s1[y.s1 == 1] <- rtruncnorm(sum(y.s1), mean = mu.z.s1[y.s1 == 1], sd = 1,
                                    a = 0, b = Inf)
      z.s2[y.s2 == 0] <- rtruncnorm(length(y.s2) - sum(y.s2), mean = mu.z.s2[y.s2 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s2[y.s2 == 1] <- rtruncnorm(sum(y.s2), mean = mu.z.s2[y.s2 == 1], sd = 1,
                                    a = 0, b = Inf)
      z.s3[y.s3 == 0] <- rtruncnorm(length(y.s3) - sum(y.s3), mean = mu.z.s3[y.s3 == 0], sd = 1,
                                    a = -Inf, b = 0)
      z.s3[y.s3 == 1] <- rtruncnorm(sum(y.s3), mean = mu.z.s3[y.s3 == 1], sd = 1,
                                    a = 0, b = Inf)
      
      # posterior mean vector of regression coefficients for each phase
      # based on formula A3 in JSSM 2018 paper
      mu.full.s1 <- sigma.full.s1 %*% (solve(sigma) %*% mu + crossprod(X, z.s1))
      mu.full.s2 <- sigma.full.s2 %*% (solve(sigma) %*% mu + crossprod(X, z.s2))
      mu.full.s3 <- sigma.full.s3 %*% (solve(sigma) %*% mu + crossprod(X, z.s3))
      
      # sample regression coefficients from multivariate normal posterior distribution
      # based on formula A1 in JSSM 2018 paper
      beta.s1 <- as.numeric(rmvnorm(1, mu.full.s1, sigma.full.s1))
      beta.s2 <- as.numeric(rmvnorm(1, mu.full.s2, sigma.full.s2))
      beta.s3 <- as.numeric(rmvnorm(1, mu.full.s3, sigma.full.s3))
      
      # store sampled regression coefficients
      res.s1[i, 1:(ncol(X))] <- beta.s1
      res.s2[i, 1:(ncol(X))] <- beta.s2
      res.s3[i, 1:(ncol(X))] <- beta.s3
      
      # posterior mean vector of latent variable for each phase
      mu.z.s1 <- X %*% beta.s1
      mu.z.s2 <- X %*% beta.s2
      mu.z.s3 <- X %*% beta.s3
      
      # compute response propensities per unit IN each phase
      p.s1 <- pnorm(mu.z.s1)  # all units reach phase 1
      p.s2 <- pnorm(mu.z.s2)  # restrict to units reaching phase 2
      p.s3 <- pnorm(mu.z.s3)  # restrict to units reaching phase 3
      
      # compute RR and CV IN each phase
      # just for comparison with results of MLE models
      res.s1[i, "RR"] <- mean(p.s1)
      res.s2[i, "RR"] <- mean(p.s2)
      res.s3[i, "RR"] <- mean(p.s3)
      res.s1[i, "CV"] <- sd(p.s1)/mean(p.s1)
      res.s2[i, "CV"] <- sd(p.s2)/mean(p.s2)
      res.s3[i, "CV"] <- sd(p.s3)/mean(p.s3)
      
      # compute response propensities per stratum and strategy
      # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
      # based on formula 1 & 2 in my report
      for (s in 1:length(unique(strata.indicator))) {
        # s indicates stratum
        # web
        p.g.s1[i, s] <- mean(p.s1[strata.indicator == s])
        # web -> f2f-short
        p.g.s2[i, s] <- mean(p.s2[strata.indicator == s])
        # web -> f2f-short -> f2f-extended
        p.g.s3[i, s] <- mean(p.s3[strata.indicator == s])
      }
      
      # --------------------------------
      # Modeling total costs
      # --------------------------------
      
      # posterior scale parameter of variance of error term
      # based on formula A14 in JSSM 2018 paper
      b.full.s2.cost <- b + 1/2*crossprod(c.s2 - X.s2.cost%*%beta.s2.cost, c.s2 - X.s2.cost%*%beta.s2.cost)
      b.full.s3.cost <- b + 1/2*crossprod(c.s3 - X.s3.cost%*%beta.s3.cost, c.s3 - X.s3.cost%*%beta.s3.cost)
      
      # sample variance parameter from inverse Gamma distribution
      # based on formula A12 in JSSM 2018 paper
      var.s2.cost <- rinvgamma(1, shape = a.full.s2.cost, scale = b.full.s2.cost)
      var.s3.cost <- rinvgamma(1, shape = a.full.s3.cost, scale = b.full.s3.cost)
      
      # posterior covariance matrix of regression coefficients
      # based on formula A10 in JSSM 2018 paper
      sigma.full.s2.cost <- solve((1/var.s2.cost)*crossprod(X.s2.cost, X.s2.cost)+solve(sigma.cost%*%sigma.cost))
      sigma.full.s3.cost <- solve((1/var.s3.cost)*crossprod(X.s3.cost, X.s3.cost)+solve(sigma.cost%*%sigma.cost))
      
      # posterior mean vector of regression coefficients
      # based on formula A11 in JSSM 2018 paper
      mu.full.s2.cost <- sigma.full.s2.cost%*%((1/var.s2.cost)*t(X.s2.cost)%*%c.s2+solve(sigma.cost%*%sigma.cost)%*%mu.cost)
      mu.full.s3.cost <- sigma.full.s3.cost%*%((1/var.s3.cost)*t(X.s3.cost)%*%c.s3+solve(sigma.cost%*%sigma.cost)%*%mu.cost)
      
      # sample slope parameters from normal distribution
      # based on formula A9 in JSSM 2018 paper
      beta.s2.cost <- as.numeric(rmvnorm(1, mu.full.s2.cost, sigma.full.s2.cost))
      beta.s3.cost <- as.numeric(rmvnorm(1, mu.full.s3.cost, sigma.full.s3.cost))
      
      # store estimated slope parameters and variance of error term
      res.s2.cost[i, 1:length(unique(strata.indicator))] <- beta.s2.cost
      res.s3.cost[i, 1:length(unique(strata.indicator))] <- beta.s3.cost
      res.s2.cost[i, (length(unique(strata.indicator))+1)] <- var.s2.cost
      res.s3.cost[i, (length(unique(strata.indicator))+1)] <- var.s3.cost
      
    }
    
    # burn-in period
    res.s1 <- res.s1[-(1:n.burn),]
    res.s2 <- res.s2[-(1:n.burn),]
    res.s3 <- res.s3[-(1:n.burn),]
    res.s2.cost <- res.s2.cost[-(1:n.burn),]
    res.s3.cost <- res.s3.cost[-(1:n.burn),]
    p.g.s1 <- p.g.s1[-(1:n.burn),]
    p.g.s2 <- p.g.s2[-(1:n.burn),]
    p.g.s3 <- p.g.s3[-(1:n.burn),]
    
    # posterior samples of regression parameters and RR & CV IN each phase
    samples.s1[[c]] <- res.s1
    samples.s2[[c]] <- res.s2
    samples.s3[[c]] <- res.s3
    samples.s2.cost[[c]] <- res.s2.cost
    samples.s3.cost[[c]] <- res.s3.cost
    
    # posterior samples of response propensities per stratum and strategy
    prop.g.s1[[c]] <- p.g.s1  # web
    prop.g.s2[[c]] <- p.g.s2  # web -> f2f-short
    prop.g.s3[[c]] <- p.g.s3  # web -> f2f-short -> f2f-extended
    
  }
  
  # return results
  final.list <- list(samples.s1 = samples.s1, samples.s2 = samples.s2, samples.s3 = samples.s3,
                     prop.g.s1 = prop.g.s1, prop.g.s2 = prop.g.s2, prop.g.s3 = prop.g.s3,
                     samples.s2.cost = samples.s2.cost, samples.s3.cost = samples.s3.cost)
  return(final.list)
}


# -------------------------------------------------------------------------------
# 3. Gibbs sampler for stratification-assessed propensity models
# -------------------------------------------------------------------------------

GibbsOptimal <- function(data, allocation, n.iter, n.burn, n.chain = 1,
                         # prior specification for response propensities models
                         mu = rep(0, 4), sigma = diag(1000, 4)){
  # matrix of final responses
  response <- data[, c("FinalRespPhase1", "FinalRespPhase2", "FinalRespPhase3")]
  # matrix of covariates for each phase
  # insert an additional column vector of 1 for intercept
  covariates <- data[, c("Health.prob", "Smoke.prob", "Obese.prob")]
  X <- cbind(rep(1, nrow(covariates)), as.matrix(covariates))
  
  # vector of strata for each phase
  # indicate which stratum a unit is in
  strata.indicator <- matrix(data[, "strata"], ncol = 1)
  
  # posterior covariance matrix of regression coefficients
  # based on formula A2 in JSSM 2018 paper
  sigma.full <- solve(solve(sigma) + crossprod(X, X))
  
  samples <- vector(mode = "list", length = nrow(allocation))
  
  # create an empty matrix to store estimated regression parameters and RR & CV IN each phase
  res <- matrix(NA, nrow = n.iter, ncol = 6)
  colnames(res) <- c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV")

  for (j in 1:nrow(allocation)) {
    if (n.chain == 1) {
      samples[[j]] <- list(res)
    } else {
      samples[[j]] <- list(res, res)
    }
  }
    
  for (j in 1:nrow(allocation)) {
    
    # choose allocation of strategies to strata
    strategy.chosen <- unname(allocation[j, ])
    
    # vector of final responses under chosen strategy
    response.list <- list()
    for (g in 1:length(strategy.chosen)) {
      response.list[[g]] <- as.matrix(response[strata.indicator == g, strategy.chosen[g]])
    }
    FinalResp <- do.call(rbind, response.list)
    
    # ----------------------------------
    # Preparation before start sampling
    # ----------------------------------
    
    # fit regression models to final response
    fit <- glm(FinalResp ~ covariates$Health.prob + covariates$Smoke.prob + covariates$Obese.prob, 
               family = binomial(link = "probit"))
    init <- list(unname(fit$coefficients), rep(0, 4))
      
    for (c in 1:n.chain) {

      # supply the initial values of regression parameters
      beta <- init[[c]]
      mu.z <- X %*% beta
      
      # create an empty vector to store sampled latent variable
      # updated in every iteration
      z <- rep(NA, length(FinalResp))
      
      # -----------------------------
      # START SAMPLING
      # -----------------------------
      
      for (i in 1:(n.iter + n.burn)) {
        
        # --------------------------------
        # Modeling response propensities
        # --------------------------------
        
        # sample latent variables from truncated normal posterior distributions for each phase
        z[FinalResp == 0] <- rtruncnorm(length(FinalResp) - sum(FinalResp), mean = mu.z[FinalResp == 0], 
                                        sd = 1, a = -Inf, b = 0)
        z[FinalResp == 1] <- rtruncnorm(sum(FinalResp), mean = mu.z[FinalResp == 1], 
                                        sd = 1, a = 0, b = Inf)
        
        # posterior mean vector of regression coefficients for each phase
        # based on formula A3 in JSSM 2018 paper
        mu.full <- sigma.full %*% (solve(sigma) %*% mu + crossprod(X, z))
        
        # sample regression coefficients from multivariate normal posterior distribution
        # based on formula A1 in JSSM 2018 paper
        beta <- as.numeric(rmvnorm(1, mu.full, sigma.full))
        
        # posterior mean vector of latent variable for each phase
        mu.z <- X %*% beta
        
        if (i > n.burn) {
          # compute response propensities per unit
          rho <- pnorm(mu.z)
          
          samples[[j]][[c]][i - n.burn, 1:4] <- beta
          # compute RR and CV
          samples[[j]][[c]][i - n.burn, "RR"] <- mean(rho)
          samples[[j]][[c]][i - n.burn, "CV"] <- sd(rho)/mean(rho)
        }
      }
    }
  }
  # return results
  return(samples)
}
