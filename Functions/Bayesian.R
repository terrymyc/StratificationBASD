
####################################################
#
# Functions for Bayesian Analysis
#
# Updated on 20/01/2021
#
####################################################

# This code file defines 6 functions:
# 1. Gibbs Sampler
# 2. Summary statistics of the samples
# 3. Trace plot, density plot, autocorrelation plot
# 4. Gelman-Rubin diagnostic
# 5. Estimation of response propensities per stratum and strategy
# 6. Optimization based on posterior distribution of quality and cost indicators


library(mvtnorm)  ## sampling from multivariate normal distribution
library(truncnorm)  ## sampling from truncated normal distribution


# --------------------------------------
# 1. Gibbs Sampler
# --------------------------------------
GibbsTerry <- function(response, covariates, n.covariates, strata, allocation,
                       n.iter, n.burn, n.chain, inits.s1, inits.s2, inits.s3,
                       # prior specification
                       mu = rep(0, 1 + n.covariates),
                       sigma = diag(1000, 1 + n.covariates)) {
  # response: a 11565*3 matrix of response variables for three phases
  # covariates: a 11565*3 matrix of three predicted survey variables
  # n.covariates: number of covariates
  # strata: a 11565*1 vector indicates the corresponding stratum for each unit
  # allocation: a 81*6 matrix of solutions that allocate strategies to 6 strata
  # n.iter: number of iterations
  # n.chain: number of chains
  # inits.s1: initial values of parameters for modeling phase 1 response
  # inits.s2: initial values of parameters for modeling phase 2 response
  # inits.s3: initial values of parameters for modeling phase 3 response
  # mu: hyperparameter of prior distributions, non-informative by default
  # sigma: hyperparameter of prior distributions, non-informative by default
  
  
  # vector of response for each phase
  y.s1 <- as.matrix(c(na.omit(response[, 1])))  # all 11565 units reach phase 1
  y.s2 <- as.matrix(c(na.omit(response[, 2])))  # only contain 7153 units reaching phase 2
  y.s3 <- as.matrix(c(na.omit(response[, 3])))  # only contain 1438 units reaching phase 3
  
  
  # matrix of covariates for each phase
  # consistent length with response for each phase
  # insert an additional column vector of 1 for intercept
  X.s1 <- cbind(rep(1, length(y.s1)), as.matrix(covariates[!is.na(response[, 1]), ]))  # 11565*4
  X.s2 <- cbind(rep(1, length(y.s2)), as.matrix(covariates[!is.na(response[, 2]), ]))  # 7153*4
  X.s3 <- cbind(rep(1, length(y.s3)), as.matrix(covariates[!is.na(response[, 3]), ]))  # 1438*4
  
  
  # vector of strata for each phase
  # indicate which stratum a unit is in
  strata.s1 <- as.matrix(strata[!is.na(response[, 1]), ])  # 11565*1
  strata.s2 <- as.matrix(strata[!is.na(response[, 2]), ])  # 7153*1
  strata.s3 <- as.matrix(strata[!is.na(response[, 3]), ])  # 1438*1
  
  # posterior covariance matrix of regression coefficients for each phase
  # based on formula A2 in JSSM 2018 paper
  sigma.full.s1 <- solve(solve(sigma) + crossprod(X.s1, X.s1))
  sigma.full.s2 <- solve(solve(sigma) + crossprod(X.s2, X.s2))
  sigma.full.s3 <- solve(solve(sigma) + crossprod(X.s3, X.s3))
  
  # create empty lists to store different chains
  # Gibbs samples for estimating regression parameters in each phase
  samples.s1 <- vector(mode = "list", length = n.chain)
  samples.s2 <- vector(mode = "list", length = n.chain)
  samples.s3 <- vector(mode = "list", length = n.chain)
  # Gibbs samples for estimating response propensities per stratum and strategy
  # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
  prop.g.s1 <- vector(mode = "list", length = n.chain)  # web
  prop.g.s2 <- vector(mode = "list", length = n.chain)  # web -> f2f-short
  prop.g.s3 <- vector(mode = "list", length = n.chain)  # web -> f2f-short -> f2f-extended
  # based on Gibbs samples of response propensities, calculating the mean and CV
  response.rate <- vector(mode = "list", length = n.chain)
  cv.response <- vector(mode = "list", length = n.chain)
  

  for (c in 1:n.chain) {
    
    # -----------------------------
    # Preparation before start sampling
    # -----------------------------
    
    # supply the initial values of regression parameters
    beta.s1 <- inits.s1[[c]]
    beta.s2 <- inits.s2[[c]]
    beta.s3 <- inits.s3[[c]]
    
    # create an empty matrix to store estimated regression parameters and RR & CV IN each phase
    res.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + n.covariates)
    res.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + n.covariates)
    res.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = 3 + n.covariates)
    colnames(res.s1) <- c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV")
    colnames(res.s2) <- c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV")
    colnames(res.s3) <- c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV")
    
    # create an empty vector to store sampled latent variable for each phase
    # updated in every iteration
    z.s1 <- rep(NA, length(y.s1))
    z.s2 <- rep(NA, length(y.s2))
    z.s3 <- rep(NA, length(y.s3))
    
    # create an empty vector to store sampled response propensities for each phase
    # updated in every iteration
    p.s1 <- rep(NA, length(y.s1))
    p.s2 <- rep(NA, length(y.s2))
    p.s3 <- rep(NA, length(y.s3))
    
    # create an empty matrix to store sampled response propensities per stratum and strategy
    # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
    # at the end of sampling per chain, these matrices will be assigned to the empty lists created before sampling starts
    p.g.s1 <- matrix(NA, nrow = n.iter + n.burn, ncol = 6)  # web
    p.g.s2 <- matrix(NA, nrow = n.iter + n.burn, ncol = 6)  # web -> f2f-short
    p.g.s3 <- matrix(NA, nrow = n.iter + n.burn, ncol = 6)  # web -> f2f-short -> f2f-extended
    
    # ---------------------------------------------
    # Additional preparation for optimization
    # ---------------------------------------------
    
    # create an empty matrix to store sampled response propensities per stratum and strategy
    # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
    # updated in every iteration
    p.g.s <- matrix(NA, nrow = 3, ncol = 6)
    
    # create an empty matrix to store RR and CV for each solution
    RR <- matrix(NA, nrow = n.iter, ncol = nrow(allocation))
    CV <- matrix(NA, nrow = n.iter, ncol = nrow(allocation))
    
    
    # -----------------------------
    # START SAMPLING
    # -----------------------------
    for (i in 1:(n.iter + n.burn)) {
      
      # posterior mean vector of latent variable for each phase
      mu.z.s1 <- X.s1 %*% beta.s1
      mu.z.s2 <- X.s2 %*% beta.s2
      mu.z.s3 <- X.s3 %*% beta.s3
      
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
      
      # subsets below facilitate computation of response propensities per stratum and strategy
      # phase 1 response propensities under strategy s_{1,2} (only contain units reaching phase 2)
      p.s1.s2 <- p.s1[!is.na(response[, 2]), ]
      # phase 1 response propensities under strategy s_{1,3} (only contain units reaching phase 3)
      p.s1.s3 <- p.s1[!is.na(response[, 3]), ]
      # phase 2 response propensities under strategy s_{1,3} (only contain units reaching phase 3)
      p.s2.s3 <- p.s2[!is.na(response[!is.na(response$ResponsePhase2),][, 3]), ]
      
      # compute response propensities per stratum and strategy
      # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
      # based on formula 1 & 2 in my report
      for (s in 1:6) {
        # s indicates stratum
        # web
        p.g.s1[i, s] <- mean(p.s1[strata.s1 == s])
        # web -> f2f-short
        p.g.s2[i, s] <- mean(p.s1.s2[strata.s2 == s] + (1 - p.s1.s2[strata.s2 == s]) * p.s2[strata.s2 == s])
        # web -> f2f-short -> f2f-extended
        p.g.s3[i, s] <- mean(p.s1.s3[strata.s3 == s] + (1 - p.s1.s3[strata.s3 == s]) * p.s2.s3[strata.s3 == s] + ((1 - p.s1.s3[strata.s3 == s]) * (1 - p.s2.s3[strata.s3 == s])) * p.s3[strata.s3 == s])
      }
      
      # -------------------------------------------
      # UPDATE POSTERIORS except for last iteration
      # -------------------------------------------
      
      if (i < (n.iter + n.burn)) {
        
        # posterior mean vector of regression coefficients for each phase
        # based on formula A3 in JSSM 2018 paper
        mu.full.s1 <- sigma.full.s1 %*% (solve(sigma) %*% mu + crossprod(X.s1, z.s1))
        mu.full.s2 <- sigma.full.s2 %*% (solve(sigma) %*% mu + crossprod(X.s2, z.s2))
        mu.full.s3 <- sigma.full.s3 %*% (solve(sigma) %*% mu + crossprod(X.s3, z.s3))
        
        # sample regression coefficients from multivariate normal posterior distribution
        # based on formula A1 in JSSM 2018 paper
        beta.s1 <- as.numeric(rmvnorm(1, mu.full.s1, sigma.full.s1))
        beta.s2 <- as.numeric(rmvnorm(1, mu.full.s2, sigma.full.s2))
        beta.s3 <- as.numeric(rmvnorm(1, mu.full.s3, sigma.full.s3))
        
        # store sampled regression coefficients
        res.s1[i+1, 1:(1 + n.covariates)] <- beta.s1
        res.s2[i+1, 1:(1 + n.covariates)] <- beta.s2
        res.s3[i+1, 1:(1 + n.covariates)] <- beta.s3
      }
      
      
      # -----------------------------------------------------------------------
      # OPTIMIZATION based on response propensities computed in each iteration after burn-in
      # -----------------------------------------------------------------------
      #if (i > n.burn) {
      if (i > (n.burn + n.iter)) {
        
        # tabulate response propensities per stratum and strategy
        # **Note: NOT response propensities IN each phase but from phase 1 THROUGH phase t**
        # Table 3 in my report shows an average result
        p.g.s[1, ] <- p.g.s1[i, ]  # web
        p.g.s[2, ] <- p.g.s2[i, ]  # web -> f2f-short
        p.g.s[3, ] <- p.g.s3[i, ]  # web -> f2f-short -> f2f-extended
        
        for (j in 1:nrow(allocation)) {
          
          # sample size per stratum for each phase
          n.g <- rbind(as.numeric(table(strata[!is.na(response[, 1]), ])),
                       as.numeric(table(strata[!is.na(response[, 2]), ])),
                       as.numeric(table(strata[!is.na(response[, 3]), ])))
          
          # choose allocation of strategies to strata
          strategy <- unname(allocation[j,])
          
          # sample size per stratum under chosen allocation
          n.g1 <- n.g[strategy[1], 1]
          n.g2 <- n.g[strategy[2], 2]
          n.g3 <- n.g[strategy[3], 3]
          n.g4 <- n.g[strategy[4], 4]
          n.g5 <- n.g[strategy[5], 5]
          n.g6 <- n.g[strategy[6], 6]
          
          # sample size in total of the chosen allocation
          n.sum <- sum(n.g1 + n.g2 + n.g3 + n.g4 + n.g5 + n.g6)
          
          # retrieve response propensities per stratum and strategy under chosen allocation
          rho.g1 <- p.g.s[strategy[1], 1]
          rho.g2 <- p.g.s[strategy[2], 2]
          rho.g3 <- p.g.s[strategy[3], 3]
          rho.g4 <- p.g.s[strategy[4], 4]
          rho.g4 <- p.g.s[strategy[5], 5]
          rho.g4 <- p.g.s[strategy[6], 6]
          
          # overall response propensity and variance under chosen allocation
          rho <- (n.g1 * rho.g1 + n.g2 * rho.g2 + n.g3 * rho.g3 + n.g4 * rho.g4 + n.g5 * rho.g5 + n.g6 * rho.g6)/n.sum
          var.rho <- (n.g1 * (rho.g1 - rho)^2 + n.g2 * (rho.g2 - rho)^2 + n.g3 * (rho.g3 - rho)^2 + 
                        n.g4 * (rho.g4 - rho)^2 + n.g5 * (rho.g5 - rho)^2) + n.g6 * (rho.g6 - rho)^2/n.sum
          
          # store RR and CV of chosen allocation
          RR[i - n.burn, j] <- rho
          CV[i - n.burn, j] <- sqrt(var.rho)/rho
        }
      }
    }
    
    # burn-in period
    res.s1 <- res.s1[-(1:n.burn),]
    res.s2 <- res.s2[-(1:n.burn),]
    res.s3 <- res.s3[-(1:n.burn),]
    p.g.s1 <- p.g.s1[-(1:n.burn),]
    p.g.s2 <- p.g.s2[-(1:n.burn),]
    p.g.s3 <- p.g.s3[-(1:n.burn),]
    
    # posterior samples of regression parameters and RR & CV IN each phase
    samples.s1[[c]] <- res.s1
    samples.s2[[c]] <- res.s2
    samples.s3[[c]] <- res.s3
    
    # posterior samples of response propensities per stratum and strategy
    prop.g.s1[[c]] <- p.g.s1  # web
    prop.g.s2[[c]] <- p.g.s2  # web -> f2f-short
    prop.g.s3[[c]] <- p.g.s3  # web -> f2f-short -> f2f-extended
    
    # posterior samples of overall RR & CV for each allocation
    response.rate[[c]] <- RR
    cv.response[[c]] <- CV
  }
  
  # return results
  final.list <- list(samples.s1 = samples.s1, samples.s2 = samples.s2, samples.s3 = samples.s3,
                     prop.g.s1 = prop.g.s1, prop.g.s2 = prop.g.s2, prop.g.s3 = prop.g.s3,
                     response.rate = response.rate, cv.response = cv.response)
  return(final.list)
}


# ----------------------------------------
# 2. Summary statistics of the samples
# ----------------------------------------
summaryGibbs <- function(samples){
  # create an empty matrix to store summarized results
  statistics <- matrix(NA, nrow = 6, ncol = 4, 
                       dimnames = list(c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV"), 
                                       c("Mean", "SD", "Naive SE", "Time-series SE")))
  # merge samples from multiple chains
  samples_merge <- do.call(rbind, samples)
  # compute summarized results
  statistics[, 1] <- round(apply(samples_merge, 2, mean), digits = 5)  ## mean
  statistics[, 2] <- round(sqrt(apply(samples_merge, 2, var)), digits = 6)  ## SD
  statistics[, 3] <- round(sqrt(apply(samples_merge, 2, var)/(length(samples)*nrow(samples[[1]]))), digits = 7)  ## naive SE
  ## time-series SE
  autoreg <- vector(mode = "list", length = length(samples))
  var_ts <- matrix(NA, nrow = 6, ncol = length(samples))
  for (i in 1:length(samples)) {
    ### fit autoregression models
    autoreg[[i]] <- apply(samples[[i]], 2, ar)
    ### calculate time-series variance for each parameter
    var_ts[1, i] <- autoreg[[i]]$`(Intercept)`$var.pred/(1 - sum(autoreg[[i]]$`(Intercept)`$ar))^2
    var_ts[2, i] <- autoreg[[i]]$Health.prob$var.pred/(1 - sum(autoreg[[i]]$Health.prob$ar))^2
    var_ts[3, i] <- autoreg[[i]]$Smoke.prob$var.pred/(1 - sum(autoreg[[i]]$Smoke.prob$ar))^2
    var_ts[4, i] <- autoreg[[i]]$Obese.prob$var.pred/(1 - sum(autoreg[[i]]$Obese.prob$ar))^2
    var_ts[5, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
    var_ts[6, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
  }
  ### compute and store time-series SE
  statistics[, 4] <- round(sqrt(apply(var_ts, 1, mean)/(length(samples)*nrow(samples[[1]]))), digits = 7)
  
  # compute quantiles for each parameter
  quantiles <- round(t(apply(samples_merge, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  # return results
  summarylist <- list(statistics = statistics, quantiles = quantiles)
  return(summarylist)
}


# ---------------------------------------------------
# 3. Trace plot, density plot, autocorrelation plot
# ---------------------------------------------------
plotSamples <- function(samples, chain1 = 1, chain2 = 2){
  par(mfrow = c(2, 2))
  for (i in 1:6) {
    # trace plot
    plot(samples[[chain1]][, i], type = "l", col = "coral",
         xlab = "Iterations", ylab = "", main = paste("Trace of", colnames(samples[[chain1]])[i]))
    #lines(samples[[chain2]][, i], type = "l", col = "deepskyblue")
    # density plot
    plot(density(samples[[chain1]][, i]), col = "coral", lwd = 3,
         ylab = "", main = paste("Density of", colnames(samples[[1]])[i]))
    #lines(density(samples[[chain2]][, i]), col = "deepskyblue", lwd = 3)
    # autocorrelation plot
    acf(samples[[chain1]][, i], lag.max = 30, col = "coral", lwd = 3,
        ylab = "Autocorrelation", main = paste("Autocorrelation of", colnames(samples[[chain1]])[i], "from Chain 1"))
    #acf(samples[[chain2]][, i], lag.max = 30, col = "deepskyblue", lwd = 3,
    #    ylab = "Autocorrelation", main = paste("Autocorrelation of", colnames(samples[[chain1]])[i], "from Chain 2"))
    
    if (i < 6) {par(ask = T)} else {par(ask = F)}
  }
}


# ------------------------------------------
# 4. Gelman-Rubin diagnostic 
# ------------------------------------------
plotGelman <- function(samples, n.iter = 500) {
  par(mfrow = c(3, 2))
  # create empty vectors to store total/between/within chain variances
  VarWithinChain <- rep(NA, 1)
  VarBetweenChain <- rep(NA, 1)
  VarTotal <- rep(NA, 1)
  # create empty matrix to store scale reduction factors
  Rhat <- matrix(NA, nrow = n.iter-1, ncol = 6)
  # plot parameter by parameter
  for (par in 1:6) {
    for (i in 2:n.iter) {
      # merge chains
      samples_merged <- vector()
      for (c in 1:length(samples)) {
        samples_merged <- append(samples_merged, samples[[c]][1:i, par])
      }
      # compute squared residuals required in next step
      sqrWithinResid <- rep(NA, length(samples))
      sqrBetweenResid <- rep(NA, length(samples))
      for (c in 1:length(samples)) {
        sqrWithinResid[c] <- sum((samples[[c]][1:i, par] - mean(samples[[c]][1:i, par]))^2)
        sqrBetweenResid[c] <- (mean(samples[[c]][1:i, par]) - mean(samples_merged))^2
      }
      # compute total/between/within chain variances
      VarWithinChain <- (1/(length(samples)*(i - 1)))*sum(sqrWithinResid)
      VarBetweenChain <- i/(length(samples) - 1)*sum(sqrBetweenResid)
      VarTotal <- (i - 1)/i*VarWithinChain + (1/i)*VarBetweenChain
      # compute scale reduction factor
      Rhat[i - 1, par] <- sqrt(VarTotal/VarWithinChain)
    }
    plot(Rhat[, par], type = "l", xlim = c(0, n.iter),
         xlab = "Iterations", ylab = expression(hat(R)),
         main = paste("Trace of the scale reduction factor for", colnames(samples[[1]])[par]))
    abline(h = 1, col = "red")
  }
}


# ----------------------------------------------------------------
# 5. Estimation of response propensities per stratum and strategy
# ----------------------------------------------------------------
RespPropensity <- function(data, prop.g.s1, prop.g.s2, prop.g.s3){
  # strata size per strategy
  strata.size <- rbind(table(data[!is.na(data$ResponsePhase1), "strata"]),
                       table(data[!is.na(data$ResponsePhase2), "strata"]),
                       table(data[!is.na(data$ResponsePhase3), "strata"]))
  
  # create an empty matrix to store summarized results
  statistics <- matrix(NA, nrow = 3, ncol = 6, 
                       dimnames = list(c("Web", "Web -> F2F-short", "Web -> F2F-extended"), 
                                       c("1", "2", "3", "4", "5", "6")))
  # compute summarized results
  statistics[1, ] <- round(apply(prop.g.s1, 2, mean), digits = 5)  ## Web
  statistics[2, ] <- round(apply(prop.g.s2, 2, mean), digits = 5)  ## Web -> F2F-short
  statistics[3, ] <- round(apply(prop.g.s3, 2, mean), digits = 5)  ## Web -> F2F-extended
  
  # compute quantiles for each phase
  quantiles.s1 <- round(t(apply(prop.g.s1, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  quantiles.s2 <- round(t(apply(prop.g.s2, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  quantiles.s3 <- round(t(apply(prop.g.s3, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  
  # return results
  summarylist <- list(strata.size = strata.size, statistics = statistics, quantiles.s1 = quantiles.s1, 
                      quantiles.s2 = quantiles.s2, quantiles.s3 = quantiles.s3)
  return(summarylist)
}


# -------------------------------------------------------------------------------
# 6. Optimization based on posterior distribution of quality and cost indicators
# -------------------------------------------------------------------------------
summaryIndicator <- function(CV, RR, allocation) {
  statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                      rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), rep(NA, nrow(allocation)))
  colnames(statistics) <- c("g1", "g2", "g3", "g4", "CV", "CV.sd", "RR", "RR.sd", "RR>=.6")
  
  # compute summarized results
  statistics[, "CV"] <- round(apply(CV[[1]], 2, mean), digits = 5)  ## mean
  statistics[, "CV.sd"] <- round(sqrt(apply(CV[[1]], 2, var)), digits = 6)  ## SD
  statistics[, "RR"] <- round(apply(RR[[1]], 2, mean), digits = 5)  ## mean
  statistics[, "RR.sd"] <- round(sqrt(apply(RR[[1]], 2, var)), digits = 6)  ## SD
  for (j in 1:nrow(allocation)) {
    statistics[j, "RR>=.6"] <- sum(RR[[1]][, j] >= 0.6)/nrow(RR[[1]])
  }
  
  # compute quantiles
  quantiles.CV <- round(t(apply(CV[[1]], 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  quantiles.RR <- round(t(apply(RR[[1]], 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  
  # return results
  summarylist <- list(statistics = statistics, quantiles.CV = quantiles.CV, quantiles.RR = quantiles.RR)
  return(summarylist)
}

