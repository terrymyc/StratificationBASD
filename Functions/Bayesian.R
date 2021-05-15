
####################################################
#
# Functions for Bayesian Analysis
#
# Updated on 10/05/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This code file defines 8 functions:
# 1. Summary statistics of response propensities models
# 2. Summary statistics of total costs models
# 3. Trace plot, density plot, autocorrelation plot
# 4. Deviance information criteria (DIC)
# 5. Estimation of response propensities per stratum and strategy
# 6. Optimization based on posterior distribution of quality and cost indicators
# 7. Summary statistics of stratification-assessed propensities models
# 8. Trace plot of stratification-assessed propensities models

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ------------------------------------------------------
# 1. Summary statistics of response propensities models
# ------------------------------------------------------
summaryPropensities <- function(samples){
  # create an empty matrix to store summarized results
  #statistics <- matrix(NA, nrow = 6, ncol = 4, 
  #                     dimnames = list(c("(Intercept)", "Health.prob", "Smoke.prob", "Obese.prob", "RR", "CV"), 
  #                                     c("Mean", "SD", "Naive SE", "Time-series SE")))
  # create an empty matrix to store summarized results
  statistics <- matrix(NA, nrow = ncol(samples[[1]]), ncol = 4)
  colnames(statistics) <- c("Mean", "SD", "Naive SE", "Time-series SE")
  if (nrow(statistics) == 8) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "RR", "CV")
  } else if (nrow(statistics) == 9) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "RR", "CV")
  } else if (nrow(statistics) == 10) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "RR", "CV")
  } else if (nrow(statistics) == 11) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "RR", "CV")
  } else if (nrow(statistics) == 12) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "RR", "CV")
  } else {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "RR", "CV")
  }
  # merge samples from multiple chains
  samples_merge <- do.call(rbind, samples)
  # compute summarized results
  statistics[, 1] <- round(apply(samples_merge, 2, mean), digits = 5)  ## mean
  statistics[, 2] <- round(sqrt(apply(samples_merge, 2, var)), digits = 6)  ## SD
  statistics[, 3] <- round(sqrt(apply(samples_merge, 2, var)/(length(samples)*nrow(samples[[1]]))), digits = 7)  ## naive SE
  ## time-series SE
  autoreg <- vector(mode = "list", length = length(samples))
  var_ts <- matrix(NA, nrow = nrow(statistics), ncol = length(samples))
  for (i in 1:length(samples)) {
    ### fit autoregression models
    autoreg[[i]] <- apply(samples[[i]], 2, ar)
    ### calculate time-series variance for each parameter
    var_ts[1, i] <- autoreg[[i]]$`(Intercept)`$var.pred/(1 - sum(autoreg[[i]]$`(Intercept)`$ar))^2
    var_ts[2, i] <- autoreg[[i]]$strata2$var.pred/(1 - sum(autoreg[[i]]$strata2$ar))^2
    var_ts[3, i] <- autoreg[[i]]$strata3$var.pred/(1 - sum(autoreg[[i]]$strata3$ar))^2
    var_ts[4, i] <- autoreg[[i]]$strata4$var.pred/(1 - sum(autoreg[[i]]$strata4$ar))^2
    var_ts[5, i] <- autoreg[[i]]$strata5$var.pred/(1 - sum(autoreg[[i]]$strata5$ar))^2
    var_ts[6, i] <- autoreg[[i]]$strata6$var.pred/(1 - sum(autoreg[[i]]$strata6$ar))^2
    if (nrow(statistics) == 8) {
      var_ts[7, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[8, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    } else if (nrow(statistics) == 9) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[9, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    } else if (nrow(statistics) == 10) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[10, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    } else if (nrow(statistics) == 11) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[11, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    } else if (nrow(statistics) == 12) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$strata10$var.pred/(1 - sum(autoreg[[i]]$strata10$ar))^2
      var_ts[11, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[12, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    } else {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$strata10$var.pred/(1 - sum(autoreg[[i]]$strata10$ar))^2
      var_ts[11, i] <- autoreg[[i]]$strata11$var.pred/(1 - sum(autoreg[[i]]$strata11$ar))^2
      var_ts[12, i] <- autoreg[[i]]$RR$var.pred/(1 - sum(autoreg[[i]]$RR$ar))^2
      var_ts[13, i] <- autoreg[[i]]$CV$var.pred/(1 - sum(autoreg[[i]]$CV$ar))^2
    }
  }
  ### compute and store time-series SE
  statistics[, 4] <- round(sqrt(apply(var_ts, 1, mean)/(length(samples)*nrow(samples[[1]]))), digits = 7)
  
  # compute quantiles for each parameter
  quantiles <- round(t(apply(samples_merge, 2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))), digits = 5)
  # return results
  summarylist <- list(statistics = statistics, quantiles = quantiles)
  return(summarylist)
}


# ------------------------------------------------------
# 2. Summary statistics of total costs models
# ------------------------------------------------------
summaryCost <- function(samples){
  # create an empty matrix to store summarized results
  statistics <- matrix(NA, nrow = ncol(samples[[1]]), ncol = 4)
  colnames(statistics) <- c("Mean", "SD", "Naive SE", "Time-series SE")
  if (nrow(statistics) == 7) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "variance")
  } else if (nrow(statistics) == 8) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "variance")
  } else if (nrow(statistics) == 9) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "variance")
  } else if (nrow(statistics) == 10) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "variance")
  } else if (nrow(statistics) == 11) {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "variance")
  } else {
    rownames(statistics) <- c("(Intercept)", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "variance")
  }
  # merge samples from multiple chains
  samples_merge <- do.call(rbind, samples)
  # compute summarized results
  statistics[, 1] <- round(apply(samples_merge, 2, mean), digits = 5)  ## mean
  statistics[, 2] <- round(sqrt(apply(samples_merge, 2, var)), digits = 6)  ## SD
  statistics[, 3] <- round(sqrt(apply(samples_merge, 2, var)/(length(samples)*nrow(samples[[1]]))), digits = 7)  ## naive SE
  ## time-series SE
  autoreg <- vector(mode = "list", length = length(samples))
  var_ts <- matrix(NA, nrow = nrow(statistics), ncol = length(samples))
  for (i in 1:length(samples)) {
    ### fit autoregression models
    autoreg[[i]] <- apply(samples[[i]], 2, ar)
    ### calculate time-series variance for each parameter
    var_ts[1, i] <- autoreg[[i]]$`(Intercept)`$var.pred/(1 - sum(autoreg[[i]]$`(Intercept)`$ar))^2
    var_ts[2, i] <- autoreg[[i]]$strata2$var.pred/(1 - sum(autoreg[[i]]$strata2$ar))^2
    var_ts[3, i] <- autoreg[[i]]$strata3$var.pred/(1 - sum(autoreg[[i]]$strata3$ar))^2
    var_ts[4, i] <- autoreg[[i]]$strata4$var.pred/(1 - sum(autoreg[[i]]$strata4$ar))^2
    var_ts[5, i] <- autoreg[[i]]$strata5$var.pred/(1 - sum(autoreg[[i]]$strata5$ar))^2
    var_ts[6, i] <- autoreg[[i]]$strata6$var.pred/(1 - sum(autoreg[[i]]$strata6$ar))^2
    if (nrow(statistics) == 7) {
      var_ts[7, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    } else if (nrow(statistics) == 8) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    } else if (nrow(statistics) == 9) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    } else if (nrow(statistics) == 10) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    } else if (nrow(statistics) == 11) {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$strata10$var.pred/(1 - sum(autoreg[[i]]$strata10$ar))^2
      var_ts[11, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    } else {
      var_ts[7, i] <- autoreg[[i]]$strata7$var.pred/(1 - sum(autoreg[[i]]$strata7$ar))^2
      var_ts[8, i] <- autoreg[[i]]$strata8$var.pred/(1 - sum(autoreg[[i]]$strata8$ar))^2
      var_ts[9, i] <- autoreg[[i]]$strata9$var.pred/(1 - sum(autoreg[[i]]$strata9$ar))^2
      var_ts[10, i] <- autoreg[[i]]$strata10$var.pred/(1 - sum(autoreg[[i]]$strata10$ar))^2
      var_ts[11, i] <- autoreg[[i]]$strata11$var.pred/(1 - sum(autoreg[[i]]$strata11$ar))^2
      var_ts[12, i] <- autoreg[[i]]$variance$var.pred/(1 - sum(autoreg[[i]]$variance$ar))^2
    }
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
  for (i in 1:ncol(samples[[chain1]])) {
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
    
    if (i < ncol(samples[[chain1]])) {par(ask = T)} else {par(ask = F)}
  }
}


# --------------------------------------
# 4. Deviance information criteria (DIC)
# --------------------------------------

DIC <- function(samples, X, y){
  # log likelihood evaluated at the posterior mean of parameters
  logL.hat <- sum(dbinom(y, size = 1, prob = pnorm(X %*% colMeans(samples)), log = T), na.rm = T)
  dhat <- -2*logL.hat
  # average log likelihood over the posterior distribution of parameters
  logL.bar <- rep(0, nrow(samples))
  for (t in 1:nrow(samples)) {
    logL.bar[t] <- sum(dbinom(y, size = 1, prob = pnorm(X %*% samples[t, ]), log = T), na.rm = T)
  }
  dbar <- -2*mean(logL.bar)
  # estimate of effective number of parameters/model complexity
  n.parameter <- dbar - dhat
  return(list(n.parameter = n.parameter, DIC = dhat + 2*n.parameter)) 
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
  statistics <- matrix(NA, nrow = 3, ncol = length(unique(data$strata)))
  rownames(statistics) <- c("Web", "Web -> F2F-short", "Web -> F2F-extended")
  if (ncol(statistics) == 6) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6")
  } else if (ncol(statistics) == 7) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7")
  } else if (ncol(statistics) == 8) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8")
  } else if (ncol(statistics) == 9) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9")
  } else if (ncol(statistics) == 10) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10")
  } else {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11")
  }
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
summaryIndicator <- function(allocation, CV, RR, B, B.per, RR.restraint, B.restraint) {
  if (ncol(allocation) == 6) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", 
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  } else if (ncol(allocation) == 7) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", 
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  } else if (ncol(allocation) == 8) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", 
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  } else if (ncol(allocation) == 9) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", 
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  } else if (ncol(allocation) == 10) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10",
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  } else {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11",
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.restraint", "B.restraint")
  }
  
  # compute summarized results
  statistics[, "CV"] <- round(apply(CV[[1]], 2, mean), digits = 5)  ## mean
  #statistics[, "CV.sd"] <- round(sqrt(apply(CV[[1]], 2, var)), digits = 6)  ## SD
  statistics[, "RR"] <- round(apply(RR[[1]], 2, mean), digits = 5)  ## mean
  #statistics[, "RR.sd"] <- round(sqrt(apply(RR[[1]], 2, var)), digits = 6)  ## SD
  statistics[, "B"] <- round(apply(B[[1]], 2, mean), digits = 5)  ## mean
  #statistics[, "B.sd"] <- round(sqrt(apply(B[[1]], 2, var)), digits = 6)  ## SD
  
  for (i in 1:nrow(allocation)) {
    statistics[i, "RR.restraint"] <- sum(RR[[1]][, i] >= RR.restraint)/nrow(RR[[1]])
    statistics[i, "B.restraint"] <- sum(B[[1]][, i] > B.restraint)/nrow(B[[1]])
  }
  
  # compute quantiles
  statistics[, c("CV.025", "CV.975")] <- round(t(apply(CV[[1]], 2, quantile,
                                                       probs = c(0.025, 0.975))),
                                               digits = 5)
  statistics[, c("RR.025", "RR.975")] <- round(t(apply(RR[[1]], 2, quantile,
                                                       probs = c(0.025, 0.975))), 
                                               digits = 5)
  statistics[, c("B.025", "B.975")] <- round(t(apply(B[[1]], 2, quantile,
                                                     probs = c(0.025, 0.975))),
                                             digits = 5)
  
  # return results
  summarylist <- list(statistics = statistics)
  return(summarylist)
}


# ------------------------------------------------------
# 7. Summary statistics of stratification-assessed propensities models
# ------------------------------------------------------
summaryOptimalDesigns <- function(samples){
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
  var_ts <- matrix(NA, nrow = nrow(statistics), ncol = length(samples))
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


# ------------------------------------------------------
# 8. Trace plot of stratification-assessed propensities models
# ------------------------------------------------------

TracePlotCrit <- function(samples, chain1 = 1, chain2 = 2){
  layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = T))
  for (i in 1:length(samples)) {
    plot(samples[[i]][[1]][, 1], type = "l", col = "coral",
         xlab = "Gibbs iterations", ylab = "Slope parameters",
         ylim = c(-1, 1.9),
         main = paste("Design", i))
    lines(samples[[i]][[2]][, 1], type = "l", col = "deepskyblue")
    lines(samples[[i]][[1]][, 2], type = "l", col = "coral")
    lines(samples[[i]][[2]][, 2], type = "l", col = "deepskyblue")
    lines(samples[[i]][[1]][, 3], type = "l", col = "coral")
    lines(samples[[i]][[2]][, 3], type = "l", col = "deepskyblue")
    lines(samples[[i]][[1]][, 4], type = "l", col = "coral")
    lines(samples[[i]][[2]][, 4], type = "l", col = "deepskyblue")
  }
}
