
####################################################
#
# Functions for Bayesian Analysis
#
# Updated on 29/07/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This code file defines 3 functions:
# 0. Deviance information criteria (DIC)
# 1. Summary estimated response propensities per stratum and strategy
# 2. Summary posterior distributions of quality and cost indicators

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------------
# 0. Deviance information criteria (DIC)
# --------------------------------------------------------------

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
# 1. Summary estimated of response propensities per stratum and strategy
# ----------------------------------------------------------------

RespPropensity <- function(data, strata, prop.g.s1, prop.g.s2, prop.g.s3){
  # strata size per strategy
  strata.size <- rbind(table(data[!is.na(data$ResponsePhase1), strata]),
                       table(data[!is.na(data$ResponsePhase2), strata]),
                       table(data[!is.na(data$ResponsePhase3), strata]))
  
  # create an empty matrix to store summarized results
  statistics <- matrix(NA, nrow = 3, ncol = length(unique(data[, strata])))
  rownames(statistics) <- c("Web", "Web -> F2F-short", "Web -> F2F-extended")
  if (ncol(statistics) == 5) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5")
  } else if (ncol(statistics) == 6) {
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
  } else if (ncol(statistics) == 11) {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11")
  } else {
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5", 
                              "strata6", "strata7", "strata8", "strata9", "strata10", "strata11", "strata12")
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
# 2. Summary posterior distributions of quality and cost indicators
# -------------------------------------------------------------------------------
summaryIndicator <- function(allocation, CV, RR, B, RR.constraint, B.constraint) {
  if (ncol(allocation) == 5) {
    statistics <- cbind(allocation, rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)), 
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)), rep(NA, nrow(allocation)),
                        rep(NA, nrow(allocation)))
    colnames(statistics) <- c("strata1", "strata2", "strata3", "strata4", "strata5",
                              "CV", "CV.025", "CV.975",
                              "RR", "RR.025", "RR.975",
                              "B", "B.025", "B.975",
                              "RR.constraint", "B.constraint")
  } else if (ncol(allocation) == 6) {
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
                              "RR.constraint", "B.constraint")
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
                              "RR.constraint", "B.constraint")
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
                              "RR.constraint", "B.constraint")
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
                              "RR.constraint", "B.constraint")
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
                              "RR.constraint", "B.constraint")
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
                              "RR.constraint", "B.constraint")
  }
  
  # compute summarized results
  statistics[, "CV"] <- round(apply(CV, 2, mean), digits = 5)  ## mean
  #statistics[, "CV.sd"] <- round(sqrt(apply(CV[[1]], 2, var)), digits = 6)  ## SD
  statistics[, "RR"] <- round(apply(RR, 2, mean), digits = 5)  ## mean
  #statistics[, "RR.sd"] <- round(sqrt(apply(RR[[1]], 2, var)), digits = 6)  ## SD
  statistics[, "B"] <- round(apply(B, 2, mean), digits = 5)  ## mean
  #statistics[, "B.sd"] <- round(sqrt(apply(B[[1]], 2, var)), digits = 6)  ## SD
  
  for (i in 1:nrow(allocation)) {
    statistics[i, "RR.constraint"] <- sum(RR[, i] >= RR.constraint)/nrow(RR)
    statistics[i, "B.constraint"] <- sum(B[, i] > B.constraint)/nrow(B)
  }
  
  # compute quantiles
  statistics[, c("CV.025", "CV.975")] <- round(t(apply(CV, 2, quantile,
                                                       probs = c(0.025, 0.975))),
                                               digits = 5)
  statistics[, c("RR.025", "RR.975")] <- round(t(apply(RR, 2, quantile,
                                                       probs = c(0.025, 0.975))), 
                                               digits = 5)
  statistics[, c("B.025", "B.975")] <- round(t(apply(B, 2, quantile,
                                                     probs = c(0.025, 0.975))),
                                             digits = 5)
  
  # return results
  summarylist <- list(statistics = statistics)
  return(summarylist)
}
