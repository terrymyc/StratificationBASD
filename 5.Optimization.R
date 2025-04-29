
######################################################
#
# 5. Optimization of adaptive survey designs
#
#    Updated on 29/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file optimizes the quality and cost indicators.

# This file assesses the optimal designs based on
# different stratification.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ----------------------------------------------------------
# 5.1 Optimize quality and cost indicators
# ----------------------------------------------------------

## generate all possible allocations of strategies
allocations.respY <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata_respY)))))
allocations.visitsY <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata_visitsY)))))
allocations.respX <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata_respX)))))
allocations.visitsX <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata_visitsX)))))
allocations.costX <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata_costX)))))

## posterior distribution of quality and cost indicators
n.iter <- 1000
n.burn <- 500

system.time(indicators.respY <- IndicatorsEst(data = data2017,
                                              strata = data2017$strata_respY,
                                              allocation = allocations.respY,
                                              n.iter = n.iter, n.burn = n.burn))
system.time(indicators.visitsY <- IndicatorsEst(data = data2017,
                                                strata = data2017$strata_visitsY,
                                                allocation = allocations.visitsY,
                                                n.iter = n.iter, n.burn = n.burn))
system.time(indicators.respX <- IndicatorsEst(data = data2017,
                                              strata = data2017$strata_respX,
                                              allocation = allocations.respX,
                                              n.iter = n.iter, n.burn = n.burn))
system.time(indicators.visitsX <- IndicatorsEst(data = data2017,
                                                strata = data2017$strata_visitsX,
                                                allocation = allocations.visitsX,
                                                n.iter = n.iter, n.burn = n.burn))
system.time(indicators.costX <- IndicatorsEst(data = data2017,
                                              strata = data2017$strata_costX,
                                              allocation = allocations.costX,
                                              n.iter = n.iter, n.burn = n.burn))

## set constraints for quality and cost indicators
realized.B <- sum(data2017$kostenTotaalCAWICAPI)/nrow(data2017)/mean(data2017$FinalRespPhase3)
RR.constraint <- 0.5
B.constraint <- realized.B

solutions.eva.respY <- summaryIndicator(allocation = allocations.respY, 
                                        CV = indicators.respY$CV,
                                        RR = indicators.respY$RR, RR.constraint = RR.constraint,
                                        B = indicators.respY$B, B.constraint = B.constraint)
solutions.eva.visitsY <- summaryIndicator(allocation = allocations.visitsY, 
                                          CV = indicators.visitsY$CV,
                                          RR = indicators.visitsY$RR, RR.constraint = RR.constraint,
                                          B = indicators.visitsY$B, B.constraint = B.constraint)
solutions.eva.respX <- summaryIndicator(allocation = allocations.respX,
                                        CV = indicators.respX$CV,
                                        RR = indicators.respX$RR, RR.constraint = RR.constraint,
                                        B = indicators.respX$B, B.constraint = B.constraint)
solutions.eva.visitsX <- summaryIndicator(allocation = allocations.visitsX, 
                                          CV = indicators.visitsX$CV,
                                          RR = indicators.visitsX$RR, RR.constraint = RR.constraint,
                                          B = indicators.visitsX$B, B.constraint = B.constraint)
solutions.eva.costX <- summaryIndicator(allocation = allocations.costX,
                                        CV = indicators.costX$CV,
                                        RR = indicators.costX$RR, RR.constraint = RR.constraint,
                                        B = indicators.costX$B, B.constraint = B.constraint)

## optimization based on RESPONSE RATES and COST per respondent
## filter solutions subject to constraints
optimized.respY <- solutions.eva.respY$statistics[solutions.eva.respY$statistics[, "RR"] >= RR.constraint &
                                                  solutions.eva.respY$statistics[, "B.constraint"] < 0.1, ]
optimized.visitsY <- solutions.eva.visitsY$statistics[solutions.eva.visitsY$statistics[, "RR"] >= RR.constraint &
                                                      solutions.eva.visitsY$statistics[, "B.constraint"] < 0.1, ]
optimized.respX <- solutions.eva.respX$statistics[solutions.eva.respX$statistics[, "RR"] >= RR.constraint &
                                                  solutions.eva.respX$statistics[, "B.constraint"] < 0.1, ]
optimized.visitsX <- solutions.eva.visitsX$statistics[solutions.eva.visitsX$statistics[, "RR"] >= RR.constraint &
                                                      solutions.eva.visitsX$statistics[, "B.constraint"] < 0.1, ]
optimized.costX <- solutions.eva.costX$statistics[solutions.eva.costX$statistics[, "RR"] >= RR.constraint &
                                                  solutions.eva.costX$statistics[, "B.constraint"] < 0.1, ]

## order solutions by CV
optimized.respY <- optimized.respY[order(optimized.respY[, "CV"]), ]
optimized.visitsY <- optimized.visitsY[order(optimized.visitsY[, "CV"]), ]
optimized.respX <- optimized.respX[order(optimized.respX[, "CV"]), ]
optimized.visitsX <- optimized.visitsX[order(optimized.visitsX[, "CV"]), ]
optimized.costX <- optimized.costX[order(optimized.costX[, "CV"]), ]

## extract top 5 optimal design solutions
optimal.respY <- unname(optimized.respY[1:5, 1:ncol(allocations.respY)])
optimal.visitsY <- unname(optimized.visitsY[1:5, 1:ncol(allocations.visitsY)])
optimal.respX <- unname(optimized.respX[1:5, 1:ncol(allocations.respX)])
optimal.visitsX <- unname(optimized.visitsX[1:5, 1:ncol(allocations.visitsX)])
optimal.costX <- unname(optimized.costX[1:5, 1:ncol(allocations.costX)])

## extract CV, RR, B of top 100 solutions for figure 3
optimal.respY.top100 <- data.frame(optimized.respY[1:100, c("CV", "CV.025", "CV.975", "RR", "B")])
optimal.visitsY.top100 <- data.frame(optimized.visitsY[1:100, c("CV", "CV.025", "CV.975", "RR", "B")])



# ----------------------------------------------------------
# 5.2 Assess optimal designs based on CV criterion
# ----------------------------------------------------------

## Evaluate posteriors of optimal design solutions based on CV criterion
stratification.eva.respY <- AssessStratification(data = data2017,
                                                 strata = data2017$strata_respY,
                                                 allocation = optimal.respY,
                                                 n.iter = 1000, n.burn = 500)
stratification.eva.visitsY <- AssessStratification(data = data2017,
                                                   strata = data2017$strata_visitsY,
                                                   allocation = optimal.visitsY,
                                                   n.iter = 1000, n.burn = 500)
stratification.eva.respX <- AssessStratification(data = data2017,
                                                 strata = data2017$strata_respX,
                                                 allocation = optimal.respX,
                                                 n.iter = 1000, n.burn = 500)
stratification.eva.visitsX <- AssessStratification(data = data2017,
                                                   strata = data2017$strata_visitsX,
                                                   allocation = optimal.visitsX,
                                                   n.iter = 1000, n.burn = 500)
stratification.eva.costX <- AssessStratification(data = data2017,
                                                 strata = data2017$strata_costX,
                                                 allocation = optimal.costX,
                                                 n.iter = 1000, n.burn = 500)

## run two chains for figure 3
stratification.eva.respY.2chains <- AssessStratification(data = data2017,
                                                         strata = data2017$strata_respY,
                                                         allocation = optimal.respY,
                                                         n.iter = 5000, n.burn = 1000, n.chain = 2)
stratification.eva.visitsY.2chains <- AssessStratification(data = data2017,
                                                           strata = data2017$strata_visitsY,
                                                           allocation = optimal.visitsY,
                                                           n.iter = 5000, n.burn = 1000, n.chain = 2)

## Extract CV
Design <- c("Design1", "Design2", "Design3", "Design4", "Design5")
CV.respY <- matrix(c(rep("ResponseY", 5), Design, rep(NA, 15)), ncol = 5)
CV.visitsY <- matrix(c(rep("VisitsY", 5), Design, rep(NA, 15)), ncol = 5)
CV.respX <- matrix(c(rep("ResponseX", 5), Design, rep(NA, 15)), ncol = 5)
CV.visitsX <- matrix(c(rep("VisitsX", 5), Design, rep(NA, 15)), ncol = 5)
CV.costX <- matrix(c(rep("CostX", 5), Design, rep(NA, 15)), ncol = 5)

colnames(CV.respY) <- c("Method", "Design", "CV", "lower", "upper")
colnames(CV.visitsY) <- c("Method", "Design", "CV", "lower", "upper")
colnames(CV.respX) <- c("Method", "Design", "CV", "lower", "upper")
colnames(CV.visitsX) <- c("Method", "Design", "CV", "lower", "upper")
colnames(CV.costX) <- c("Method", "Design", "CV", "lower", "upper")

CV.respY[, 3] <- round(apply(stratification.eva.respY$CV, 2, mean), digits = 5)
CV.visitsY[, 3] <- round(apply(stratification.eva.visitsY$CV, 2, mean), digits = 5)
CV.respX[, 3] <- round(apply(stratification.eva.respX$CV, 2, mean), digits = 5)
CV.visitsX[, 3] <- round(apply(stratification.eva.visitsX$CV, 2, mean), digits = 5)
CV.costX[, 3] <- round(apply(stratification.eva.costX$CV, 2, mean), digits = 5)

CV.respY[, 4:5] <- round(t(apply(stratification.eva.respY$CV, 2, quantile, probs = c(0.025, 0.975))), digits = 5)
CV.visitsY[, 4:5] <- round(t(apply(stratification.eva.visitsY$CV, 2, quantile, probs = c(0.025, 0.975))), digits = 5)
CV.respX[, 4:5] <- round(t(apply(stratification.eva.respX$CV, 2, quantile, probs = c(0.025, 0.975))), digits = 5)
CV.visitsX[, 4:5] <- round(t(apply(stratification.eva.visitsX$CV, 2, quantile, probs = c(0.025, 0.975))), digits = 5)
CV.costX[, 4:5] <- round(t(apply(stratification.eva.costX$CV, 2, quantile, probs = c(0.025, 0.975))), digits = 5)

CV <- data.frame(rbind(CV.respY, CV.visitsY, CV.respX, CV.visitsX, CV.costX))
CV$CV <- as.numeric(CV$CV)
CV$lower <- as.numeric(CV$lower)
CV$upper <- as.numeric(CV$upper)

