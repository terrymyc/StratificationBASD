
######################################################
#
# 5. Optimization of adaptive survey designs
#
#    (SECTION 5.4 & APPENDIX C in manuscript)
#
#    Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file optimizes the quality and cost indicators
# as described in manuscript section 5.4.1.

# This file assesses the optimal designs based on
# different stratification as described in manuscript 
# section 5.4.2.

# Table 3 and Figure 3 show optimal designs as described
# in manuscript section 5.4.3

# Figure C.3 in manuscript shows the trace plot of Gibbs draws
# of stratification-assessed propensity model.

# The optimization and assessment process in 5.1 & 5.2
# are the same when other stratification applies, but
# Table 3, Figure 3, and Figure C.3 are only built 
# for ResponseY stratification.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ----------------------------------------------------------
# 5.1 Optimize quality and cost indicators
# ----------------------------------------------------------

## evaluate the performance of all strategies
solution.assess <- summaryIndicator(allocation = allocation.matrix, CV = samples$cv.response,
                                    RR = samples$response.rate, RR.restraint = 0.5,
                                    B = samples$budget, B.restraint = 42)

## optimization based on RESPONSE RATES and COST per respondent
optimized <- solution.assess$statistics[solution.assess$statistics[, "RR"] >= 0.5 &
                                        solution.assess$statistics[, "B.restraint"] < 0.1, ]

## extract optimal design solutions
optimal.allocation <- unname(optimized[order(optimized[, "CV"]),][1:5, 1:ncol(allocation.matrix)])


# ----------------------------------------------------------
# 5.2 Assess optimal designs based on CV criterion
# ----------------------------------------------------------

## Evaluate posteriors of optimal design solutions based on CV criterion
set.seed(123)
eva.samples <- GibbsOptimal(data = data2017, allocation = optimal.allocation,
                            n.iter = 5000, n.burn = 1000)

## Extract CV
if (ncol(optimal.allocation) == 9) {
  CV <- matrix(c(rep("Optimal", 5), 
                 "Design1", "Design2", "Design3", "Design4", "Design5", 
                 rep(NA, 15)), 
               ncol = 5)
} else if (ncol(optimal.allocation) == 10) {
  CV <- matrix(c(rep("ResponseX", 5), 
                 "Design1", "Design2", "Design3", "Design4", "Design5", 
                 rep(NA, 15)), 
               ncol = 5)
} else {
  CV <- matrix(c(rep("CostX", 5), 
                 "Design1", "Design2", "Design3", "Design4", "Design5", 
                 rep(NA, 15)), 
               ncol = 5)
}
colnames(CV) <- c("Method", "Design", "CV", "lower", "upper")

for (i in 1:5) {
  CV[i, 3] <- summaryOptimalDesigns(eva.samples[[i]])$statistics["CV", "Mean"]
  CV[i, 4:5] <- summaryOptimalDesigns(eva.samples[[i]])$quantiles["CV", c("2.5%", "97.5%")]
}

write.csv(CV, paste0("Output/", ncol(optimal.allocation), "StrataOptimalDesigns.csv"))


if (length(unique(data2017$strata)) == 9) {
# ----------------------------------------------------------
# 5.3 Table 3
#     ONLY run when ResponseY stratification applies
# ----------------------------------------------------------

Table3 <- file("Tables/Table3.txt")
sink(Table3, type = "output")
print(optimized[order(optimized[, "CV"]),][1:5, ])
sink()
close(Table3)


# ----------------------------------------------------------
# 5.4 Figure 3
#     ONLY run when ResponseY stratification applies
# ----------------------------------------------------------

# prepare data
opt.design <- data.frame(optimized[order(optimized[, "CV"]), ][1:150, c("CV", "CV.025", "CV.975", "RR", "B")])
opt.design.highlight <- opt.design[1:5, ]

png("Figures/Figure3a.png", width = 8, height = 7, units = "in", res = 600)

# plot against budget
ggplot(opt.design, aes(B, CV)) +
  geom_pointrange(aes(ymin = CV.025, ymax = CV.975), alpha = 0.3) +
  geom_pointrange(data = opt.design.highlight,
                  aes(ymin = CV.025, ymax = CV.975),
                  color = "red") +
  labs(x = TeX("Expectation of budget per respondent, $E\\left(B\\left(C(s_{\\phi}),\\rho_y(s_{\\phi})\\right)\\right)$"),
       y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(\\rho_y(s_{\\phi})\\right)\\right)$")) +
  theme_light()

dev.off()

png("Figures/Figure3b.png", width = 8, height = 7, units = "in", res = 600)

# plot against response rate
ggplot(opt.design, aes(RR, CV)) +
  geom_pointrange(aes(ymin = CV.025, ymax = CV.975), alpha = 0.3) +
  geom_pointrange(data = opt.design.highlight,
                  aes(ymin = CV.025, ymax = CV.975),
                  color = "red") +
  labs(x = TeX("Expectation of response rate, $E\\left(RR(\\rho_y(s_{\\phi}))\\right)$"),
       y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(\\rho_y(s_{\\phi})\\right)\\right)$")) +
  theme_light()

dev.off()


# ------------------------------------------------------------
# 5.5 Figure C.3 Stratification-assessed propensity model parameters
#     ONLY run when ResponseY stratification applies
# ------------------------------------------------------------

## Evaluate posteriors of optimal design solutions based on CV criterion
## run two chains
set.seed(123)
eva.samples.2c <- GibbsOptimal(data = data2017, allocation = optimal.allocation,
                            n.iter = 5000, n.burn = 1000, n.chain = 2)

png("Figures/FigureC3.png", width = 8, height = 9, units = "in", res = 400)

TracePlotCrit(eva.samples.2c)

dev.off()

}


# ------------------------------------------------------------
# 5.6 Remove strata indicator variable from data
# ------------------------------------------------------------

data2017 <- within(data2017, rm(strata))
