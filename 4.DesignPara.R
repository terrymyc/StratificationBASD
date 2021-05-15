
######################################################
#
# 4. Estimate design parameters
#
#    (SECTION 5.3 & APPENDIX C in manuscript)
#
#    Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file estimates design parameters as described 
# in manuscript section 5.3.

# Table 2 in manuscript records estimated response
# propensities per stratum and strategy based on
# ResponseY stratification.

# Figure 1 in manuscript illustrate Table 2.

# Figure C.2 in manuscript shows the trace plot of Gibbs draws.

# The 4.1 sampling process is the same when other
# stratification applies, but Table 2, Figure 1, and
# Figure C.2 are only built for ResponseY stratification.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(ggplot2)
library(reshape2)
library(latex2exp)


# -----------------------------------------------
# 4.1 Sampling by Gibbs Sampler
# -----------------------------------------------

## MLE estimates as starting values
res.s1 <- glm(FinalRespPhase1 ~ strata, 
              family = binomial(link = "probit"), data = data2017)
res.s2 <- glm(FinalRespPhase2 ~ strata, 
              family = binomial(link = "probit"), data = data2017)
res.s3 <- glm(FinalRespPhase3 ~ strata, 
              family = binomial(link = "probit"), data = data2017)
res.s2.cost <- lm(kostenTotaalPhase2 ~ strata, data = data2017)
res.s3.cost <- lm(kostenTotaalPhase23 ~ strata, data = data2017)

## supply starting values
init.s1 <- list(unname(res.s1$coefficients), rep(0, length(unique(data2017$strata))))
init.s2 <- list(unname(res.s2$coefficients), rep(0, length(unique(data2017$strata))))
init.s3 <- list(unname(res.s3$coefficients), rep(0, length(unique(data2017$strata))))
init.s2.cost <- list(unname(res.s2.cost$coefficients), rep(0, length(unique(data2017$strata))))
init.s3.cost <- list(unname(res.s3.cost$coefficients), rep(0, length(unique(data2017$strata))))

## generate all possible allocations of strategies
allocation.matrix <- as.matrix(expand.grid(rep(list(1:3), length(unique(data2017$strata)))))

## trigger Gibbs Sampler
set.seed(123)
samples <- GibbsTerry(data = data2017,
                      n.iter = 5000, n.burn = 1000, n.chain = 1, allocation = allocation.matrix,
                      inits.s1 = init.s1, inits.s2 = init.s2, inits.s3 = init.s3,
                      inits.s2.cost = init.s2.cost, inits.s3.cost = init.s3.cost)

## assess convergence
#plotSamples(samples$samples.s1)
#plotSamples(samples$samples.s2)
#plotSamples(samples$samples.s3)
#plotSamples(samples$samples.s2.cost)
#plotSamples(samples$samples.s3.cost)

## summary statistics of models
summaryPropensities(samples$samples.s1)
summaryPropensities(samples$samples.s2)
summaryPropensities(samples$samples.s3)
summaryCost(samples$samples.s2.cost)
summaryCost(samples$samples.s3.cost)


if (length(unique(data2017$strata)) == 9) {

# -------------------------------------------------------------
# 4.2 Store estimated response propensities to build Table 2
#     ONLY run when ResponseY stratification applies
# -------------------------------------------------------------

propensitiesByStrata <- RespPropensity(data2017, samples$prop.g.s1[[1]], 
                                       samples$prop.g.s2[[1]], 
                                       samples$prop.g.s3[[1]])

Table2 <- file("Tables/Table2.txt")
sink(Table2, type = "output")
print(propensitiesByStrata)
sink()
close(Table2)


# ----------------------------------------------------------
# 4.3 Figure 1
#     ONLY run when ResponseY stratification applies
# ----------------------------------------------------------

# prepare data of expectations and intervals
stratum <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
df.exp <- data.frame(stratum,
                     t(propensitiesByStrata$statistics))
df.lower <- data.frame(stratum,
                       propensitiesByStrata$quantiles.s1[, 1],
                       propensitiesByStrata$quantiles.s2[, 1],
                       propensitiesByStrata$quantiles.s3[, 1])
df.upper <- data.frame(stratum,
                       propensitiesByStrata$quantiles.s1[, 5],
                       propensitiesByStrata$quantiles.s2[, 5],
                       propensitiesByStrata$quantiles.s3[, 5])
col_name <- c("stratum", "Web", "F2F-short", "F2F-extended")
colnames(df.exp) <- col_name
colnames(df.lower) <- col_name
colnames(df.upper) <- col_name

df.exp.long <- melt(df.exp,
                    id.vars = "stratum",
                    measure.vars = c("Web", "F2F-short", "F2F-extended"),
                    variable.name = "strategy")
df.lower.long <- melt(df.lower,
                      id.vars = "stratum",
                      measure.vars = c("Web", "F2F-short", "F2F-extended"),
                      variable.name = "strategy")
colnames(df.lower.long)[3] <- "lower"
df.upper.long <- melt(df.upper,
                      id.vars = "stratum",
                      measure.vars = c("Web", "F2F-short", "F2F-extended"),
                      variable.name = "strategy")
colnames(df.upper.long)[3] <- "upper"

interval <- inner_join(df.lower.long, df.upper.long, by = c("stratum", "strategy"))
df <- inner_join(df.exp.long, interval, by = c("stratum", "strategy"))

# plot and save
ggplot(df, aes(stratum, value, group = strategy)) +
  geom_pointrange(aes(ymin = lower, ymax = upper, shape = strategy, color = strategy),
                  position = position_dodge(0.3)) +
  labs(x = TeX("Stratum, $g$"), 
       y = TeX("Response propensities, $\\rho_g(s)$"),
       shape = TeX("Strategy, $s$"),
       color = TeX("Strategy, $s$")) +
  theme_light()
ggsave("Figures/Figure1.png", width = 8, height = 5)


# ------------------------------------------------------------
# 4.4 Figure C.2 Survey design parameter model parameters
#     ONLY run when ResponseY stratification applies
# ------------------------------------------------------------

## trigger Gibbs Sampler
set.seed(123)
samples.2c <- GibbsTerryWoOpt(data = data2017,
                           n.iter = 5000, n.burn = 1000, n.chain = 2,
                           inits.s1 = init.s1, inits.s2 = init.s2, inits.s3 = init.s3,
                           inits.s2.cost = init.s2.cost, inits.s3.cost = init.s3.cost)

png("Figures/FigureC2.png", width = 8, height = 9, units = "in", res = 400)

layout(matrix(c(1, 1, 2, 3, 4, 5), 3, 2, byrow = T))

## Response at the end of phase 1
plot(samples.2c$samples.s1[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.2c$samples.s1[[2]][, 1]), 
              max(samples.2c$samples.s1[[2]][, "strata9"])),
     main = "Response at the End of Phase 1")
lines(samples.2c$samples.s1[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s1[[1]][, "strata2"], type = "l", col = "coral")
lines(samples.2c$samples.s1[[2]][, "strata2"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s1[[1]][, "strata5"], type = "l", col = "coral")
lines(samples.2c$samples.s1[[2]][, "strata5"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s1[[1]][, "strata9"], type = "l", col = "coral")
lines(samples.2c$samples.s1[[2]][, "strata9"], type = "l", col = "deepskyblue")

## Response at the end of phase 2
plot(samples.2c$samples.s2[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.2c$samples.s2[[2]][, 1]), 
              max(samples.2c$samples.s2[[2]][, "strata9"])),
     main = "Response at the End of Phase 2")
lines(samples.2c$samples.s2[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2[[1]][, "strata2"], type = "l", col = "coral")
lines(samples.2c$samples.s2[[2]][, "strata2"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2[[1]][, "strata5"], type = "l", col = "coral")
lines(samples.2c$samples.s2[[2]][, "strata5"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2[[1]][, "strata9"], type = "l", col = "coral")
lines(samples.2c$samples.s2[[2]][, "strata9"], type = "l", col = "deepskyblue")

## Response at the end of phase 3
plot(samples.2c$samples.s3[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.2c$samples.s3[[1]][, "strata2"]), 
              max(samples.2c$samples.s3[[2]][, "strata5"])),
     main = "Response at the End of Phase 3")
lines(samples.2c$samples.s3[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3[[1]][, "strata2"], type = "l", col = "coral")
lines(samples.2c$samples.s3[[2]][, "strata2"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3[[1]][, "strata5"], type = "l", col = "coral")
lines(samples.2c$samples.s3[[2]][, "strata5"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3[[1]][, "strata9"], type = "l", col = "coral")
lines(samples.2c$samples.s3[[2]][, "strata9"], type = "l", col = "deepskyblue")


## Cost at the end of phase 2
plot(samples.2c$samples.s2.cost[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.2c$samples.s2.cost[[1]][, "strata2"]), 
              max(samples.2c$samples.s2.cost[[2]][, 1])),
     main = "Cost at the End of Phase 2")
lines(samples.2c$samples.s2.cost[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2.cost[[1]][, "strata2"], type = "l", col = "coral")
lines(samples.2c$samples.s2.cost[[2]][, "strata2"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2.cost[[1]][, "strata5"], type = "l", col = "coral")
lines(samples.2c$samples.s2.cost[[2]][, "strata5"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s2.cost[[1]][, "strata9"], type = "l", col = "coral")
lines(samples.2c$samples.s2.cost[[2]][, "strata9"], type = "l", col = "deepskyblue")

## Cost at the end of phase 3
plot(samples.2c$samples.s3.cost[[1]][, 1], type = "l", col = "coral",
     xlab = "Gibbs iterations", ylab = "Slope parameters",
     ylim = c(min(samples.2c$samples.s3.cost[[2]][, "strata2"]), 
              max(samples.2c$samples.s3.cost[[2]][, 1])),
     main = "Cost at the End of Phase 3")
lines(samples.2c$samples.s3.cost[[2]][, 1], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3.cost[[1]][, "strata2"], type = "l", col = "coral")
lines(samples.2c$samples.s3.cost[[2]][, "strata2"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3.cost[[1]][, "strata5"], type = "l", col = "coral")
lines(samples.2c$samples.s3.cost[[2]][, "strata5"], type = "l", col = "deepskyblue")
lines(samples.2c$samples.s3.cost[[1]][, "strata9"], type = "l", col = "coral")
lines(samples.2c$samples.s3.cost[[2]][, "strata9"], type = "l", col = "deepskyblue")

dev.off()

}