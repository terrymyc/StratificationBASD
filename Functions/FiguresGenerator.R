
####################################################
#
# Functions for Generating Figures
#
# Updated on 29/07/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file defines functions for generating figures.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ----------------------------------------------------------
# Figure 1 Estimated response propensity per stratum and strategy
# ----------------------------------------------------------

figure1.prop.est <- function(prop.est, n.strata, method) {
  # prepare data of expectations and intervals
  stratum <- c(1:n.strata)
  df.exp <- data.frame(stratum,
                       t(prop.est$statistics))
  df.lower <- data.frame(stratum,
                         prop.est$quantiles.s1[, 1],
                         prop.est$quantiles.s2[, 1],
                         prop.est$quantiles.s3[, 1])
  df.upper <- data.frame(stratum,
                         prop.est$quantiles.s1[, 5],
                         prop.est$quantiles.s2[, 5],
                         prop.est$quantiles.s3[, 5])
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
  df$stratum <- as.factor(as.integer(df$stratum))
  
  # plot and save
  plot <- ggplot(df, aes(stratum, value, group = strategy)) +
    geom_pointrange(aes(ymin = lower, ymax = upper, shape = strategy, color = strategy),
                    position = position_dodge(0.3)) +
    ylim(0.1, 0.8) +
    labs(x = TeX("Stratum, $g$"),
         y = TeX("Response propensities, $\\rho_g(s)$"),
         shape = TeX("Strategy, $s$"),
         color = TeX("Strategy, $s$")) +
    theme_light()
  #ggsave(paste0("Figures/Figure1_", method, ".png"),
  #       width = 8, height = 5, units = "in", dpi = 600)

  return(plot)
}





# ----------------------------------------------------------
# Figure 2 CV criterion of individual response propensities of top 5 solutions
# ----------------------------------------------------------

figure2 <- function(CV.realized, CV) {
  # plot and save
  plot <- ggplot(CV, aes(Method, CV, group = Design)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), alpha = 0.8,
                    position = position_dodge(0.3)) +
    geom_hline(yintercept = CV.realized, color = "red") +
    labs(x = "Stratification method",
         y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(E\\left(Y|X\\right),s_{\\phi_{opt},i}\\right)\\right)$")) +
    theme_light() +
    ylim(0.03, 0.18)
  #ggsave("Figures/Figure2_100perB.png", width = 8, height = 5, units = "in", dpi = 600)
  
  return(plot)
}




# ----------------------------------------------------------
# Figure 3 CV of stratum response propensities of top 100 solutions
# ----------------------------------------------------------

figure3 <- function(design) {
  # plot against budget
  plot.a <- ggplot(design, aes(B, CV)) +
    geom_pointrange(aes(ymin = CV.025, ymax = CV.975), alpha = 0.3) +
    geom_pointrange(data = design[1:5, ],
                    aes(ymin = CV.025, ymax = CV.975),
                    color = "red") +
    labs(x = TeX("Expectation of budget per respondent, $E\\left(B\\left(C(s_{\\phi}),\\rho_y(s_{\\phi})\\right)\\right)$"),
         y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(\\rho_y(s_{\\phi})\\right)\\right)$")) +
    theme_light()
  #ggsave("Figures/Figure3a.png", width = 8, height = 7, units = "in", dpi = 600)
  
  # plot against response rate
  plot.b <- ggplot(design, aes(RR, CV)) +
    geom_pointrange(aes(ymin = CV.025, ymax = CV.975), alpha = 0.3) +
    geom_pointrange(data = design[1:5, ],
                    aes(ymin = CV.025, ymax = CV.975),
                    color = "red") +
    labs(x = TeX("Expectation of response rate, $E\\left(RR(\\rho_y(s_{\\phi}))\\right)$"),
         y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(\\rho_y(s_{\\phi})\\right)\\right)$")) +
    theme_light()
  #ggsave("Figures/Figure3b.png", width = 8, height = 7, units = "in", dpi = 600)
  
  return(list(plot.a, plot.b))
}




# ------------------------------------------------------------
# Figure C.1 Trace plot for survey variable model parameters
# ------------------------------------------------------------

figureC1 <- function(samples.health, samples.smoke, samples.obese) {
  #png("Figures/FigureC1.png", width = 8, height = 9, units = "in", res = 600)
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
  
  #dev.off()
}



# ------------------------------------------------------------
# Figure C.2 Trace plot for survey design parameter model parameters
# Designed for visitsY
# ------------------------------------------------------------

figureC2 <- function(samples) {
  #png("Figures/FigureC2.png", width = 8, height = 9, units = "in", res = 600)
  
  layout(matrix(c(1, 1, 2, 3, 4, 5), 3, 2, byrow = T))
  
  ## Response at the end of phase 1
  plot(samples$samples.prop.s1[[1]][, 1], type = "l", col = "coral",
       xlab = "Gibbs iterations", ylab = "Slope parameters",
       ylim = c(min(samples$samples.prop.s1[[1]][, "strata8"]), 
                max(samples$samples.prop.s1[[2]][, 1])),
       main = "Response at the End of Phase 1")
  lines(samples$samples.prop.s1[[2]][, 1], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s1[[1]][, "strata2"], type = "l", col = "coral")
  lines(samples$samples.prop.s1[[2]][, "strata2"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s1[[1]][, "strata5"], type = "l", col = "coral")
  lines(samples$samples.prop.s1[[2]][, "strata5"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s1[[1]][, "strata8"], type = "l", col = "coral")
  lines(samples$samples.prop.s1[[2]][, "strata8"], type = "l", col = "deepskyblue")
  
  ## Response at the end of phase 2
  plot(samples$samples.prop.s2[[1]][, 1], type = "l", col = "coral",
       xlab = "Gibbs iterations", ylab = "Slope parameters",
       ylim = c(min(samples$samples.prop.s2[[2]][, "strata8"]), 
                max(samples$samples.prop.s2[[2]][, 1])),
       main = "Response at the End of Phase 2")
  lines(samples$samples.prop.s2[[2]][, 1], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s2[[1]][, "strata2"], type = "l", col = "coral")
  lines(samples$samples.prop.s2[[2]][, "strata2"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s2[[1]][, "strata5"], type = "l", col = "coral")
  lines(samples$samples.prop.s2[[2]][, "strata5"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s2[[1]][, "strata8"], type = "l", col = "coral")
  lines(samples$samples.prop.s2[[2]][, "strata8"], type = "l", col = "deepskyblue")
  
  ## Response at the end of phase 3
  plot(samples$samples.prop.s3[[1]][, 1], type = "l", col = "coral",
       xlab = "Gibbs iterations", ylab = "Slope parameters",
       ylim = c(min(samples$samples.prop.s3[[1]][, "strata8"]), 
                max(samples$samples.prop.s3[[2]][, 1])),
       main = "Response at the End of Phase 3")
  lines(samples$samples.prop.s3[[2]][, 1], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s3[[1]][, "strata2"], type = "l", col = "coral")
  lines(samples$samples.prop.s3[[2]][, "strata2"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s3[[1]][, "strata5"], type = "l", col = "coral")
  lines(samples$samples.prop.s3[[2]][, "strata5"], type = "l", col = "deepskyblue")
  lines(samples$samples.prop.s3[[1]][, "strata8"], type = "l", col = "coral")
  lines(samples$samples.prop.s3[[2]][, "strata8"], type = "l", col = "deepskyblue")
  
  
  ## Cost at the end of phase 2
  plot(samples$samples.cost.s2[[1]][, 1], type = "l", col = "coral",
       xlab = "Gibbs iterations", ylab = "Slope parameters",
       ylim = c(min(samples$samples.cost.s2[[1]][, "strata2"]), 
                max(samples$samples.cost.s2[[1]][, 1])),
       main = "Cost at the End of Phase 2")
  lines(samples$samples.cost.s2[[2]][, 1], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s2[[1]][, "strata2"], type = "l", col = "coral")
  lines(samples$samples.cost.s2[[2]][, "strata2"], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s2[[1]][, "strata5"], type = "l", col = "coral")
  lines(samples$samples.cost.s2[[2]][, "strata5"], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s2[[1]][, "strata8"], type = "l", col = "coral")
  lines(samples$samples.cost.s2[[2]][, "strata8"], type = "l", col = "deepskyblue")
  
  ## Cost at the end of phase 3
  plot(samples$samples.cost.s3[[1]][, 1], type = "l", col = "coral",
       xlab = "Gibbs iterations", ylab = "Slope parameters",
       ylim = c(min(samples$samples.cost.s3[[2]][, "strata2"]), 
                max(samples$samples.cost.s3[[2]][, 1])),
       main = "Cost at the End of Phase 3")
  lines(samples$samples.cost.s3[[2]][, 1], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s3[[1]][, "strata2"], type = "l", col = "coral")
  lines(samples$samples.cost.s3[[2]][, "strata2"], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s3[[1]][, "strata5"], type = "l", col = "coral")
  lines(samples$samples.cost.s3[[2]][, "strata5"], type = "l", col = "deepskyblue")
  lines(samples$samples.cost.s3[[1]][, "strata8"], type = "l", col = "coral")
  lines(samples$samples.cost.s3[[2]][, "strata8"], type = "l", col = "deepskyblue")

  #dev.off()
}




# ------------------------------------------------------------
# Figure C.3 Trace plot for stratification-assessed propensity model parameters
# ------------------------------------------------------------

figureC3 <- function(samples) {
  #png("Figures/FigureC3.png", width = 8, height = 9, units = "in", res = 600)
  
  layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = T))
  for (i in 1:5) {
    plot(as.matrix(samples$fit.prop.c1[[i]])[, 1], type = "l", col = "coral",
         xlab = "Gibbs iterations", ylab = "Slope parameters",
         ylim = c(min(samples$fit.prop.c1[[i]][, 1]),
                  max(samples$fit.prop.c1[[i]][, 4])),
         main = paste("Design", i))
    lines(samples$fit.prop.c2[[i]][, 1], type = "l", col = "deepskyblue")
    lines(samples$fit.prop.c1[[i]][, 2], type = "l", col = "coral")
    lines(samples$fit.prop.c2[[i]][, 2], type = "l", col = "deepskyblue")
    lines(samples$fit.prop.c1[[i]][, 3], type = "l", col = "coral")
    lines(samples$fit.prop.c2[[i]][, 3], type = "l", col = "deepskyblue")
    lines(samples$fit.prop.c1[[i]][, 4], type = "l", col = "coral")
    lines(samples$fit.prop.c2[[i]][, 4], type = "l", col = "deepskyblue")
  }
  
  #dev.off()
}

