
######################################################
#
# 5. Determine optimal stratification
#
#    (SECTION 5.4.2 in manuscript)
#
#    Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file plots the optimal designs based on
# different stratification as described in manuscript 
# section 5.4.2.

# ONLY run when Output folder contains csv files for
# ResponseY, ResponseX, and CostX stratification.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(ggplot2)
library(latex2exp)

# ------------------------------------------------------------
# 6.1 Figure 2
# ------------------------------------------------------------

## Read optimal designs
CV.ResponseY <- read.csv("Output/9StrataOptimalDesigns.csv")[, -1]
CV.ResponseX <- read.csv("Output/10StrataOptimalDesigns.csv")[, -1]
CV.CostX <- read.csv("Output/7StrataOptimalDesigns.csv")[, -1]

## prepare data
strataCV <- data.frame(rbind(CV.ResponseY, CV.ResponseX, CV.CostX))

## plot and save
png("Figures/Figure2.png", width = 8, height = 5, units = "in", res = 600)

ggplot(strataCV, aes(Method, CV, group = Design)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), alpha = 0.8,
                  position = position_dodge(0.3)) +
  labs(x = "Stratification method",
       y = TeX("Expectation of CV of response propensities, $E\\left(CV\\left(E\\left(Y|X\\right),s_{\\phi_{opt},i}\\right)\\right)$")) +
  theme_light()

dev.off()
