
#########################################################
#
# 3. Stratification by classification tree algorithm
#
#    Updated on 20/01/2021
#
#########################################################

library(rpart)  ## classification tree
library(rpart.plot)  ## plot the tree


# ------------------------------------
# 3.1 Recode response indicator
# ------------------------------------
## Indicator for responses to different phases
data2017[data2017$ModeRespons == "CAWI", "PhaseRespons"] <- "CAWI"
data2017[data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken <= 3, "PhaseRespons"] <- "CAPI-short"
data2017[data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken > 3, "PhaseRespons"] <- "CAPI-extended"
data2017[data2017$ModeRespons == ".", "PhaseRespons"] <- NA
data2017$PhaseRespons <- as.factor(data2017$PhaseRespons)
table(data2017$PhaseRespons, data2017$aantalBezoeken)


# --------------------------------------------------------
# 3.2 Stratification based on predicted survey variables
# --------------------------------------------------------
## perform classification tree algorithm
set.seed(123)
tree.svy <- rpart(PhaseRespons ~ Health.prob + Smoke.prob + Obese.prob,
                  data = data2017, method = "class", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.svy)

## plot the tree
rpart.plot(tree.svy)

## extract the classification rules
rpart.rules(tree.svy)

## create strata indicator
data2017[data2017$Smoke.prob >= 0.34 & data2017$Health.prob <  0.67, "strata"] <- 1
data2017[data2017$Smoke.prob <  0.24 & data2017$Health.prob <  0.54 & data2017$Obese.prob >= 0.037, "strata"] <- 2
data2017[data2017$Smoke.prob >= 0.34 & data2017$Health.prob >= 0.67, "strata"] <- 3
data2017[data2017$Smoke.prob <  0.24 & data2017$Obese.prob  <  0.037, "strata"] <- 4
data2017[data2017$Smoke.prob >= 0.24 & data2017$Smoke.prob  <  0.34, "strata"] <- 5
data2017[data2017$Smoke.prob <  0.24 & data2017$Health.prob >= 0.54 & data2017$Obese.prob >= 0.037, "strata"] <- 6
data2017$strata <- as.factor(data2017$strata)
table(data2017$strata)

