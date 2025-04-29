
######################################################
#
# 3.2 VisitsY Stratification
#
#     Updated on 29/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the number of visits.

# The inputs are expected values of survey variables
# estimated in 2.BayesModelSvyVar.R.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# 3.1.1 Stratification based on predicted survey variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(12345)
tree.visitsY <- rpart(aantalBezoeken ~ Health.prob + Smoke.prob + Obese.prob,
                  data = data2017, method = "anova", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.visitsY)
plotcp(tree.visitsY)

## prune the tree
tree.visitsY <- prune(tree.visitsY, cp = 0.0013)

## extract splitting rules
rules.visitsY <- tidyRules(tree.visitsY)

## create stratum indicator variable
data2017 <- data2017 %>%
  mutate(strata_visitsY = ifelse(eval(parse(text = rules.visitsY[1, "LHS"])), 1, 
                               ifelse(eval(parse(text = rules.visitsY[2, "LHS"])), 2,
                                      ifelse(eval(parse(text = rules.visitsY[3, "LHS"])), 3,
                                             ifelse(eval(parse(text = rules.visitsY[4, "LHS"])), 4,
                                                    ifelse(eval(parse(text = rules.visitsY[5, "LHS"])), 5,
                                                           ifelse(eval(parse(text = rules.visitsY[6, "LHS"])), 6,
                                                                  ifelse(eval(parse(text = rules.visitsY[7, "LHS"])), 7, 8))))))))

data2017$strata_visitsY <- as.factor(data2017$strata_visitsY)
table(data2017$strata_visitsY)

## true response rates in each phase
tapply(data2017$FinalRespPhase1, data2017$strata_visitsY, mean)
tapply(data2017$FinalRespPhase2, data2017$strata_visitsY, mean)
tapply(data2017$FinalRespPhase3, data2017$strata_visitsY, mean)

