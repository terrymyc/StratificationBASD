
######################################################
#
# 3.1 ResponseY Stratification
#
#     Updated on 29/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the final response outcome.

# The inputs are expected values of survey variables
# estimated in 2.BayesModelSvyVar.R.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# 3.1.1 Stratification based on predicted survey variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(12345)
tree.respY <- rpart(FinalRespPhase3 ~ Health.prob + Smoke.prob + Obese.prob,
                  data = data2017, method = "class",
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.respY)
plotcp(tree.respY)

## prune the tree
tree.respY <- prune(tree.respY, cp = 0.0025)

## extract splitting rules
rules.respY <- tidyRules(tree.respY)

## create stratum indicator variable
data2017 <- data2017 %>%
  mutate(strata_respY = ifelse(eval(parse(text = rules.respY[1, "LHS"])), 1, 
                                    ifelse(eval(parse(text = rules.respY[2, "LHS"])), 2,
                                           ifelse(eval(parse(text = rules.respY[3, "LHS"])), 3,
                                                  ifelse(eval(parse(text = rules.respY[4, "LHS"])), 4,
                                                         ifelse(eval(parse(text = rules.respY[5, "LHS"])), 5, 6))))))


data2017$strata_respY <- as.factor(data2017$strata_respY)
table(data2017$strata_respY)


## true response rates in each phase
tapply(data2017$FinalRespPhase1, data2017$strata_respY, mean)
tapply(data2017$FinalRespPhase2, data2017$strata_respY, mean)
tapply(data2017$FinalRespPhase3, data2017$strata_respY, mean)

