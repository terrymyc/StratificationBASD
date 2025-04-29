
######################################################
#
# 3.3 CostX Stratification
#
#     Updated on 13/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the total costs.

# The inputs are auxiliary variables linked from
# administrative data.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# 3.3.1 Stratification explaining costs
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(12345)
tree.cost <- rpart(kostenTotaalCAWICAPI ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse +
                    BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                    TypeHH + OplNivHB + InschrCWI,
                  data = data2017, method = "anova", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.cost)
plotcp(tree.cost)

## prune the tree
tree.cost <- prune(tree.cost, cp = 0.0017)

## extract splitting rules
rules.cost <- tidyRules(tree.cost)

## create stratum indicator variable
data2017 <- data2017 %>%
  mutate(strata_costX = ifelse(eval(parse(text = rules.cost[1, "LHS"])), 1, 
                               ifelse(eval(parse(text = rules.cost[2, "LHS"])), 2,
                                      ifelse(eval(parse(text = rules.cost[3, "LHS"])), 3,
                                             ifelse(eval(parse(text = rules.cost[4, "LHS"])), 4,
                                                    ifelse(eval(parse(text = rules.cost[5, "LHS"])), 5,
                                                           ifelse(eval(parse(text = rules.cost[6, "LHS"])), 6, 7)))))))

data2017$strata_costX <- as.factor(data2017$strata_costX)
table(data2017$strata_costX)

## true response rates in each phase
tapply(data2017$FinalRespPhase1, data2017$strata_costX, mean)
tapply(data2017$FinalRespPhase2, data2017$strata_costX, mean)
tapply(data2017$FinalRespPhase3, data2017$strata_costX, mean)
