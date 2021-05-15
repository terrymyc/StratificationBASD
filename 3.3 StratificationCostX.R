
######################################################
#
# 3.3 CostX Stratification
#
#     (SECTION 5.3 in manuscript)
#
#     Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the number of visits as described in manuscript
# section 5.3.

# The inputs are auxiliary variables linked from
# administrative data.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(dplyr)
library(rpart)  ## classification tree
library(tidyrules)  ## extract splitting rules

# --------------------------------------------------------
# 3.3.1 Stratification explaining costs
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(123)
tree.cost <- rpart(aantalBezoeken ~ Leeftijdsklasse13 + Geslacht + Inkomensklasse +
                    BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                    TypeHH + OplNivHB + InschrCWI,
                  data = data2017, method = "anova", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.cost)
plotcp(tree.cost)

## prune the tree
tree.cost <- prune(tree.cost, cp = 0.0014)

## extract splitting rules
rules.cost <- tidyRules(tree.cost)

## create stratum indicator variable
strata1 <- data2017 %>%
  filter(eval(parse(text = rules.cost[1, "LHS"]))) %>%
  mutate(strata = 1)
strata2 <- data2017 %>%
  filter(eval(parse(text = rules.cost[2, "LHS"]))) %>%
  mutate(strata = 2)
strata3 <- data2017 %>%
  filter(eval(parse(text = rules.cost[3, "LHS"]))) %>%
  mutate(strata = 3)
strata4 <- data2017 %>%
  filter(eval(parse(text = rules.cost[4, "LHS"]))) %>%
  mutate(strata = 4)
strata5 <- data2017 %>%
  filter(eval(parse(text = rules.cost[5, "LHS"]))) %>%
  mutate(strata = 5)
strata6 <- data2017 %>%
  filter(eval(parse(text = rules.cost[6, "LHS"]))) %>%
  mutate(strata = 6)
strata7 <- data2017 %>%
  filter(eval(parse(text = rules.cost[7, "LHS"]))) %>%
  mutate(strata = 7)

data2017 <- rbind(strata1, strata2, strata3, strata4, strata5,
                  strata6, strata7)
data2017$strata <- as.factor(data2017$strata)
table(data2017$strata)
