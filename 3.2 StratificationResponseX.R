
######################################################
#
# 3.2 ResponseX Stratification
#
#     (SECTION 5.3 in manuscript)
#
#     Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the response outcome of Web mode in data
# collection phase 1 as described in manuscript section 5.3.

# The inputs are auxiliary variables linked from
# administrative data.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(dplyr)
library(rpart)  ## classification tree
library(tidyrules)  ## extract splitting rules


# --------------------------------------------------------
# 3.2.1 Stratification based on auxiliary variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(123)
tree.aux <- rpart(FinalRespPhase1 ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse +
                    BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                    TypeHH + OplNivHB + InschrCWI,
                  data = data2017, method = "class", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.aux)
plotcp(tree.aux)

## prune the tree
tree.aux <- prune(tree.aux, cp = 0.0011)

## extract splitting rules
rules.aux <- tidyRules(tree.aux)

## create stratum indicator variable
strata1 <- data2017 %>%
  filter(eval(parse(text = rules.aux[1, "LHS"]))) %>%
  mutate(strata = 1)
strata2 <- data2017 %>%
  filter(eval(parse(text = rules.aux[2, "LHS"]))) %>%
  mutate(strata = 2)
strata3 <- data2017 %>%
  filter(eval(parse(text = rules.aux[3, "LHS"]))) %>%
  mutate(strata = 3)
strata4 <- data2017 %>%
  filter(eval(parse(text = rules.aux[4, "LHS"]))) %>%
  mutate(strata = 4)
strata5 <- data2017 %>%
  filter(eval(parse(text = rules.aux[5, "LHS"]))) %>%
  mutate(strata = 5)
strata6 <- data2017 %>%
  filter(eval(parse(text = rules.aux[6, "LHS"]))) %>%
  mutate(strata = 6)
strata7 <- data2017 %>%
  filter(eval(parse(text = rules.aux[7, "LHS"]))) %>%
  mutate(strata = 7)
strata8 <- data2017 %>%
  filter(eval(parse(text = rules.aux[8, "LHS"]))) %>%
  mutate(strata = 8)
strata9 <- data2017 %>%
  filter(eval(parse(text = rules.aux[9, "LHS"]))) %>%
  mutate(strata = 9)
strata10 <- data2017 %>%
  filter(eval(parse(text = rules.aux[10, "LHS"]))) %>%
  mutate(strata = 10)
data2017 <- rbind(strata1, strata2, strata3, strata4,
                  strata5, strata6, strata7, strata8,
                  strata9, strata10)

data2017$strata <- as.factor(data2017$strata)
table(data2017$strata)
