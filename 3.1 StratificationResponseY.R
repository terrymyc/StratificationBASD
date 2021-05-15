
######################################################
#
# 3.1 ResponseY Stratification
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

# The inputs are expected values of survey variables
# estimated in 2.BayesModelSvyVar.R.

# Table 1 in manuscript records the classification output.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(dplyr)
library(rpart)  ## classification tree
library(tidyrules)  ## extract splitting rules


# --------------------------------------------------------
# 3.1.1 Stratification based on predicted survey variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(123)
tree.svy <- rpart(FinalRespPhase1 ~ Health.prob + Smoke.prob + Obese.prob,
                  data = data2017, method = "class", 
                  control = rpart.control(cp = 0.0001, minsplit = 500))

## print complex parameters
printcp(tree.svy)
plotcp(tree.svy)

## prune the tree
tree.svy <- prune(tree.svy, cp = 0.0006)

## extract splitting rules
rules.svy <- tidyRules(tree.svy)

## create stratum indicator variable
strata1 <- data2017 %>%
  filter(eval(parse(text = rules.svy[1, "LHS"]))) %>%
  mutate(strata = 1)
strata2 <- data2017 %>%
  filter(eval(parse(text = rules.svy[2, "LHS"]))) %>%
  mutate(strata = 2)
strata3 <- data2017 %>%
  filter(eval(parse(text = rules.svy[3, "LHS"]))) %>%
  mutate(strata = 3)
strata4 <- data2017 %>%
  filter(eval(parse(text = rules.svy[4, "LHS"]))) %>%
  mutate(strata = 4)
strata5 <- data2017 %>%
  filter(eval(parse(text = rules.svy[5, "LHS"]))) %>%
  mutate(strata = 5)
strata6 <- data2017 %>%
  filter(eval(parse(text = rules.svy[6, "LHS"]))) %>%
  mutate(strata = 6)
strata7 <- data2017 %>%
  filter(eval(parse(text = rules.svy[7, "LHS"]))) %>%
  mutate(strata = 7)
strata8 <- data2017 %>%
  filter(eval(parse(text = rules.svy[8, "LHS"]))) %>%
  mutate(strata = 8)
strata9 <- data2017 %>%
  filter(eval(parse(text = rules.svy[9, "LHS"]))) %>%
  mutate(strata = 9)

data2017 <- rbind(strata1, strata2, strata3, strata4,
                  strata5, strata6, strata7, strata8,
                  strata9)

data2017$strata <- as.factor(data2017$strata)
table(data2017$strata)


# --------------------------------------------------------
# 3.1.2 Store splitting rules to build Table 1
# --------------------------------------------------------

Table1 <- file("Tables/Table1.txt")
sink(Table1, type = "output")
print(rules.svy)
sink()
close(Table1)
