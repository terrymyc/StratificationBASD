
######################################################
#
# 3.2 ResponseX Stratification
#
#     Updated on 13/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the final response outcome.

# The inputs are auxiliary variables linked from
# administrative data.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# 3.2.1 Stratification based on auxiliary variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(12345)
tree.respX <- rpart(FinalRespPhase3 ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse +
                    BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                    TypeHH + OplNivHB + InschrCWI,
                  data = data2017, method = "class", 
                  control = rpart.control(cp = 0.001, minsplit = 500))


## print complex parameters
printcp(tree.respX)
plotcp(tree.respX)

## prune the tree
tree.respX <- prune(tree.respX, cp = 0.0017)

## extract splitting rules
rules.respX <- tidyRules(tree.respX)

## create stratum indicator variable
data2017 <- data2017 %>%
  mutate(strata_respX = ifelse(eval(parse(text = rules.respX[1, "LHS"])), 1, 
                               ifelse(eval(parse(text = rules.respX[2, "LHS"])), 2,
                                      ifelse(eval(parse(text = rules.respX[3, "LHS"])), 3,
                                             ifelse(eval(parse(text = rules.respX[4, "LHS"])), 4, 5)))))

data2017$strata_respX <- as.factor(data2017$strata_respX)
table(data2017$strata_respX)


## true response rates at the end of each phase
tapply(data2017$FinalRespPhase1, data2017$strata_respX, mean)
tapply(data2017$FinalRespPhase2, data2017$strata_respX, mean)
tapply(data2017$FinalRespPhase3, data2017$strata_respX, mean)
