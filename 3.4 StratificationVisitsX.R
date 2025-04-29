
######################################################
#
# 3.2 VisitsX Stratification
#
#     Updated on 13/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file applies classification tree algorithm to
# explain the number of visits.

# The inputs are auxiliary variables linked from
# administrative data.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# 3.2.1 Stratification based on auxiliary variables
# --------------------------------------------------------

## perform classification tree algorithm
set.seed(12345)
tree.visitsX <- rpart(aantalBezoeken ~ Leeftijdsklasse12 + Geslacht + Inkomensklasse +
                    BurgerlijkeStaat + Stedelijkheid + HerkomstGeneratie +
                    TypeHH + OplNivHB + InschrCWI,
                  data = data2017, method = "anova", 
                  control = rpart.control(cp = 0.001, minsplit = 500))

## print complex parameters
printcp(tree.visitsX)
plotcp(tree.visitsX)

## prune the tree
tree.visitsX <- prune(tree.visitsX, cp = 0.0013)

## extract splitting rules
rules.visitsX <- tidyRules(tree.visitsX)

## create stratum indicator variable
data2017 <- data2017 %>%
  mutate(strata_visitsX = ifelse(eval(parse(text = rules.visitsX[1, "LHS"])), 1, 
                               ifelse(eval(parse(text = rules.visitsX[2, "LHS"])), 2,
                                      ifelse(eval(parse(text = rules.visitsX[3, "LHS"])), 3,
                                             ifelse(eval(parse(text = rules.visitsX[4, "LHS"])), 4,
                                                    ifelse(eval(parse(text = rules.visitsX[5, "LHS"])), 5,
                                                           ifelse(eval(parse(text = rules.visitsX[6, "LHS"])), 6, 7)))))))

data2017$strata_visitsX <- as.factor(data2017$strata_visitsX)
table(data2017$strata_visitsX)


## true response rates in each phase
tapply(data2017$FinalRespPhase1, data2017$strata_visitsX, mean)
tapply(data2017$FinalRespPhase2, data2017$strata_visitsX, mean)
tapply(data2017$FinalRespPhase3, data2017$strata_visitsX, mean)
