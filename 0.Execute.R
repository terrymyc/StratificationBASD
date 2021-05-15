
####################################################
#
# 0. Step-by-step Guide for Reproducing the Research
#
#    (SECTION 5 in manuscript)
#
#    Updated on 10/05/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file provides a step-by-step to reproduce the
# case study on Dutch Health Survey as described in
# manuscript section 5.

# Tables, figures, and output to determine optimal 
# stratification will be generated in corresponding folders.

# 0.1 Install all required dependencies.
# 0.2 Import self-defined functions.
# 0.3 Reproduce case study results.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ------------------------------------
# 0.1 Dependencies
# ------------------------------------

install.packages("LaF")
install.packages("dplyr")
install.packages("rpart")
install.packages("tidyrules")
install.packages("mvtnorm")
install.packages("truncnorm")
install.packages("MCMCpack")
install.packages("fastDummies")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("latex2exp")


# -----------------------------------
# 0.2 Functions
# -----------------------------------

source("Functions/KostenN.R")
source("Functions/VerwerkX1.R")
source("Functions/VerwerkX2.R")
source("Functions/PasVarNamenAan.R")
source("Functions/ReadGEZOfiles.R")
source("Functions/GibbsSampler.R")
source("Functions/Bayesian.R")


# -----------------------------------
# 0.3 Step-by-step reproducible workflow
# -----------------------------------

## Read and recode raw data into analysis data
source("1.ReadRecodeRawData.R")

## Predict key survey variables
source("2.BayesModelSvyVar.R")  ## Table B.1, Figure C.1 generated

## ResponseY stratification
## Running time more than 4 HOURS
source("3.1 StratificationResponseY.R")  ## Table 1 generated
source("4.DesignPara.R")  ## Table 2, Figure 1, Figure C.2 generated
source("5.Optimization.R")  ## Table 3, Figure 3, Figure C.3 generated

## ResponseX stratification
## Running time more than 5 HOURS
source("3.2 StratificationResponseX.R")
source("4.DesignPara.R")
source("5.Optimization.R")

## CostX stratification
## Running time more than 0.5 HOURS
source("3.3 StratificationCostX.R")
source("4.DesignPara.R")
source("5.Optimization.R")

## Determine optimal stratification
source("6.OptimalStratification.R")  # Figure 2 generated
