
####################################################
#
# 0. Step-by-step Guide for Reproducing the Research
#
#    Updated on 29/07/2021
#
####################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file provides a step-by-step to reproduce the
# case study on Dutch Health Survey.

# Tables, figures, and output to determine optimal 
# stratification will be generated in corresponding folders.

# 0.1 Install all required dependencies.
# 0.2 Import self-defined functions.
# 0.3 Reproduce case study results.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# ------------------------------------
# 0.1 Dependencies
# ------------------------------------

install.packages("doParallel")
install.packages("dplyr")
install.packages("foreach")
install.packages("ggplot2")
install.packages("LaF")
install.packages("latex2exp")
install.packages("MCMCpack")
install.packages("MCMCvis")
install.packages("mvtnorm")
install.packages("parallel")
install.packages("reshape2")
install.packages("rpart")
install.packages("tidyrules")
install.packages("truncnorm")


library(doParallel)
library(dplyr)
library(foreach)
library(ggplot2)
library(latex2exp)
library(MCMCpack)
library(MCMCvis)
library(mvtnorm)
library(parallel)
library(reshape2)
library(rpart)
library(tidyrules)
library(truncnorm)



# -----------------------------------
# 0.2 Functions
# -----------------------------------

source("Functions/KostenN.R")
source("Functions/VerwerkX1.R")
source("Functions/VerwerkX2.R")
source("Functions/PasVarNamenAan.R")
source("Functions/ReadGEZOfiles.R")
source("Functions/GibbsSampler(MCMCpack).R")
source("Functions/GibbsSampleSummary.R")
source("Functions/FiguresGenerator.R")


cores <- detectCores(logical = F)
registerDoParallel(cores)
getDoParWorkers()


# -----------------------------------
# 0.3 Step-by-step reproducible workflow
# -----------------------------------

## Read and recode raw data into analysis data
source("1.ReadRecodeRawData.R")

## Predict key survey variables
source("2.BayesModelSvyVar.R")

## Stratification
source("3.1 StratificationResponseY.R")
source("3.2 StratificationVisitsY.R")
source("3.3 StratificationResponseX.R")
source("3.4 StratificationVisitsX.R")
source("3.5 StratificationCostX.R")

## Estimate design parameters
source("4.DesignPara.R")

## Optimization
source("5.Optimization.R")

## Tables generated
source("Tables/Tables.R")

## Figures generated
source("Figures/Figures.R")
