
####################################################
#
# Step-by-step Guide for Reproducing the Research
#
# Updated on 20/01/2021
#
####################################################

# ------------------------------------
# Dependencies
# ------------------------------------
install.packages("LaF")
install.packages("dplyr")
install.packages("MASS")
install.packages("gld")
install.packages("Exact")
install.packages("DescTools")
install.packages("ResourceSelection")
install.packages("stargazer")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("mvtnorm")
install.packages("truncnorm")

library(LaF)
Y="//cbsp.nl/Productie/Primair/PDCA-SEC/"


# -----------------------------------
# Functions
# -----------------------------------
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/KostenN.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/VerwerkX1.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/VerwerkX2.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/PasVarNamenAan.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/ReadGEZOfiles.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/Functions/Bayesian.R"))


# -----------------------------------
# Step-by-step reproducible workflow
# -----------------------------------
source(paste0(Y,"/Beheer/Terry/Stratification/1.ReadValidData.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/2.ModelSvyVar.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/3.Stratification.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/4.DesignPara.R"))
source(paste0(Y,"/Beheer/Terry/Stratification/5.Optimization.R"))
