
######################################################
#
# 4. Estimate design parameters
#
#    Updated on 29/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file estimates design parameters.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# -----------------------------------------------
# 4.1 Estimate response propensities per stratum and strategy
# -----------------------------------------------

samples.respY <- DesignParaEst(data = data2017, strata = data2017$strata_respY,
                               n.iter = 5000, n.burn = 1000)
samples.visitsY <- DesignParaEst(data = data2017, strata = data2017$strata_visitsY,
                                 n.iter = 5000, n.burn = 1000)

## run two chains for figure C2
samples.respY.2chains <- DesignParaEst(data = data2017, strata = data2017$strata_respY,
                                       n.iter = 5000, n.burn = 1000, n.chain = 2)
samples.visitsY.2chains <- DesignParaEst(data = data2017, strata = data2017$strata_visitsY,
                                         n.iter = 5000, n.burn = 1000, n.chain = 2)

## for table 2
prop.est.respY <- RespPropensity(data2017, "strata_respY",
                                 samples.respY$p.g.s1, samples.respY$p.g.s2, samples.respY$p.g.s3)
prop.est.visitsY <- RespPropensity(data2017, "strata_visitsY",
                                   samples.visitsY$p.g.s1, samples.visitsY$p.g.s2, samples.visitsY$p.g.s3)



