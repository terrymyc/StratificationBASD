
######################################################
#
#  Figures
#
#  Updated on 29/07/2021
#
######################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file generates all figures shown in the manuscript.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ----------------------------------------------------------
# Figure 1 Estimated response propensity per stratum and strategy
# ----------------------------------------------------------

figure1.prop.est(prop.est = prop.est.respY, n.strata = 6, method = "respY")
figure1.prop.est(prop.est = prop.est.visitsY, n.strata = 8, method = "visitsY")



# ----------------------------------------------------------
# Figure 2 CV criterion of individual response propensities
# ----------------------------------------------------------

figure2(CV.realized = 0.12019257, CV)



# ----------------------------------------------------------
# Figure 3 CV of stratum response propensities of top 100 solutions
# ----------------------------------------------------------

figure3(design = optimal.respY.top100)



# ------------------------------------------------------------
# Figure C.1 Trace plot for survey variable model parameters
# ------------------------------------------------------------

figureC1(samples.health, samples.smoke, samples.obese)  # no return


# ------------------------------------------------------------
# Figure C.2 Trace plot for survey design parameter model parameters
# Designed for visitsY
# ------------------------------------------------------------

figureC2(samples.visitsY.2chains)  # no return



# ------------------------------------------------------------
# Figure C.3 Trace plot for stratification-assessed propensity model parameters
# ------------------------------------------------------------

figureC3(stratification.eva.respY.2chains)  # no return
figureC3(stratification.eva.visitsY.2chains)  # no return
