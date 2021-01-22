
##########################################################
#
# 5. Optimization of quality and cost indicators
#
#    Updated on 20/01/2021
#
##########################################################

## evaluate the performance of all strategies
solution.assess <- summaryIndicator(CV = samples$cv.response, RR = samples$response.rate, 
                                    allocation = allocation.matrix)

## optimization based on RESPONSE RATES
solution.assess$statistics[order(solution.assess$statistics[, "RR>=.6"]),]