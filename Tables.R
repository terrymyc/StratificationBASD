
######################################################
#
#  Tables
#
#  Updated on 24/08/2021
#
######################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file generates all tables shown in the manuscript.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# --------------------------------------------------------
# Table 1
# --------------------------------------------------------

Table1 <- file("Tables/Table1_respY.txt")
sink(Table1, type = "output")
print(rules.respY)
sink()
close(Table1)

Table1 <- file("Tables/Table1_visitY.txt")
sink(Table1, type = "output")
print(rules.visitsY)
sink()
close(Table1)

Table1 <- file("Tables/Table1_respX.txt")
sink(Table1, type = "output")
print(rules.respX)
sink()
close(Table1)

Table1 <- file("Tables/Table1_visitX.txt")
sink(Table1, type = "output")
print(rules.visitsX)
sink()
close(Table1)

# Table1 <- file("Tables/Table1_costX.txt")
# sink(Table1, type = "output")
# print(rules.costX)
# sink()
# close(Table1)


# --------------------------------------------------------
# Table 2
# --------------------------------------------------------

Table2 <- file("Tables/Table2_respY.txt")
sink(Table2, type = "output")
print(prop.est.respY)
sink()
close(Table2)

Table2 <- file("Tables/Table2_visitsY.txt")
sink(Table2, type = "output")
print(prop.est.visitsY)
sink()
close(Table2)

# Table2 <- file("Tables/Table2_respX.txt")
# sink(Table2, type = "output")
# print(prop.est.respX)
# sink()
# close(Table2)

# Table2 <- file("Tables/Table2_visitX.txt")
# sink(Table2, type = "output")
# print(prop.est.visitsX)
# sink()
# close(Table2)

# Table2 <- file("Tables/Table2_costX.txt")
# sink(Table2, type = "output")
# print(prop.est.costX)
# sink()
# close(Table2)

# ----------------------------------------------------------
# Table 3
# ----------------------------------------------------------

Table3 <- file("Tables/Table3_respY.txt")
sink(Table3, type = "output")
print(optimized.respY[1:5, ])
sink()
close(Table3)

Table3 <- file("Tables/Table3_visitY.txt")
sink(Table3, type = "output")
print(optimized.visitsY[1:5, ])
sink()
close(Table3)

Table3 <- file("Tables/Table3_respX.txt")
sink(Table3, type = "output")
print(optimized.respX[1:5, ])
sink()
close(Table3)

Table3 <- file("Tables/Table3_visitX.txt")
sink(Table3, type = "output")
print(optimized.visitsX[1:5, ])
sink()
close(Table3)

Table3 <- file("Tables/Table3_costX.txt")
sink(Table3, type = "output")
print(optimized.costX[1:5, ])
sink()
close(Table3)

# --------------------------------------------------------
# Table B.1
# --------------------------------------------------------

TableB1 <- file("Tables/TableB1.txt")
sink(TableB1, type = "output")
print(MCMCsummary(as.matrix(res.health), Rhat = F, n.eff = F, digits = 3))
print(MCMCsummary(as.matrix(res.smoke), Rhat = F, n.eff = F, digits = 3))
print(MCMCsummary(as.matrix(res.health), Rhat = F, n.eff = F, digits = 3))
sink()
close(TableB1)


