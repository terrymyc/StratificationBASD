
######################################################
#
# 6. Sensitivity analysis of optimal designs
#
#    Updated on 30/07/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Description

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n.g <- prop.est.respY$strata.size[1, ]

# extract intervals
s1.interval <- matrix(NA, nrow = 2, ncol = 6)
s1.interval[1, ] <- prop.est.respY$quantiles.s1[, 1]
s1.interval[2, ] <- prop.est.respY$quantiles.s1[, 5]

s2.interval <- matrix(NA, nrow = 2, ncol = 6)
s2.interval[1, ] <- prop.est.respY$quantiles.s2[, 1]
s2.interval[2, ] <- prop.est.respY$quantiles.s2[, 5]

s3.interval <- matrix(NA, nrow = 2, ncol = 6)
s3.interval[1, ] <- prop.est.respY$quantiles.s3[, 1]
s3.interval[2, ] <- prop.est.respY$quantiles.s3[, 5]

# 64 scenarios per design
scen <- as.matrix(expand.grid(rep(list(1:2), 6)))

# Design 1 scenario
d1.scen.prop <- matrix(NA, nrow = 64, ncol = 6)
d1.scen.CV <- rep(NA, 64)
for (i in 1:64) {
  d1.scen.prop[i, 1] <- s3.interval[scen[i, 1], 1]
  d1.scen.prop[i, 2] <- s3.interval[scen[i, 2], 2]
  d1.scen.prop[i, 3] <- s2.interval[scen[i, 3], 3]
  d1.scen.prop[i, 4] <- s2.interval[scen[i, 4], 4]
  d1.scen.prop[i, 5] <- s3.interval[scen[i, 5], 5]
  d1.scen.prop[i, 6] <- s1.interval[scen[i, 6], 6]
  rho <- sum(n.g * d1.scen.prop[i, ])/sum(n.g)
  var.rho <- sum(n.g * (d1.scen.prop[i, ] - rho)^2)/sum(n.g)
  d1.scen.CV[i] <- sqrt(var.rho)/rho
}
summary(d1.scen.CV)

# Design 2 scenario
d2.scen.prop <- matrix(NA, nrow = 64, ncol = 6)
d2.scen.CV <- rep(NA, 64)
for (i in 1:64) {
  d2.scen.prop[i, 1] <- s3.interval[scen[i, 1], 1]
  d2.scen.prop[i, 2] <- s3.interval[scen[i, 2], 2]
  d2.scen.prop[i, 3] <- s3.interval[scen[i, 3], 3]
  d2.scen.prop[i, 4] <- s2.interval[scen[i, 4], 4]
  d2.scen.prop[i, 5] <- s3.interval[scen[i, 5], 5]
  d2.scen.prop[i, 6] <- s1.interval[scen[i, 6], 6]
  rho <- sum(n.g * d2.scen.prop[i, ])/sum(n.g)
  var.rho <- sum(n.g * (d2.scen.prop[i, ] - rho)^2)/sum(n.g)
  d2.scen.CV[i] <- sqrt(var.rho)/rho
}
summary(d2.scen.CV)

# Design 3 scenario
d3.scen.prop <- matrix(NA, nrow = 64, ncol = 6)
d3.scen.CV <- rep(NA, 64)
for (i in 1:64) {
  d3.scen.prop[i, 1] <- s3.interval[scen[i, 1], 1]
  d3.scen.prop[i, 2] <- s3.interval[scen[i, 2], 2]
  d3.scen.prop[i, 3] <- s3.interval[scen[i, 3], 3]
  d3.scen.prop[i, 4] <- s3.interval[scen[i, 4], 4]
  d3.scen.prop[i, 5] <- s3.interval[scen[i, 5], 5]
  d3.scen.prop[i, 6] <- s1.interval[scen[i, 6], 6]
  rho <- sum(n.g * d3.scen.prop[i, ])/sum(n.g)
  var.rho <- sum(n.g * (d3.scen.prop[i, ] - rho)^2)/sum(n.g)
  d3.scen.CV[i] <- sqrt(var.rho)/rho
}
summary(d3.scen.CV)


# Design 4 scenario
d4.scen.prop <- matrix(NA, nrow = 64, ncol = 6)
d4.scen.CV <- rep(NA, 64)
for (i in 1:64) {
  d4.scen.prop[i, 1] <- s3.interval[scen[i, 1], 1]
  d4.scen.prop[i, 2] <- s3.interval[scen[i, 2], 2]
  d4.scen.prop[i, 3] <- s2.interval[scen[i, 3], 3]
  d4.scen.prop[i, 4] <- s3.interval[scen[i, 4], 4]
  d4.scen.prop[i, 5] <- s3.interval[scen[i, 5], 5]
  d4.scen.prop[i, 6] <- s1.interval[scen[i, 6], 6]
  rho <- sum(n.g * d4.scen.prop[i, ])/sum(n.g)
  var.rho <- sum(n.g * (d4.scen.prop[i, ] - rho)^2)/sum(n.g)
  d4.scen.CV[i] <- sqrt(var.rho)/rho
}
summary(d4.scen.CV)


# Design 5 scenario
d5.scen.prop <- matrix(NA, nrow = 64, ncol = 6)
d5.scen.CV <- rep(NA, 64)
for (i in 1:64) {
  d5.scen.prop[i, 1] <- s3.interval[scen[i, 1], 1]
  d5.scen.prop[i, 2] <- s2.interval[scen[i, 2], 2]
  d5.scen.prop[i, 3] <- s3.interval[scen[i, 3], 3]
  d5.scen.prop[i, 4] <- s2.interval[scen[i, 4], 4]
  d5.scen.prop[i, 5] <- s3.interval[scen[i, 5], 5]
  d5.scen.prop[i, 6] <- s1.interval[scen[i, 6], 6]
  rho <- sum(n.g * d5.scen.prop[i, ])/sum(n.g)
  var.rho <- sum(n.g * (d5.scen.prop[i, ] - rho)^2)/sum(n.g)
  d5.scen.CV[i] <- sqrt(var.rho)/rho
}
summary(d5.scen.CV)

# plot CV of 5 designs
scen.CV <- cbind(d1.scen.CV, d2.scen.CV, d3.scen.CV, d4.scen.CV, d5.scen.CV)
colnames(scen.CV) <- c("Design 1", "Design 2", "Design 3", "Design 4", "Design 5")

scen.CV.long <- melt(scen.CV)
colnames(scen.CV.long) <- c("id", "Design", "CV")

ggplot(scen.CV.long, aes(x = Design, y = CV)) +
  geom_boxplot()
