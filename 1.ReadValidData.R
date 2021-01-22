
###################################################
#
# 1. Read data & Recode key survey variables
#
#    Updated on 20/01/2021
#
###################################################


# -------------------------------------------
# Read data 04/2017-03/2018
# -------------------------------------------
data38 <- ReadGEZOCAWICAPI(Y, 2017, 4, "GEZO","CAPI","4 - April")
data39 <- ReadGEZOCAWICAPI(Y, 2017, 5, "GEZO","CAPI","5 - Mei")
data40 <- ReadGEZOCAWICAPI(Y, 2017, 6, "GEZO","CAPI","6 - Juni")
data41 <- ReadGEZOCAWICAPI(Y, 2017, 7, "GEZO","CAPI","7 - Juli")
data42 <- ReadGEZOCAWICAPI(Y, 2017, 8, "GEZO","CAPI","8 - Augustus")
data43 <- ReadGEZOCAWICAPI(Y, 2017, 9, "GEZO", "CAPI", "9 - September")
data44 <- ReadGEZOCAWICAPI(Y, 2017, 10, "GEZO","CAPI","10 - Oktober")
data45 <- ReadGEZOCAWICAPI(Y, 2017, 11, "GEZO","CAPI","11 - November")
data46 <- ReadGEZOCAWICAPI(Y, 2017, 12, "GEZO","CAPI","12 - December")
#data47 <- ReadGEZOCAWICAPI(Y, 2018, 1, "GEZO","CAPI","1 - Januari")
#data48 <- ReadGEZOCAWICAPI(Y, 2018, 2, "GEZO","CAPI","2 - Februari")
#data49 <- ReadGEZOCAWICAPI(Y, 2018, 3, "GEZO","CAPI","3 - Maart")

# merge monthly data
data2017 <- rbind(data38, data39, data40, data41, data42, data43, 
                  data44, data45, data46)


# -------------------------------------------
# Recode key survey variables
# -------------------------------------------
# General health
data2017$Health <- ifelse(data2017$AlgGezon < 3, 1, 0)
data2017$Health <- factor(data2017$Health, levels = c(0, 1),
                          labels = c("unhealthy", "healthy"))
table(data2017$Health)

# Smoke
data2017$Smoke <- ifelse(data2017$Rookt == 1, 1, 0)
data2017$Smoke <- factor(data2017$Smoke, levels = c(0, 1), 
                         labels = c("not smoke", "smoke"))
table(data2017$Smoke)

# Obese
## code NA of weight and height
data2017$Gewicht[data2017$Gewicht == 999 | data2017$Gewicht == 9998] <- NA
data2017$Lengte[data2017$Lengte == 999] <- NA

## calculate BMI
data2017$BMI <- round(data2017$Gewicht/(data2017$Lengte/100)^2, digits = 2)
summary(data2017$BMI)

## code extreme values into NA
### no BMI for age <= 1
data2017[data2017$Leeftijd <= 1, "BMI"] <- NA

### 10 <= BMI < 50 normal for 1 < age <= 19
na.index <- which(data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"] < 10 |
                    data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"] >= 50)
data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"][na.index] <- NA

### 14 <= BMI < 50 normal for age > 19
na.index <- which(data2017[data2017$Leeftijd > 19, "BMI"] < 14 | 
                    data2017[data2017$Leeftijd > 19, "BMI"] >= 50)
data2017[data2017$Leeftijd > 19, "BMI"][na.index] <- NA

summary(data2017$BMI)

## transform BMI into obesity
drempV <- c(19.55,19.23,19.12,19.34,20.08,21.01,22.18,23.46,24.77,26.05,27.24,28.20,28.87,29.29,29.56,29.84)
drempM <- c(19.80,19.39,19.26,19.47,20.23,21.09,22.17,23.39,24.57,25.58,26.43,27.25,27.98,28.60,29.14,29.70)
dremp <- matrix(c(drempM, drempV), 2, 16, byrow = T)

for (i in 1:nrow(data2017)) {
  sex <- ifelse(data2017[i, "Geslacht"] == "M", 1, 2)
  if (is.na(data2017[i, "BMI"])) {
    data2017[i, "Obese"] <- NA
  } else {
    if (data2017[i, "Leeftijd"] > 17 & data2017[i, "BMI"] >= 30) {
      data2017[i, "Obese"] <- 1
    } else if (data2017[i, "Leeftijd"] > 17 & data2017[i, "BMI"] < 30) {
      data2017[i, "Obese"] <- 0
    } else if (data2017[i, "Leeftijd"] <= 17 & data2017[i, "BMI"] > dremp[sex, max(data2017[i, "Leeftijd"]-1, 1)]) {
      data2017[i, "Obese"] <- 1
    } else {
      data2017[i, "Obese"] <- 0
    }
  }
}

data2017$Obese <- factor(data2017$Obese, levels = c(0, 1), 
                         labels = c("not obese", "obese"))
table(data2017$Obese)
