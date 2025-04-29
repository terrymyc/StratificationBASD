
######################################################
#
# 1. Read & Recode Raw Data
#
#    Updated on 10/05/2021
#
######################################################


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This file reads the raw Dutch Health Survey data 
# stored in CBS SEC environment and recodes the raw
# data into analysis data.

# Key survey variables are recoded as described in
# manuscript section 5.1.

# Three response outcomes and costs are recoded to 
# reflect three data collection phases described in 
# manuscript section 5.2.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# -------------------------------------------
# 1.1 Read data 04/2017-03/2018
# -------------------------------------------

Y="//cbsp.nl/Productie/Primair/PDCA-SEC/"
data38 <- ReadGEZOCAWICAPI(Y, 2017, 4, "GEZO","CAPI","4 - April")
data39 <- ReadGEZOCAWICAPI(Y, 2017, 5, "GEZO","CAPI","5 - Mei")
data40 <- ReadGEZOCAWICAPI(Y, 2017, 6, "GEZO","CAPI","6 - Juni")
data41 <- ReadGEZOCAWICAPI(Y, 2017, 7, "GEZO","CAPI","7 - Juli")
data42 <- ReadGEZOCAWICAPI(Y, 2017, 8, "GEZO","CAPI","8 - Augustus")
data43 <- ReadGEZOCAWICAPI(Y, 2017, 9, "GEZO", "CAPI", "9 - September")
data44 <- ReadGEZOCAWICAPI(Y, 2017, 10, "GEZO","CAPI","10 - Oktober")
data45 <- ReadGEZOCAWICAPI(Y, 2017, 11, "GEZO","CAPI","11 - November")
data46 <- ReadGEZOCAWICAPI(Y, 2017, 12, "GEZO","CAPI","12 - December")
data47 <- ReadGEZOCAWICAPI(Y, 2018, 1, "GEZO","CAPI","1 - Januari")
data48 <- ReadGEZOCAWICAPI(Y, 2018, 2, "GEZO","CAPI","2 - Februari")
data49 <- ReadGEZOCAWICAPI(Y, 2018, 3, "GEZO","CAPI","3 - Maart")

## merge monthly data
data2017 <- rbind(data38, data39, data40, data41, data42, data43, 
                  data44, data45, data46, data47, data48, data49)


# -------------------------------------------
# 1.2 Recode key survey variables
# -------------------------------------------

## General health
data2017$Health <- ifelse(data2017$AlgGezon < 3, 1, 0)
table(data2017$Health)

## Smoke
data2017$Smoke <- ifelse(data2017$Rookt == 1, 1, 0)
table(data2017$Smoke)

## Obese
### code NA of weight and height
data2017$Gewicht[data2017$Gewicht == 999 | data2017$Gewicht == 9998] <- NA
data2017$Lengte[data2017$Lengte == 999] <- NA

### calculate BMI
data2017$BMI <- round(data2017$Gewicht/(data2017$Lengte/100)^2, digits = 2)
summary(data2017$BMI)

### code extreme values into NA
#### no BMI for age <= 1
data2017[data2017$Leeftijd <= 1, "BMI"] <- NA

#### 10 <= BMI < 50 normal for 1 < age <= 19
na.index <- which(data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"] < 10 |
                    data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"] >= 50)
data2017[data2017$Leeftijd > 1 & data2017$Leeftijd <= 19, "BMI"][na.index] <- NA

#### 14 <= BMI < 50 normal for age > 19
na.index <- which(data2017[data2017$Leeftijd > 19, "BMI"] < 14 | 
                    data2017[data2017$Leeftijd > 19, "BMI"] >= 50)
data2017[data2017$Leeftijd > 19, "BMI"][na.index] <- NA

summary(data2017$BMI)

### transform BMI into obesity
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

table(data2017$Obese)


# -------------------------------------------
# 1.3 Recode response outcomes
# -------------------------------------------

## Questionable units
table(data2017$ModeRespons, data2017$aantalBezoeken)

## Correction
## 3 CAWI respondents have aantalBezoeken > 0  ==> CAPI-short nonresponse
## 5 CAPI respondents were not visited         ==> CAWI response
data2017[data2017$ModeRespons == "CAWI" & data2017$aantalBezoeken > 0, "ModeRespons"] <- "."
data2017[data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken == 0, "ModeRespons"] <- "CAWI"

## Indicator for response in each phase
### Phase 1: All units are approached by CAWI
data2017$ResponsePhase1 <- 1*(data2017$ModeRespons == "CAWI")
table(data2017$ResponsePhase1, data2017$aantalBezoeken)

### Phase 2: Phase 1 nonrespondents are followed up by CAPI-short
data2017$ResponsePhase2 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken <= 3, 1, 0)
data2017[data2017$ResponsePhase1 == 1, "ResponsePhase2"] <- NA
table(data2017$ResponsePhase2, data2017$aantalBezoeken)

### Phase 3: Phase 2 nonrespondents are followed up by CAPI-extended
data2017$ResponsePhase3 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken > 3, 1, 0)
data2017[data2017$aantalBezoeken <= 3, "ResponsePhase3"] <- NA
table(data2017$ResponsePhase3, data2017$aantalBezoeken)

## Indicator for final response at the end of each phase
### Phase 1: CAWI
data2017$FinalRespPhase1 <- 1*(data2017$ModeRespons == "CAWI")
table(data2017$FinalRespPhase1, data2017$aantalBezoeken)

### Phase 2: CAWI + CAPI-short
data2017$FinalRespPhase2 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken <= 3, 1, 0)
data2017[data2017$FinalRespPhase1 == 1, "FinalRespPhase2"] <- 1
table(data2017$FinalRespPhase2, data2017$aantalBezoeken)

### Phase 3: CAWI + CAPI-short + CAPI-extended
data2017$FinalRespPhase3 <- ifelse(data2017$ModeRespons == "CAPI" & data2017$aantalBezoeken > 3, 1, 0)
data2017[data2017$FinalRespPhase1 == 1 | data2017$FinalRespPhase2 == 1, "FinalRespPhase3"] <- 1
table(data2017$FinalRespPhase3, data2017$aantalBezoeken)


# -----------------------------------------
# 1.4 Recode costs
# -----------------------------------------

## For units entering phase 2: CAPI-short
data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 1, "kostenTotaalPhase2"] <- rowSums(
  cbind(data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 1, "kostenContactCAPI3"],
        data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 1, "kostenResponsCAPI"]), na.rm = T)
data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 0 & data2017$aantalBezoeken <= 3, "kostenTotaalPhase2"] <- rowSums(
  cbind(data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 0 & data2017$aantalBezoeken <= 3, "kostenContactCAPI3"],
        data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 0 & data2017$aantalBezoeken <= 3, "kostenNonresponsCAPI"]), na.rm = T)
data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 0 & data2017$aantalBezoeken > 3, "kostenTotaalPhase2"] <- rowSums(
  cbind(data2017[!is.na(data2017$ResponsePhase2) & data2017$ResponsePhase2 == 0 & data2017$aantalBezoeken > 3, "kostenContactCAPI3"]), na.rm = T)

summary(data2017$kostenTotaalPhase2)

## For units entering phase 3: CAPI-short + CAPI-extended cost
data2017[!is.na(data2017$ResponsePhase2), "kostenTotaalPhase23"] <- data2017[!is.na(data2017$ResponsePhase2), "kostenTotaalCAPI"]
summary(data2017$kostenTotaalPhase23)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#                                #
#   remove units with age < 12   #
#                                #  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

data2017$Leeftijdsklasse12 <- ifelse(data2017$Leeftijdsklasse13 != "01. <12",
                                     data2017$Leeftijdsklasse13, NA)
data2017$Leeftijdsklasse12 <- as.factor(data2017$Leeftijdsklasse12)
data2017 <- data2017[!is.na(data2017$Leeftijdsklasse12), ]
