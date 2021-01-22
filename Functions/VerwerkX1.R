#################################################################################
#
# Script voor prepareren en koppelen standaard registervariabelen
#
#  Versie: v1 = 22-01-2016
#
#################################################################################

VerwerkX1 = function(projectmap,Jaar,Mnd,stpdata,statdata, hh){

  LftKlas13 = function(Leeftijd){ 
    Lft13 = "-1. 0-100"
    if (Leeftijd<12)  Lft13 = "01. <12"
    if ((Leeftijd>=12) & (Leeftijd<18)) Lft13 = "02. 12-17"
    if ((Leeftijd>=18) & (Leeftijd<25)) Lft13 = "03. 18-24"
    if ((Leeftijd>=25) & (Leeftijd<30)) Lft13 = "04. 25-29"
    if ((Leeftijd>=30) & (Leeftijd<35)) Lft13 = "05. 30-34"
    if ((Leeftijd>=35) & (Leeftijd<40)) Lft13 = "06. 35-39"
    if ((Leeftijd>=40) & (Leeftijd<45)) Lft13 = "07. 40-44"
    if ((Leeftijd>=45) & (Leeftijd<50)) Lft13 = "08. 45-49"
    if ((Leeftijd>=50) & (Leeftijd<55)) Lft13 = "09. 50-54"
    if ((Leeftijd>=55) & (Leeftijd<60)) Lft13 = "10. 55-59"
    if ((Leeftijd>=60) & (Leeftijd<65)) Lft13 = "11. 60-64"
    if ((Leeftijd>=65) & (Leeftijd<70)) Lft13 = "12. 65-69"
    if (Leeftijd>=70)  Lft13 = "13. >=70"
    
    Lft13
  }
  
LftKlas15 = function(Leeftijd){ 
  Lft15 = "-1. 0-100"
  if (Leeftijd<4)  Lft15 = "01. <4"
  if ((Leeftijd>=4) & (Leeftijd<12)) Lft15 = "02. 4-11"
  if ((Leeftijd>=12) & (Leeftijd<18)) Lft15 = "03. 12-17"
  if ((Leeftijd>=18) & (Leeftijd<25)) Lft15 = "04. 18-24"
  if ((Leeftijd>=25) & (Leeftijd<30)) Lft15 = "05. 25-29"
  if ((Leeftijd>=30) & (Leeftijd<35)) Lft15 = "06. 30-34"
  if ((Leeftijd>=35) & (Leeftijd<40)) Lft15 = "07. 35-39"
  if ((Leeftijd>=40) & (Leeftijd<45)) Lft15 = "08. 40-44"
  if ((Leeftijd>=45) & (Leeftijd<50)) Lft15 = "09. 45-49"
  if ((Leeftijd>=50) & (Leeftijd<55)) Lft15 = "10. 50-54"
  if ((Leeftijd>=55) & (Leeftijd<60)) Lft15 = "11. 55-59"
  if ((Leeftijd>=60) & (Leeftijd<65)) Lft15 = "12. 60-64"
  if ((Leeftijd>=65) & (Leeftijd<70)) Lft15 = "13. 65-69"
  if ((Leeftijd>=70) & (Leeftijd<75)) Lft15 = "14. 70-74"
  if (Leeftijd>=75)  Lft15 = "15. >=75"
  
  Lft15
}

Generatie = function(Herkomst,Gebland){
  if (Herkomst=="0") Gen = "Autochtoon"                          # Autochtoon
  if ((Herkomst!="0") & Gebland == 6030) Gen = "Eerste generatie"      # Eerste generatie
  if ((Herkomst!="0") & Gebland != 6030) Gen = "Tweede generatie"      # Tweede generatie
  
  Gen
}

HerkomstGeneratie = function(Herk,Gen){
  if (Herk=="0") herkgen = "Autochtoon"                                                                                                               # Autochtoon
  if (((Herk=="1") | (Herk=="2") | (Herk=="3") | (Herk=="4") | (Herk=="5") ) & (Gen=="Eerste generatie")) herkgen = "Eerste generatie niet-Westers"    # 1e generatie niet-Westers
  if (((Herk=="1") | (Herk=="2") | (Herk=="3") | (Herk=="4") | (Herk=="5") ) & (Gen=="Tweede generatie")) herkgen = "Tweede generatie niet-westers"    # 2e generatie niet-westers
  if ((Herk=="6") & (Gen=="Eerste generatie")) herkgen = "Eerste generatie westers"                                                                    # 1e generatie westers
  if ((Herk=="6") & (Gen=="Tweede generatie")) herkgen = "Tweede generatie westers"                                                                    # 2e generatie westers
  
  herkgen
}

Herkomst = function(herkomst){ 
  if (herkomst=="0") herkomstr = "0. Autochtoon"                                                                                               # Autochtoon
  if (herkomst=="1") herkomstr = "1. Marokko"
  if (herkomst=="2") herkomstr = "2. Turkije" 
  if (herkomst=="3") herkomstr = "3. Suriname" 
  if (herkomst=="4") herkomstr = "4. Nederlandse Antillen en Aruba" 
  if (herkomst=="5") herkomstr = "5. Overige niet-Westerse landen" 
  if (herkomst=="6") herkomstr = "6. Overig Westerse landen"
  if (herkomst=="7") herkomstr = "7. Onbekend"
  
  
  herkomstr
}

BurgerlijkeStaat=function(burgstaat){
  if (burgstaat=="1") burgstaatr="1. Ongehuwd"
  if (burgstaat=="2"|burgstaat=="5") burgstaatr="2. Gehuwd of Partnerschap"
  if (burgstaat=="3"|burgstaat=="6") burgstaatr="3. Verweduwd of Verweduwd na partnerschap"
  if (burgstaat=="4"|burgstaat=="7") burgstaatr="4. Gescheiden of Gescheiden na partnerschap"
  burgstaatr
  
}

# InkKlas6 = function(Inkomen){
#   if (is.na(Inkomen)) Ink6="0. Missing"
#   else {
#     if (Inkomen<=500) Ink6 = "1. <=500"
#     if ((Inkomen>500) & (Inkomen<=1000)) Ink6 = "2. 500-1000"
#     if ((Inkomen>1000) & (Inkomen<=2000)) Ink6 = "3. 1000-2000"
#     if ((Inkomen>2000) & (Inkomen<=3000)) Ink6 = "4. 2000-3000"
#     if (Inkomen>3000) Ink6 = "5. >3000"
#   }
#   
#   Ink6
# }


InkKlas6 = function(Inkomen){
  if (is.na(Inkomen)) Ink6="0. Missing"
  else {
    if (Inkomen<=821) Ink6 = "1. 0-20% perc"
    if ((Inkomen>821) & (Inkomen<=1461)) Ink6 = "2. 20-40% perc"
    if ((Inkomen>1461) & (Inkomen<=2246)) Ink6 = "3. 40-60% perc"
    if ((Inkomen>2246) & (Inkomen<=3396)) Ink6 = "4. 60-80% perc"
    if (Inkomen>3396) Ink6 = "5. 80-100% perc"
  }
  
  Ink6
}

# OMZETTEN DATA TYPEN
stpdata$RINPersoon = as.numeric(stpdata$RINPersoon)
stpdata$Geboortejaar = as.numeric(stpdata$Geboortejaar)
stpdata$Geboortemaand = as.numeric(stpdata$Geboortemaand)
stpdata$Gemeentecode = as.numeric(stpdata$Gemeentecode)
stpdata$Geslacht =  as.factor(stpdata$Geslacht)
stpdata$BurgerlijkeStaat = as.factor(stpdata$BurgerlijkeStaat)
stpdata$Geboorteland = as.numeric(stpdata$Geboorteland)
stpdata$GeboortelandMoeder = as.numeric(stpdata$GeboortelandMoeder)
stpdata$GeboortelandVader = as.numeric(stpdata$GeboortelandVader)
stpdata$Herkomst = as.factor(stpdata$Herkomst)

# AFLEIDEN LEEFTIJD EN LEEFTIJDKLASSEN

stpdata$Leeftijd = Jaar - stpdata$Geboortejaar - as.numeric(stpdata$Geboortemaand > Mnd)
stpdata$Leeftijdsklasse = as.factor(apply(array(stpdata$Leeftijd),1,LftKlas15))
stpdata$Leeftijdsklasse13 = as.factor(apply(array(stpdata$Leeftijd),1,LftKlas13))

# AFLEIDEN HERKOMSTGENERATIE EN GECOMBINEERDE VARIABELE

stpdata$Generatie = "test"
stpdata$HerkomstGeneratie = "test"
#stpdata$Herkomst = "test"
#stpdata$BurgerlijkeStaat= "test"

for (i in 1:length(stpdata$Herkomst))
{
   stpdata$Generatie[i] = Generatie(stpdata$Herkomst[i],stpdata$Geboorteland[i])
   stpdata$HerkomstGeneratie[i] = HerkomstGeneratie(stpdata$Herkomst[i],stpdata$Generatie[i])
      
}
for (i in 1:length(stpdata$Herkomst))
{
stpdata$Herkomst2[i] = Herkomst(stpdata$Herkomst[i])
stpdata$BurgerlijkeStaat2[i]=BurgerlijkeStaat(stpdata$BurgerlijkeStaat[i])
}

stpdata$Generatie = as.factor(stpdata$Generatie)
stpdata$HerkomstGeneratie = as.factor(stpdata$HerkomstGeneratie)
stpdata$Herkomst = as.factor(stpdata$Herkomst2)
stpdata$BurgerlijkeStaat = as.factor(stpdata$BurgerlijkeStaat2)

# INVOEGEN STEDELIJKHEID
# data_regio<-read.csv(paste(projectmap,"/Beheer/Syntax/Regio/Regiocoderingen_",Jaar,".csv",sep=""),sep=";")
data_regio<-read.csv(paste(projectmap,"/Beheer/Syntax/Regio/Regiocoderingen_2015.csv",sep=""),sep=";")
sleutel_regio = match(stpdata$Gemeentecode,data_regio$Gemeentecode)
stpdata$Gemeente = data_regio[sleutel_regio,"Gemeente"]
stpdata$Provincie = data_regio[sleutel_regio,"Provincie"]
stpdata$Stedelijkheid = data_regio[sleutel_regio,"Stedelijkheid"]

# AFLEIDEN INKOMENKLASSEN
stpdata$Inkomen = stpdata$InkomenLoon + stpdata$InkomenUitkering

stpdata$Inkomensklasse = as.factor(apply(array(stpdata$Inkomen),1,InkKlas6))

# KOPPELEN
if (!hh) {
  statdata$RINPersoon = as.numeric(statdata$RINPersoon)
  sleutel = match(statdata$RINPersoon,stpdata$RINPersoon)
  statdata$Geslacht = stpdata[sleutel,"Geslacht"]
  statdata$BurgerlijkeStaat = stpdata[sleutel,"BurgerlijkeStaat"]
  #statdata$BurgerlijkeStaat = stpdata[sleutel,"BurgerlijkeStaat"]
  statdata$Herkomst = stpdata[sleutel,"Herkomst"]
  #statdata$Herkomst2 = stpdata[sleutel,"Herkomst2"]
  statdata$Generatie = stpdata[sleutel,"Generatie"]
  statdata$HerkomstGeneratie = stpdata[sleutel,"HerkomstGeneratie"]
  statdata$Stedelijkheid = stpdata[sleutel,"Stedelijkheid"]
  statdata$Provincie = stpdata[sleutel,"Provincie"]
  statdata$Inkomen = stpdata[sleutel,"Inkomen"]
  statdata$Inkomensklasse = stpdata[sleutel,"Inkomensklasse"]
  statdata$Leeftijd = stpdata[sleutel,"Leeftijd"]
  statdata$Leeftijdsklasse = stpdata[sleutel,"Leeftijdsklasse"]
  statdata$Leeftijdsklasse13 = stpdata[sleutel,"Leeftijdsklasse13"]
} else {  
  misstp = sum(as.numeric(is.na(match(statdata$RINAdres,stpdata$RINAdres))))
  misstat = sum(as.numeric(is.na(match(stpdata$RINAdres,statdata$RINAdres))))
  if (misstp>0) { cat("Er zijn ",misstp,"adressen zonder rinpersonen! Deze worden verwijderd.\n") }
  if (misstat>0) { cat("Er zijn ",misstat,"steekproefrinpersonen zonder veldwerkstatus!\n") }

  sleutel = match(stpdata$RINAdres,statdata$RINAdres)
  statdata = statdata[sleutel,]
  statdata$RINPersoon = stpdata$RINPersoon
  statdata$Geslacht = stpdata$Geslacht
  statdata$BurgerlijkeStaat = stpdata$BurgerlijkeStaat
  #statdata$BurgerlijkeStaat = stpdata$BurgerlijkeStaat
  statdata$Herkomst = stpdata$Herkomst
  #statdata$Herkomst2 = stpdata$Herkomst2
  statdata$Generatie = stpdata$Generatie
  statdata$HerkomstGeneratie = stpdata$HerkomstGeneratie
  statdata$Stedelijkheid = stpdata$Stedelijkheid
  statdata$Provincie = stpdata$Provincie
  statdata$Inkomen = stpdata$Inkomen
  statdata$Inkomensklasse = stpdata$Inkomensklasse
  statdata$Leeftijd = stpdata$Leeftijd
  statdata$Leeftijdsklasse = stpdata$Leeftijdsklasse
  statdata$Leeftijdsklasse13 = stpdata$Leeftijdsklasse13
  statdata = statdata[(!is.na(statdata$RINAdres)),]
}
statdata$Stedelijk=statdata$Stedelijkheid
cat("Stedelijkheid ontbreekt voor",nrow(statdata[is.na(statdata$Stedelijk),]),"adressen!\n")
statdata = statdata[!is.na(statdata$Stedelijk),]

statdata
}