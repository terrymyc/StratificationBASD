#################################################################################
#
# Script voor prepareren en koppelen standaard registervariabelen
#
#  Versie: v1 = 22-05-2017
#          v2 = 04-12-2017
#          v3 = 18-01-2018
#          v4 = 13-03-2018
#
#################################################################################

VerwerkX2 = function(Jaar,Mnd,statdata,hh){

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
  Lft15 = "-1. Onbekend"
  if (!(is.na(Leeftijd))) {
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
  }
  
  Lft15
}

HerkomstGeneratie = function(Herk,Gen){
  herkgen = "Onbekend"
  if ((!is.na(Herk)) & (!is.na(Gen))) {
  if (Herk==1) herkgen = "Autochtoon"                                                                                                               # Autochtoon
  if (((Herk>1) & (Herk<32)) & (Gen==1)) herkgen = "Eerste generatie niet-Westers"    # 1e generatie niet-Westers
  if (((Herk>0) & (Herk<32)) & (Gen==2)) herkgen = "Tweede generatie niet-westers"    # 2e generatie niet-westers
  if ((Herk==32) & (Gen==1)) herkgen = "Eerste generatie westers"                                                                    # 1e generatie westers
  if ((Herk==32) & (Gen==2)) herkgen = "Tweede generatie westers"                                                                    # 2e generatie westers
  }
  
  herkgen
}

Herkomst = function(herkomst){
  herkomstr = "7. Onbekend"
  if (!is.na(herkomst)){
  if (herkomst==1) herkomstr = "0. Autochtoon"                                                                                               # Autochtoon
  if (herkomst==2) herkomstr = "1. Marokko"
  if (herkomst==3) herkomstr = "2. Turkije" 
  if (herkomst==4) herkomstr = "3. Suriname" 
  if (herkomst==5) herkomstr = "4. Nederlandse Antillen en Aruba" 
  if (herkomst==25) herkomstr = "5. Overige niet-Westerse landen" 
  if (herkomst==32) herkomstr = "6. Overig Westerse landen"
  if (herkomst==0) herkomstr = "7. Onbekend"
  }
  
  herkomstr
}

InkKlas6 = function(Inkomen){
  if (is.na(Inkomen)) Ink6="0. Missing"
  else {
    if (Inkomen<=20) Ink6 = "1. 0-20% perc"
    if ((Inkomen>20) & (Inkomen<=40)) Ink6 = "2. 20-40% perc"
    if ((Inkomen>40) & (Inkomen<=60)) Ink6 = "3. 40-60% perc"
    if ((Inkomen>60) & (Inkomen<=80)) Ink6 = "4. 60-80% perc"
    if (Inkomen>80) Ink6 = "5. 80-100% perc"
  }
  
  Ink6
}

# OMZETTEN DATA TYPEN
statdata$Geslacht =  as.factor(statdata$VRLGBAGESLACHT)
statdata$BurgerlijkeStaat = as.factor(statdata$BURKLASSE4)
statdata$Generatie = as.numeric(statdata$VRLGBAGENERATIE)
statdata$Provincie = as.factor(statdata$PROVINCIE)


# GEDURENDE 2017 IS DE NAAM VAN DE PERS INKOMEN VARIABELE VERANDERD
#if ((length(statdata$PERCPERSINK))>0){ statdata$InkPerc = as.numeric(statdata$PERCPERSINK) }
#if ((length(statdata$P100PPERS))>0) { statdata$InkPerc = as.numeric(statdata$P100PPERS) }
#if ((length(statdata$INPP100PPERS))>0) { statdata$InkPerc = as.numeric(statdata$INPP100PPERS) }
statdata$InkPerc = as.numeric(statdata$InkPerc)

statdata$LFT = as.numeric(statdata$LFT)
#NB In 2016 en 2017 zijn variabelen etniciteit anders gelabeld
#if (Jaar == 2016)  { statdata$Herkomst = as.numeric(statdata$HERK8) }
#if (Jaar == 2017)  { statdata$Herkomst = as.numeric(statdata$ETNGROEP3) }
statdata$Herkomst = as.numeric(statdata$Herkomst)

statdata$Stedelijkheid = as.factor(statdata$STEDGEM)
# NB IN 2017 ZIJN STEDELIJKHEID VOOR 2017 EN 2016 OPGENOMEN
#if (Jaar == 2016)  { statdata$Stedelijkheid = as.factor(statdata$STEDGEM) }
#if (Jaar == 2017)  {
#   labelSted = paste("STEDGEM",as.character(Jaar),sep="")
#   statdata$Stedelijkheid = as.factor(statdata[[ labelSted ]])
#  }


Stedelijkheid = function(Stedelijkheid){
    if (Stedelijkheid==3) Stedelijkheid = "Matig stedelijk"
    if (Stedelijkheid==5) Stedelijkheid = "Niet stedelijk"
    if (Stedelijkheid==2) Stedelijkheid = "Sterk stedelijk"
    if (Stedelijkheid==4)  Stedelijkheid = "Weinig stedelijk"
    if (Stedelijkheid==1) Stedelijkheid = "Zeer sterk stedelijk"
  
  Stedelijkheid

}


BurgerlijkeStaat = function(BurgerlijkeStaat){
  if (BurgerlijkeStaat==4) BurgerlijkeStaat="1. Ongehuwd"
  if (BurgerlijkeStaat==1) BurgerlijkeStaat="2. Gehuwd of Partnerschap"
  if (BurgerlijkeStaat==3) BurgerlijkeStaat="3. Verweduwd of Verweduwd na partnerschap"
  if (BurgerlijkeStaat==2) BurgerlijkeStaat="4. Gescheiden of Gescheiden na partnerschap" 
 
    BurgerlijkeStaat
  
}

Geslacht = function(Geslacht){
  if (Geslacht==1) Geslacht="M"
  if (Geslacht==2) Geslacht="V"
  Geslacht
}





# Educational level (highest attained)
OplNiveau = function(oplniv) {
  if (is.na(oplniv)) { opl = "6. Unknown" }
  else {
    if ((oplniv==11) | (oplniv==110)) { opl = "1. Primary" }
    if ((oplniv==12) | (oplniv==120)) { opl = "2. VMBO"}
    if ((oplniv==21) | (oplniv==210)) { opl = "3. HAVO-VWO-MBO" }
    if ((oplniv==31) | (oplniv==310)) { opl = "4. Bachelor HBO-WO" }
    if ((oplniv==32) | (oplniv==320)) { opl = "5. Master HBO-WO" }
  }
  
  opl
}

# House ownership
Woning = function(huurkoop) {
  if (is.na(huurkoop) | (huurkoop ==9)) { eigenaar="0. Missing" }
  else {
    if (huurkoop==1) { eigenaar = "1. House owner"}
    if (huurkoop==2) { eigenaar = "2. Rental (social)" }
    if (huurkoop==3) { eigenaar = "3. Rental (other)" }
  }
  
  eigenaar
}

#  Type of household
HHtype = function(hhtype){
  if (is.na(hhtype) | (hhtype=="")) { hh = "5. Other" } 
  else { 
    if (hhtype=="1") hh = "1. 1"                                                                                               
    if ((hhtype=="2") | (hhtype=="3")) hh = "2. 2"
    if ((hhtype=="4") | (hhtype=="5")) hh = "3. 2+"
    if (hhtype=="6") hh = "4. 1+"
    if ((hhtype=="0") |(hhtype=="7") | (hhtype=="8")) hh = "5. Other"
  }
  hh
}

# Socio-economic status (SES)
InkSoort = function(ses){
  if (is.na(ses)) {
    inksoort = "5. Other non-active/unknown"
  }
  else { 
    if (ses==11) { inksoort = "1. Employed"}
    if ((ses>11) & (ses<16)) { inksoort = "2. Self-employed"}
    if ((ses>20) & (ses<25)) { inksoort = "3. Allowance" }
    if (ses==25) { inksoort = "4. Retired"}
    if (ses>25)  { inksoort = "5. Other non-active/unknown" }
  }
  
  inksoort
}

# Assets
VermogenKlas6 = function(Inkomen){
  if (is.na(Inkomen) | (Inkomen<0)) { Verm6 = "0. Missing" }
  else {
    if (Inkomen<=20) { Verm6 = "1. 0-20% perc"}
    if ((Inkomen>20) & (Inkomen<=40)) { Verm6 = "2. 20-40% perc" }
    if ((Inkomen>40) & (Inkomen<=60)) { Verm6 = "3. 40-60% perc" }
    if ((Inkomen>60) & (Inkomen<=80)) { Verm6 = "4. 60-80%" }
    if (Inkomen>80) { Verm6 = "5. 80-100% perc" }
  }
  
  Verm6
}

# Registered unemployed
CWI = function(inschrcwi){
  if (is.na(inschrcwi)) {
    cwi = "0. No"
  }else{
  if (inschrcwi== 0)  cwi = "0. No"
  if ((inschrcwi== 1) | (inschrcwi== 2))  cwi = "1. Yes"
  }
  cwi
}



statdata$TypeHH = as.factor(apply(array(statdata$VRLTYPHH),1,HHtype))   
statdata$OplNivHB = as.factor(apply(array(statdata$OPLNIVHB),1,OplNiveau))
# statdata$HuurKoop = as.factor(apply(array(statdata$HuurKoop),1,Woning))
# statdata$VermogenHH = as.factor(apply(array(statdata$VermogenHH),1,VermogenKlas6))
statdata$InschrCWI = as.factor(apply(array(statdata$INGESCHREVEN),1,CWI)) 


# DESELECTEREN EENHEDEN MET MISSENDE WAARDEN OP GESLACHT, LEEFTIJD OF STEDELIJKHEID
compleet = ((!is.na(statdata$Geslacht)) & (!is.na(statdata$LFT)) & (!is.na(statdata$Stedelijkheid)))
cat(nrow(statdata)-sum(compleet),"eenheden zijn verwijderd vanwege incomplete registergegevens\n")
statdata = statdata[compleet,]

# AFLEIDEN LEEFTIJD EN LEEFTIJDKLASSEN
statdata$Leeftijd=statdata$LFT
statdata$Leeftijdsklasse = as.factor(apply(array(statdata$LFT),1,LftKlas15))
statdata$Leeftijdsklasse13 = as.factor(apply(array(statdata$LFT),1,LftKlas13))

# AFLEIDEN HERKOMSTGENERATIE EN GECOMBINEERDE VARIABELE
statdata$HerkomstGeneratie = "Onbekend"

for (i in 1:length(statdata$Herkomst))
{
  statdata$HerkomstGeneratie[i] = HerkomstGeneratie(statdata$Herkomst[i],statdata$Generatie[i])
}


statdata$Herkomst = as.factor(apply(array(statdata$Herkomst),1,Herkomst))
statdata$Generatie = as.factor(statdata$Generatie)
statdata$HerkomstGeneratie = as.factor(statdata$HerkomstGeneratie)
statdata$Stedelijkheid = as.factor(apply(array(statdata$Stedelijkheid),1,Stedelijkheid))
statdata$BurgerlijkeStaat = as.factor(apply(array(statdata$BurgerlijkeStaat),1,BurgerlijkeStaat))
statdata$Geslacht = as.factor(apply(array(statdata$Geslacht),1,Geslacht))
statdata$AlgGezon=statdata$AlgGezo


# AFLEIDEN INKOMENKLASSEN
statdata$Inkomensklasse = as.factor(apply(array(statdata$InkPerc),1,InkKlas6))
statdata$Inkomen =as.numeric(statdata$InkPerc)

statdata
}