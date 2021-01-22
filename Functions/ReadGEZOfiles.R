# GEZO: files  we want to read are in "projectmap\Werk\onderzoek\Jaar\dirMnd"

### use two functions  Kosten en VerwerkX

#### read csv and bla files and select the variables
last <- function(x) { return( x[length(x)] ) }
# ReadGEZOCAWICAPI(Y, 2015, 3, "GEZO","CAPI", "3 - Maart")
# 
# 
# ReadGEZOCAWICAPI(Y, 2018, 3, "GEZO", "CAPI", "3 - Maart")
# 
#   (Y, 2015, 11, "GEZO","11 - November")
# projectmap=Y
#   Jaar=2018
#    Mnd=3
#      onderzoek="GEZO"
#     dirMnd="3 - Maart"
#    design="CAPI"

 ReadGEZOCAWICAPI = function(projectmap, Jaar, Mnd, onderzoek, design, dirMnd){

  hh=FALSE
  
      map<-paste(projectmap,"/Werk/",onderzoek,"/",Jaar,"/",dirMnd,sep="")
      filecsv <- last(list.files(map, pattern = paste0(".*",onderzoek,".*.",design,".*",".csv*")))
      status = read.csv2(paste(map,"/",filecsv,sep=""), header=TRUE, na.strings=NA)
      print(filecsv)  
      #status$aantalRappels  =status$AantalRappels
      #status$aantalBezoeken =status$Aantal_bezoeken
      #status=PasVarNamenAanSTAT(status)
      status$herbenaderingsmode<-ifelse(status$Aantal_bezoeken>0, "CAPI", "Geen") ############# Added lines
      status=PasVarNamenAanSTAT(status)
      statdata = Kosten(projectmap, status, "CAWICAPI", "incentivetype" == 0, "incentivesize"=0)
      statdata = VerwerkX2(Jaar,Mnd,statdata,hh)
      statdata$month <- Mnd
      statdata$month <- as.factor(statdata$month)
  
  statdata = statdata[statdata$Eindresultaat>10,]  

statdata[,c( "Eindresultaat", "ModeRespons", 
              "aantalBezoeken", "DuurVragenlijstInMin",
              "IsAfgebroken",
              "kostenContactCAWI",      
              "kostenWeigeringCAWI",    
              "kostenTotaalCAWI",        
              "kostenContactCAPI",
              "kostenContactCAPI3",
              "kostenNonresponsCAPI",   
              "kostenResponsCAPI" ,      
              "kostenContactResponsCAPI",
              "kostenTotaalCAPI",        
              "kostenTotaalCAWICAPI",
              "Geslacht", 
              "BurgerlijkeStaat",   
              "HerkomstGeneratie",  "Stedelijkheid",           
              "Inkomensklasse",
              "Inkomen",
              "Leeftijd", 
              "Leeftijdsklasse",
              "Leeftijdsklasse13",
              "Rookt",
              "Lengte",
              "Gewicht",
              "AlgGezon",
              "TypeHH",
              "OplNivHB",
              #"HuurKoop",
              #"VermogenHH",
              "InschrCWI",
              "month")]
}
