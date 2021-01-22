
############################################################
#
# Functie voor het schatten van gerealiseerde kosten
#
############################################################

# design: "CATI", CAWI", "CAWICATI", "CAWICAPI","CAWICAPTI"

Kosten<-function(projectmap, statdata, design, incentivetype, incentivesize=0){
  
   # Kosten(projectmap, status, "CAWICAPI", "incentivetype" == 0)
   # statdata=status
   # design="CAWICAPI"
   # incentivetype=0

  n = nrow(statdata)
  
  #INLEZEN PARAMETERS
  
  tijdenBezoeken<-as.data.frame(read.csv2(file=paste(projectmap,"/Beheer/Syntax/Kosten/BenadenAdmtijdenPerbezoek.csv",sep=""),
                                            header=T, dec=","))
  #colnames(benadenadmtijden)<-c("bezoek","benader","adm")
  colnames(tijdenBezoeken)<-c("aantalBezoeken","benadertijd","administratietijd")
  ####
  GemSnelheid<-as.data.frame(read.csv2(file=paste(projectmap,"/Beheer/Syntax/Kosten/GemSneelheidPerRegio.csv", sep=""),
                                        header=T, dec=",")[,1:2])
  #######
  KilometersPerAdres<-as.data.frame(read.csv2(file=paste(projectmap,"/Beheer/Syntax/Kosten/KilometersPerAdres.csv", sep=""),
                                              header=T, dec=","))
  ##########
  
  #INSTELLEN PARAMETERS
  
  kostenBrief=0.21  # euro
  kostenFolder=0.03  # euro
  kostenPostBriefmetFolder=0.53 # euro
  UurtariefSchaal7=41 # euro, telefonisten die weigering opvangen CATI
  UurtariefSchaal5=35 # euro, veldinterviewers CAPI
  tijdWeigeringCAWI=5 # min, was administratietijd
  UurtariefSchaal4=34 # euro, telefonisten(interviewers) CATI
  tijdContactCATI=4.7
  tijdAfspraakCATI=1.9
  tijdNonresponsCATI=1.8
  tijdNonresponsCAPI=5 # min, was administratietijd  
  
  # Maak indicator voor eventuele incentive randomisatie
  incentrand = "Incentive" %in% names(statdata)
  
  #BEREKENEN KOSTEN
  
  #************************** CAWI **********************#
  
  if (design %in% c("CAWI", "CAWICATI", "CAWICAPI","CAWICAPTI")){
  
  ### indicatoren
  indWeigeringCAWI<-!is.na(statdata$Eindresultaat)&((statdata$Eindresultaat==20 & statdata$AantalBezoeken==0 )|statdata$Eindresultaat %in% c(54,55))
    
  ### kosten voor eerste contact
  kostenEersteContactCAWI<-kostenBrief+kostenFolder+kostenPostBriefmetFolder
  ### totale kosten voor rappels
  kostenRappelsCAWI<-(kostenBrief+kostenPostBriefmetFolder)*statdata$aantalRappels
  ### totale kosten voor contact
  statdata$kostenContactCAWI<-rowSums(cbind(kostenEersteContactCAWI,kostenRappelsCAWI), na.rm=TRUE)
  ### kosten voor nonrespons
  statdata$kostenWeigeringCAWI<-NA
  statdata$kostenWeigeringCAWI[indWeigeringCAWI]<-UurtariefSchaal7/60*tijdWeigeringCAWI
  ### geen kosten voor respons
  ### totale kosten
  statdata$kostenTotaalCAWI<-rowSums(cbind(statdata$kostenContactCAWI,statdata$kostenWeigeringCAWI), na.rm=T)
  
  
  ####### Toevoegen incentive #######
  if (incentrand){
     for (i in 1:n){
       # Onconditioneel
       if (((incentivetype==1) | (incentivetype==2)) & (statdata$Incentive[i]==1)) { statdata$kostenTotaalCAWI[i] = statdata$kostenTotaalCAWI[i] + incentivesize }    
       # Conditioneel
       if ((incentivetype==3) & (statdata$Eindresultaat[i]==30) & (statdata$ModeRespons[i]=="CAWI") & (statdata$Incentive[i]==1)) { statdata$kostenTotaalCAWI[i] = statdata$kostenTotaalCAWI[i] + incentivesize }
     }
      } else {
       # Onconditioneel
       if ((incentivetype==1) | (incentivetype==2)) { statdata$kostenTotaalCAWI = statdata$kostenTotaalCAWI + incentivesize }
       for (i in 1:n){
         # Conditioneel
         if ((incentivetype==3) & (statdata$Eindresultaat[i]==30) & (statdata$ModeRespons[i]=="CAWI")) { statdata$kostenTotaalCAWI[i] = statdata$kostenTotaalCAWI[i] + incentivesize }
       }
     }
    
  }
  
  #************************** CATI NA CAWI **********************#
  
  if (design %in% c("CAWICATI","CAWICAPTI")){
    
    ### indicatoren
    indCATI<-statdata$herbenaderingsmode=="CATI"
    indNonresponsCATI<-!is.na(statdata$Eindresultaat)&(statdata$Eindresultaat%in%c(20,23,26,50,51))&statdata$herbenaderingsmode=="CATI"
    indRespCATI<-statdata$Eindresultaat==30&statdata$ModeRespons=="CATI"&statdata$herbenaderingsmode=="CATI"
    
    
    #### kosten voor maken contact
    kostenContactCATI<-NA
    kostenContactCATI[indCATI]<-UurtariefSchaal4/60*statdata$aantalContacten[indCATI]*tijdContactCATI*1.14

    ### kosten voor maken afspraak [NaAfspraak==1]
    kostenAfspraakCATI<-NA
    kostenAfspraakCATI[indCATI]<-ifelse(!is.na(statdata$NaAfspraak[indCATI]), UurtariefSchaal4/60*tijdAfspraakCATI*1.14, 0)
    
    ### totale kosten voor maken contact
    statdata$kostenContactCATI<-rowSums(cbind(kostenContactCATI,kostenAfspraakCATI),na.rm=T) #CHECK
    
    ### kosten voor nonrespons
    statdata$kostenNonresponsCATI<-NA #weigering ook in nonrespons
    statdata$kostenNonresponsCATI[indNonresponsCATI]<-UurtariefSchaal4/60*tijdNonresponsCATI*1.14 ### hier vragen over het UurtariefCATI
        
    ###  kosten voor respons  
    kostenResponsCATI<-NA
    kostenResponsCATI[indRespCATI]<- UurtariefSchaal4/60*(statdata$DuurVragenlijstInMin[indRespCATI])*1.14
    statdata$kostenResponsCATI<-kostenResponsCATI
    
    ### kosten voor contact t/m respons
    statdata$kostenContactAfspraakResponsCATI<-rowSums(cbind(kostenAfspraakCATI,kostenContactCATI,kostenResponsCATI),na.rm=T)
    
    #### totale kosten
    kostenTotaalCATI<-rowSums(cbind(statdata$kostenContactAfspraakResponsCATI,statdata$kostenNonresponsCATI), na.rm=T)
    statdata$kostenTotaalCATI<-kostenTotaalCATI
    
    # Toevoegen conditionele incentive
    if (incentrand){
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCATI[i]) & (statdata$Incentive[i]==1)) { statdata$kostenTotaalCATI[i] = statdata$kostenTotaalCATI[i] + incentivesize }
      }
    } else {
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCATI[i])) { statdata$kostenTotaalCATI[i] = statdata$kostenTotaalCATI[i] + incentivesize }
      }
    }
    
    ### totale kosten CAWI+CATI
    statdata$kostenTotaalCAWICATI<-rowSums(cbind(statdata$kostenTotaalCAWI,kostenTotaalCATI), na.rm=T)
    
    
  }
  
#************************** CATI EERSTE MODE **********************#
  
  if (design %in% c("CATI")){
    
    ### indicatoren
    indNonresponsCATI<-!is.na(statdata$Eindresultaat)&(statdata$Eindresultaat%in%c(20,23,26,50,51))
    indRespCATI<-statdata$Eindresultaat==30&statdata$ModeRespons=="CATI"
    
    #### kosten voor maken contact
    kostenContactCATI<-NA
    kostenContactCATI<-UurtariefSchaal4/60*statdata$aantalContacten*tijdContactCATI*1.14
    
    ### kosten voor maken afspraak [NaAfspraak==1]
    kostenAfspraakCATI<-NA
    kostenAfspraakCATI<-ifelse(!is.na(statdata$NaAfspraak), UurtariefSchaal4/60*tijdAfspraakCATI*1.14, 0)
    
    ### totale kosten voor maken contact
    statdata$kostenContactCATI<-rowSums(cbind(kostenContactCATI,kostenAfspraakCATI),na.rm=T) #CHECK
    
    ### kosten voor nonrespons
    statdata$kostenNonresponsCATI<-NA #weigering ook in nonrespons
    statdata$kostenNonresponsCATI[indNonresponsCATI]<-UurtariefSchaal4/60*tijdNonresponsCATI*1.14 ### hier vragen over het UurtariefCATI
    
    ###  kosten voor respons  
    kostenResponsCATI<-NA
    kostenResponsCATI[indRespCATI]<- UurtariefSchaal4/60*(statdata$DuurVragenlijstInMin[indRespCATI])*1.14
    statdata$kostenResponsCATI<-kostenResponsCATI
    
    ### kosten voor contact t/m respons
    statdata$kostenContactAfspraakResponsCATI<-rowSums(cbind(kostenAfspraakCATI,kostenContactCATI,kostenResponsCATI),na.rm=T)
    
    #### totale kosten
    kostenTotaalCATI<-rowSums(cbind(statdata$kostenContactAfspraakResponsCATI,statdata$kostenNonresponsCATI), na.rm=T)
    statdata$kostenTotaalCATI<-kostenTotaalCATI
    
    # Toevoegen conditionele incentive
    if (incentrand){
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCATI[i]) & (statdata$Incentive[i]==1)) { statdata$kostenTotaalCATI[i] = statdata$kostenTotaalCATI[i] + incentivesize }
      }
    } else {
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCATI[i])) { statdata$kostenTotaalCATI[i] = statdata$kostenTotaalCATI[i] + incentivesize }
      }
    }
      
    
  }
    
  
  #************************** CAPI NA CAWI **********************#
  
  if (design %in% c("CAWICAPI","CAWICAPTI")){
    
    ### indicatoren
    indCAPI<-statdata$herbenaderingsmode=="CAPI"
    indNonresponsCAPI<-!is.na(statdata$Eindresultaat)&(statdata$Eindresultaat%in%c(1:9,20:23,50,51,62))&statdata$herbenaderingsmode=="CAPI"
    indRespCAPI<-statdata$Eindresultaat==30&statdata$ModeRespons=="CAPI"&statdata$herbenaderingsmode=="CAPI"
    
    #indNonresponsCAPI<-!is.na(statdata$Eindresultaat)&
    #  ((statdata$Eindresultaat>0&statdata$Eindresultaat<10)| #### 
    #     (statdata$Eindresultaat>19&statdata$Eindresultaat<24)|
    #     statdata$Eindresultaat==50|statdata$Eindresultaat==51|
    #     statdata$Eindresultaat==62)  
    
      
    ### reiskosten
    
       kostenReisCAPI<-NA
    kostenReisCAPI[indCAPI]<-ifelse(statdata$aantalBezoeken[indCAPI]==0, NA,  #### this function does not dependent on region.
                           UurtariefSchaal5/60*statdata$aantalBezoeken[indCAPI]*KilometersPerAdres$km.bezoek[14]/GemSnelheid$OViN[14]*60+
                             statdata$aantalBezoeken[indCAPI]*KilometersPerAdres$km.bezoek[14]*0.37) # 0.37 is  vergoeding
    
    kostenReisCAPI3<-NA
    kostenReisCAPI3[indCAPI]<-ifelse(statdata$aantalBezoeken[indCAPI]==0, NA,  #### this function does not dependent on region.
                                     ifelse(statdata$aantalBezoeken[indCAPI]<4,
                                      UurtariefSchaal5/60*statdata$aantalBezoeken[indCAPI]*KilometersPerAdres$km.bezoek[14]/GemSnelheid$OViN[14]*60+
                                      statdata$aantalBezoeken[indCAPI]*KilometersPerAdres$km.bezoek[14]*0.37, 
                                   ifelse(statdata$aantalBezoeken[indCAPI]>3, ## drove more than 3 times, then calculate the cost for 3 trips 
                                             UurtariefSchaal5/60*3*KilometersPerAdres$km.bezoek[14]/GemSnelheid$OViN[14]*60+
                                               3*KilometersPerAdres$km.bezoek[14]*0.37,NA)))
    
    ### kosten voor bezoeken
    kostenBezoekCAPI<-NA
    kostenBezoekCAPI[indCAPI]<-ifelse(statdata$aantalBezoeken[indCAPI]==0, NA, 
                              ifelse(statdata$aantalBezoeken[indCAPI]>5,
                                     UurtariefSchaal5/60*(tijdenBezoeken$benadertijd[6+1]+tijdenBezoeken$administratietijd[6+1]),
                                     UurtariefSchaal5/60*(tijdenBezoeken$benadertijd[statdata$aantalBezoeken[indCAPI]+1]+
                                                            tijdenBezoeken$administratietijd[statdata$aantalBezoeken[indCAPI]+1])))
    kostenBezoekCAPI3<-NA
    kostenBezoekCAPI3[indCAPI]<-ifelse(statdata$aantalBezoeken[indCAPI]==0, NA, 
                                      ifelse(statdata$aantalBezoeken[indCAPI]<4,
                                            UurtariefSchaal5/60*(tijdenBezoeken$benadertijd[statdata$aantalBezoeken[indCAPI]+1]+
                                                                    tijdenBezoeken$administratietijd[statdata$aantalBezoeken[indCAPI]+1]), 
                                     ifelse(statdata$aantalBezoeken[indCAPI]>3, ### more than 3 times approached, then calculate costs of 3 times approach
                                                   UurtariefSchaal5/60*(tijdenBezoeken$benadertijd[3+1]+
                                                                          tijdenBezoeken$administratietijd[3+1]),      NA)))
    ### kosten voor contact
    kostenContactCAPI<-rowSums(cbind(kostenReisCAPI,kostenBezoekCAPI),na.rm=T)
    kostenContactCAPI3<-rowSums(cbind(kostenReisCAPI3,kostenBezoekCAPI3),na.rm=T)
    statdata$kostenContactCAPI<-kostenContactCAPI
    statdata$kostenContactCAPI3<-kostenContactCAPI3
    ### kosten voor nonrespons
    statdata$kostenNonresponsCAPI<-NA
    statdata$kostenNonresponsCAPI[indNonresponsCAPI]<-UurtariefSchaal7/60*tijdNonresponsCAPI
    
    
    ### kosten voor respons        
    kostenResponsCAPI<-NA
    kostenResponsCAPI[indRespCAPI]<- UurtariefSchaal5/60*(statdata$DuurVragenlijstInMin[indRespCAPI]+6)
    statdata$kostenResponsCAPI<-kostenResponsCAPI
    
    ### kosten voor contact t/m evt respons
    statdata$kostenContactResponsCAPI<-rowSums(cbind(kostenContactCAPI,kostenResponsCAPI),na.rm=T)
    
    ### totale kosten CAPI
    kostenTotaalCAPI<-rowSums(cbind(statdata$kostenContactResponsCAPI,statdata$kostenNonresponsCAPI), na.rm=T)
    statdata$kostenTotaalCAPI<-kostenTotaalCAPI
    
    # Toevoegen conditionele incentive
    if (incentrand){
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCAPI[i]) & (statdata$Incentive[i]==1)) { statdata$kostenTotaalCAPI[i] = statdata$kostenTotaalCAPI[i] + incentivesize }
      }
    } else {
      for (i in 1:n){
        if ((incentivetype==3) & (indRespCAPI[i])) { statdata$kostenTotaalCAPI[i] = statdata$kostenTotaalCAPI[i] + incentivesize }
      }
    }
    
    ### totale kosten CAWI+CAPI
    statdata$kostenTotaalCAWICAPI<-rowSums(cbind(statdata$kostenTotaalCAWI,kostenTotaalCAPI), na.rm=T)
   
    
  }
  
  
  #************************** CAPTI NA CAWI **********************#
  
  if (design == "CAWICAPTI"){
    ### totale kosten
    statdata$kostenTotaalCAWICATICAPI<-rowSums(cbind(statdata$kostenTotaalCAWI,kostenTotaalCATI,kostenTotaalCAPI), na.rm=T)
  
  }
  
  statdata

}


