# Tijdelijke data aanpassingen
PasVarNaamAan=function(brondata,foute_varnaam,goede_varnaam){
  names(brondata)[names(brondata) %in% foute_varnaam ] <- goede_varnaam
  
  brondata  
}

PasVarNamenAanSTAT=function(statdata){
  #Insluitgewicht
  statdata=PasVarNaamAan(statdata,"InsluitGewicht","Insluitgewicht")
  
  #Uitdungewicht
  statdata=PasVarNaamAan(statdata,"uitdungewicht","Uitdungewicht")
    
  #DuurVragenlijstInMin
  statdata=PasVarNaamAan(statdata,"Vragenlijstduur_in_minuten","DuurVragenlijstInMin")
    
  #NaAfspraak
  statdata=PasVarNaamAan(statdata,"na_afpsraak","NaAfspraak")
    
  #DeviceType1
  statdata=PasVarNaamAan(statdata,"DeviceType.1","DeviceType1")
  
  #aantalRappels
  statdata=PasVarNaamAan(statdata,c("Aantal_uitgestuurde_rappels","AantalUitgestuurdeRappels","AantalRappels"),"aantalRappels")
  
  #aantalContacten
  statdata=PasVarNaamAan(statdata,c("contactpogingen","Contactpogingen","AantalContacten"),"aantalContacten")
  
  
  #aantalBezoeken
  statdata=PasVarNaamAan(statdata,c("Aantal_bezoeken","AantalBezoeken","Aantal_Bezoeken","aantalBezoeken"),"aantalBezoeken")
    
  #IsAfgebroken
  statdata=PasVarNaamAan(statdata,"afgebroken","IsAfgebroken")
  
  #ModeRespons
  statdata=PasVarNaamAan(statdata,"Mode_respons","ModeRespons")
  
  #Herbenaderingsmode
  statdata=PasVarNaamAan(statdata,"Herbenaderingsmode","herbenaderingsmode")
  
  #opleidingsniveaus
  for (pers in 1:8){
    statdata=PasVarNaamAan(statdata,paste("NivAct_",pers,sep=""),paste("NivAct",pers,sep=""))
    
    statdata=PasVarNaamAan(statdata,paste("Afl_HgstNivGev_",pers,sep=""),paste("Afl_HgstNivGev",pers,sep=""))
    
    statdata=PasVarNaamAan(statdata,paste("Afgel_4W_",pers,sep=""),paste("Afgel_4W",pers,sep=""))

  }

  #herkomstgeneratie
    statdata=PasVarNaamAan(statdata,"Herkomstgeneratie","HerkomstGeneratie")  
  #ink perc
    statdata=PasVarNaamAan(statdata,"Inkperc","InkPerc")  
    
  statdata

}



