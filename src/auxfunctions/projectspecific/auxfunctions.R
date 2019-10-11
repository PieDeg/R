saveDf <- function(df, filename){
  tryCatch({
    write_feather(df, here("output","feather",paste(filename)))
  },
  error=function(cond){
    saveRDS(df, here("output","feather",paste(filename)))
  })
}

readDf <- function(filename){
  tryCatch({
    read_feather(here("output","feather","processed",paste(filename)))
  },
  error=function(cond){
    readRDS(here("output","feather","processed",paste(filename)))
  })
  
}

readData <- function(data){
  if(data == "orders"){
    df_orders <- readDf("df_orders")%>%
      filter(datumLevering>="2018-08-01" & datumLevering<="2019-07-31",
             numActiviteit != "VCG")%>%
      mutate(numDrop = paste(numKlant,datumLevering, sep=" "))%>% #----------------------------------- DEFINITIE DROPS ---#
      select(-originOrder, -numBonPreparation,-prijsPRV, -waardePRV)
    
    return(df_orders)
  }else if(data == "productmaster"){
    df_product <- readDf("df_productmaster")
    
    return(df_product)
  }else if(data == "ritten"){
    df_ritten <- readDf("df_ritten")%>%
      filter(datum>="2018-08-01" & datum<="2019-07-31",
             aantalRitten != "NA")%>%
      select(-(20:35))%>%
      select(-12,-21)
    df_ritten$month <- format_monthyear(df_ritten$datum)
    df_ritten$weekday <- factorize_weekdays(weekdays(df_ritten$datum))
    
    return(df_ritten)
  }else if(data == "klantenmaster"){
    df_klant <- readDf("df_klantenmaster")%>%
      select(-ss_fam_stat)
    df_klant <- distinct(df_klant)
    
    return(df_klant)
  }else if(data == "parameters"){
    parameters <- as.data.frame(readDf("parameters"),stringsAsFactors = FALSE)%>%
      rbind("#AB88AA")%>%
      rbind("#CCCCCC")
    
    return(parameters)
  }else if(data == "alles"){
    df_orders <- readData("orders")
    df_product <- readData("productmaster")
    df_klant <- readData("klantenmaster")
    df_ritten <- readData("ritten")
    
    df_all <- df_orders%>%
      left_join(df_product, by = "numArtikel")%>%
      left_join(df_klant, by = c("numKlant" = "Master_Klant_BE"))%>%
      left_join(df_ritten, by = c("numRoute" = "numRit", "datumLevering" = "datum"))%>%
      mutate(TotaalVolume = volumeNetMl * out.hoeveelheid)
    
    return(df_all)
  }
}




























