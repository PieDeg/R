process_orderData <- function(){
  
  tic()
  # Read inputdata
  df_input1         <- read_xlsx(path=here("data","raw","Mobius_verk_201808.xlsx"), skip=3) 
  df_input2         <- read_xlsx(path=here("data","raw","Mobius_verk_201809.xlsx"), skip=3) 
  df_input3         <- read_xlsx(path=here("data","raw","Mobius_verk_201810.xlsx"), skip=3) 
  df_input4         <- read_xlsx(path=here("data","raw","Mobius_verk_201811.xlsx"), skip=3) 
  df_input5         <- read_xlsx(path=here("data","raw","Mobius_verk_201812.xlsx"), skip=3) 
  df_input6         <- read_xlsx(path=here("data","raw","Mobius_verk_201901.xlsx"), skip=3) 
  df_input7         <- read_xlsx(path=here("data","raw","Mobius_verk_201902.xlsx"), skip=3) 
  df_input8         <- read_xlsx(path=here("data","raw","Mobius_verk_201903.xlsx"), skip=3) 
  df_input9         <- read_xlsx(path=here("data","raw","Mobius_verk_201904.xlsx"), skip=3) 
  df_input10        <- read_xlsx(path=here("data","raw","Mobius_verk_201905.xlsx"), skip=3) 
  df_input11        <- read_xlsx(path=here("data","raw","Mobius_verk_201906.xlsx"), skip=2) 
  df_input12        <- read_xlsx(path=here("data","raw","Mobius_verk_201907.xlsx"), skip=3) 
  df_input13        <- read_xlsx(path=here("data","raw","Mobius_verk_201908.xlsx"), skip=3) 
  
  toc()
  
  df_input <- rbind(df_input1, df_input2, df_input3, df_input4, df_input5, df_input6, df_input7,
                    df_input8, df_input9, df_input10, df_input11,df_input12,df_input13)
  
  
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="Mobius_verk_201906")
  
  # Format data 
  df_output        <- df_input
  names(df_output) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep     <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_output        <- df_output[,fieldsToKeep] # Select only fields to keep and store in df_output

  df_output <- df_output %>%
    mutate(datumBestelling=as.Date(datumBestelling),
           datumLevering=as.Date(datumLevering))
  
  # Write outputdata as feather file
  saveDf(df_output, "df_orders")
}
process_aanleverData <- function(){
  # Read inputdata
  df <- read_xlsx(here("data","raw","Leveranciers Leveringen DataBase.xlsx"), sheet=2, skip=1)
  
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="Leveranciers Leveringen")
  
  # Format data 
  df_output        <- df
  names(df_output) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep     <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_output        <- df_output[,fieldsToKeep] # Select only fields to keep and store in df_output

  df_output <- df_output %>%
    mutate(datumOntvangst=as.Date(x=as.character(datumOntvangst), format="%Y%m%d"),
           datumBestelling=as.Date(x=as.character(datumBestelling), format="%Y%m%d"))

  saveDf(df_output,"df_aanleveringen")
}
process_productMaster <- function(){
  df <- read_xlsx(here("data","raw","Artikel Full Data.xlsx"), sheet=2, skip=1)
  
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="Artikel Full Data")
  
  # Format data 
  df_output        <- df
  names(df_output) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep     <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_output        <- df_output[,fieldsToKeep] # Select only fields to keep and store in df_output

  df_output <- df_output %>%
    mutate(datumAanmaakArtikel=as.Date(x=as.character(datumAanmaakArtikel), format="%Y%m%d"),
           DC=factor(temperatuurTransport, labels=c("droog","vers","diepvries"), levels=c("020","005","-18")))

  saveDf(df_output,"df_productMaster")
  
}
process_ritten <- function(){
  df <- read_xlsx(here("data","raw","data ritten.xlsx"))
  df_wodups <- distinct(df)
  
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="data ritten")
  
  # Format data 
  df_output        <- df_wodups
  names(df_output) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep     <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_output        <- df_output[,fieldsToKeep] # Select only fields to keep and store in df_output

  df_output <- df_output %>%
    mutate(datum=as.Date(x=datum, format="%d/%m/%Y"))

  saveDf(df_output,"df_ritten")
  
  
}
process_clients <- function(){
  df_klantdb       <- read_xlsx(here("data","raw","Klant Database.xlsx"), sheet=2, skip=1)
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="Klant DataBase")
  
  # Format data
  names(df_klantdb) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep      <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_klantdb        <- df_klantdb[,fieldsToKeep] # Select only fields to keep and store in df_klantdb
  
  df_klantmaster   <- read_xlsx(here("data","raw","Klant Master Data.xlsx"), sheet="DOELGROEPEN")
  translationTable <- read_xlsx(path=here("data","processed","Column_Transformation.xlsx"), sheet="Klant_Master_Data")

  # Format data
  names(df_klantmaster) <- translationTable$`Nieuw veld` # Replace old field names with new field names
  fieldsToKeep          <- translationTable$`Nieuw veld`[translationTable$`Nieuw veld`!="Delete"] # Determine which fields to keep (vector)
  df_klantmaster        <- df_klantmaster[,fieldsToKeep] # Select only fields to keep and store in df_klantmaster

  df_output <- df_klantdb %>%
     left_join(df_klantmaster, by=c("Master_Klant_SFamilie"="s_fam_stat"))
  
  saveDf(df_output,"df_klantenmaster")
}

raw_orderData <- function(){
  
  tic()
  # Read inputdata
  df_input1         <- read_xlsx(path=here("data","raw","Mobius_verk_201808.xlsx"), skip=3) 
  df_input2         <- read_xlsx(path=here("data","raw","Mobius_verk_201809.xlsx"), skip=3) 
  df_input3         <- read_xlsx(path=here("data","raw","Mobius_verk_201810.xlsx"), skip=3) 
  df_input4         <- read_xlsx(path=here("data","raw","Mobius_verk_201811.xlsx"), skip=3) 
  df_input5         <- read_xlsx(path=here("data","raw","Mobius_verk_201812.xlsx"), skip=3) 
  df_input6         <- read_xlsx(path=here("data","raw","Mobius_verk_201901.xlsx"), skip=3) 
  df_input7         <- read_xlsx(path=here("data","raw","Mobius_verk_201902.xlsx"), skip=3) 
  df_input8         <- read_xlsx(path=here("data","raw","Mobius_verk_201903.xlsx"), skip=3) 
  df_input9         <- read_xlsx(path=here("data","raw","Mobius_verk_201904.xlsx"), skip=3) 
  df_input10        <- read_xlsx(path=here("data","raw","Mobius_verk_201905.xlsx"), skip=3) 
  df_input11        <- read_xlsx(path=here("data","raw","Mobius_verk_201906.xlsx"), skip=2) 
  df_input12        <- read_xlsx(path=here("data","raw","Mobius_verk_201907.xlsx"), skip=3) 
  df_input13        <- read_xlsx(path=here("data","raw","Mobius_verk_201908.xlsx"), skip=3) 
  
  toc()
  
  df_input <- rbind(df_input1, df_input2, df_input3, df_input4, df_input5, df_input6, df_input7,
                    df_input8, df_input9, df_input10, df_input11,df_input12,df_input13)
  
  # Format data 
  df_output        <- df_input
  # Write outputdata as feather file
  saveDf(df_output, "df_orders")
}
raw_aanleverData <- function(){
  # Read inputdata
  df <- read_xlsx(here("data","raw","Leveranciers Leveringen DataBase.xlsx"), sheet=2, skip=1)
  
  # Format data 
  df_output        <- df
  saveDf(df_output,"df_aanleveringen")
}
raw_productMaster <- function(){
  df <- read_xlsx(here("data","raw","Artikel Full Data.xlsx"), sheet=2, skip=1)
  
  # Format data 
  df_output        <- df
  saveDf(df_output,"df_productMaster")
  
}
raw_ritten <- function(){
  df <- read_xlsx(here("data","raw","data ritten.xlsx"))
  df_wodups <- distinct(df)
  
  df_output        <- df_wodups
  saveDf(df_output,"df_ritten")
  
  
}
raw_clients <- function(){
  df_klantdb       <- read_xlsx(here("data","raw","Klant Database.xlsx"), sheet=2, skip=1)
  df_klantmaster   <- read_xlsx(here("data","raw","Klant Master Data.xlsx"), sheet="DOELGROEPEN")

  
  # Format data 
  df_output        <- df_klantmaster
  df_output <- df_klantdb %>%
    left_join(df_klantmaster, by=c("S-fam Client stat ."="S.fam Stat"))
  
  saveDf(df_output,"df_klantenmaster")
}