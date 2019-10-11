# Read Data ---------------------------------------------------------------
ReadFinancial <- function(){
  df_financial <- readData("alles")
  df_financial <- df_financial%>%
    mutate(numDropF = paste(Master_Klant_Leveradres,datumLevering,numRoute,sep=""))%>%
    filter(numRoute != "NA")
  
  return(df_financial)
}
ReadLogistic <- function(){
  df_logistic <- df_financial%>%
    filter(typeOrder != "CPT")
  df_logistic <- df_logistic%>%
    mutate(numDropF = paste(Master_Klant_Leveradres,datumLevering,numRoute,sep=""))%>%
    filter(numRoute != "NA")
  
  return(df_logistic)
}
ReadParameters <- function(){
  parameters <- readData("parameters")
  
  return(parameters)
}
# Logistiek perspectief -------------- delivery lijn niveau ####
HighLevel <- function(df_financial, df_logistic){
  # 1. Omzet 
  totaalOmzet <- sum(df_financial$`waardeNetto(Deduite)`,na.rm = T) #--------------------- 128.221.455,81 EUR
  totaalOmzetPerLijn <- mean(df_logistic$`waardeNetto(Deduite)`,na.rm=T) #---------------- 18.32 EUR
  # 2. Aantal lijnen
  totaalFysiekeLijnen <- nrow(df_logistic) #---------------------------------------------- 7.007.490
  totaalAdminLijnen <- nrow(df_financial) - nrow(df_logistic) #--------------------------- 23.817
  # 3. Gewicht
  totaalGewichtKG <- sum(df_logistic$out.gewichtGram)/1000 #------------------------------ 66.645.439,388 KG
  totaalGewichtKGPerLijn <- mean(df_logistic$out.gewichtGram,na.rm=T)/1000 #-------------- 9.51 KG
  # 4. Volume
  totaalVolumeL <- sum(df_logistic$TotaalVolume)/1000 #------------------------------------ 33.667.431,530 L
  totaalVolumeLPerLijn <- mean(df_logistic$TotaalVolume,na.rm=T)/1000 #-------------------- 4.8 L
  # 5. Aantal Leveringen
  totaalLeveringen <- n_distinct(df_logistic$numLevering) #------------------------------- 326.673
  # 6. Aantal Ritten
  totaalRitten <- nrow(df_logistic%>%
                         filter(aantalRitten != "NA")%>%
                         group_by(datumLevering,numRoute)%>%summarize()) #---------------- 23.069
  # 7. Aantal factuurNummers
  totaalFacturen <- n_distinct(df_logistic$numFactuur) #---------------------------------- 267.544
  # 8. Aantal ordernummers
  totaalOrders <- n_distinct(df_logistic$numOrder) #-------------------------------------- 318.425
  
  Variable <- c("totaalOmzet", "totaalOmzetPerLijn", "totaalFysiekeLijnen", "totaalAdminLijnen",
             "totaalGewichtKG", "totaalGewichtKGPerLijn", "totaalVolumeL", "totaalVolumeLPerLijn",
             "totaalLeveringen", "totaalRitten", "totaalFacturen", "totaalOrders")
  Value <- c(totaalOmzet, totaalOmzetPerLijn, totaalFysiekeLijnen, totaalAdminLijnen, totaalGewichtKG, totaalGewichtKGPerLijn, totaalVolumeL, totaalVolumeLPerLijn,
            totaalLeveringen, totaalRitten, totaalFacturen, totaalOrders)
  df_highLevel <- data.frame(Variable,Value)
  return(df_highLevel)
}
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
SegmentAlgemeen <- function(df_logistic){
  # Verdeling activite over DC
  ac_dc <- df_logistic%>%
    group_by(numActiviteit, DC)%>%
    summarize(orderlijnen = n())
  # 1. Omzet
  segmentOmzet <- setNames(aggregate(`waardeNetto(Deduite)`~segment, df_financial, sum), c("segment","Omzet"))
  segmentOmzetPerLijn <- setNames(aggregate(`waardeNetto(Deduite)`~segment, df_financial, mean), c("segment","Omzet Per Lijn"))
  # 2. Aantal lijnen
  segmentFysiekeLijnen <- df_logistic%>%group_by(segment)%>%
    summarize(FysiekeLijnen = n())%>%
    filter(segment!="NA")
  segmentAdminLijnen <- df_financial%>%filter(typeOrder =="CPT")%>%
    group_by(segment)%>%
    summarize(AdminLijnen = n())%>%
    filter(segment!="NA")
  # 3. Gewicht
  segmentGewicht  <- df_logistic%>%group_by(segment)%>%
    summarize(avgGewichtKG = mean(out.gewichtGram, na.rm=T)/1000,
              totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000)%>%
    filter(segment!="NA")
  # 4. Volume
  segmentVolume  <- df_logistic%>%group_by(segment)%>%
    summarize(avgVolumeL = mean(TotaalVolume, na.rm=T)/1000,
              totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000)%>%
    filter(segment!="NA")
  # 5. Aantal Leveringen
  segmentLeveringen <- df_logistic%>%group_by(segment)%>%
    summarize(aantalLeveringen = n_distinct(numLevering))%>%
    filter(segment!="NA")
  # 6. Aantal Ritten
  segmentRitten<- df_logistic%>%filter(aantalRitten != "NA")%>%
    mutate(datumRit = paste(datumLevering,numRoute,sep=""))%>%
    group_by(segment)%>%
    summarize(aantalRitten = n_distinct(datumRit))%>%
    filter(segment!="NA")
  # 7. Aantal factuurNummers
  segmentFacturen <- df_logistic%>%group_by(segment)%>%
    summarize(aantalFacturen = n_distinct(numFactuur))%>%
    filter(segment!="NA") 
  # 8. Aantal ordernummers
  segmentOrders <- df_logistic%>%group_by(segment)%>%
    summarize(aantalOrders= n_distinct(numOrder))%>%
    filter(segment!="NA")
  # Segment full dataframe
  Segment <- segmentOmzet%>%
    left_join(segmentOmzetPerLijn, by="segment")%>%
    left_join(segmentFysiekeLijnen, by="segment")%>%
    left_join(segmentAdminLijnen, by="segment")%>%
    left_join(segmentGewicht, by="segment")%>%
    left_join(segmentVolume, by="segment")%>%
    left_join(segmentLeveringen, by="segment")%>%
    left_join(segmentRitten, by="segment")%>%
    left_join(segmentFacturen, by="segment")%>%
    left_join(segmentOrders, by="segment")
  
  rm(segmentOmzet,segmentOmzetPerLijn, segmentFysiekeLijnen, segmentAdminLijnen, segmentGewicht, segmentVolume, segmentLeveringen, segmentRitten, segmentFacturen, segmentOrders)
  
  percsegment <- Segment%>%
    group_by(segment)%>%
    mutate(PercOmzet = Omzet/sum(Segment$Omzet)*100,
           PercFysiekeLijnen = FysiekeLijnen/sum(Segment$FysiekeLijnen)*100,
           PercAdminLijnen = AdminLijnen/sum(Segment$AdminLijnen)*100,
           PercKG = totaalGewichtKG/sum(Segment$totaalGewichtKG)*100,
           PercL = totaalVolumeL /sum(Segment$totaalVolumeL )*100)
}
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Activiteit - algemene statistieken
# 1. Omzet
activiteitOmzet <- setNames(aggregate(`waardeNetto(Deduite)`~numActiviteit, df_financial, sum), c("numActiviteit","Omzet"))
activiteitOmzetPerLijn <- setNames(aggregate(`waardeNetto(Deduite)`~numActiviteit, df_financial, mean), c("numActiviteit","Omzet Per Lijn"))
# 2. Aantal lijnen
activiteitFysiekeLijnen <- df_logistic%>%group_by(numActiviteit)%>%
  summarize(FysiekeLijnen = n())%>%
  filter(numActiviteit!="NA")
activiteitAdminLijnen <- df_financial%>%filter(typeOrder =="CPT")%>%
  group_by(numActiviteit)%>%
  summarize(AdminLijnen = n())%>%
  filter(numActiviteit!="NA")
# 3. Gewicht
activiteitGewicht  <- df_logistic%>%group_by(numActiviteit)%>%
  summarize(avgGewichtKG = mean(out.gewichtGram, na.rm=T)/1000,
            totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000)%>%
  filter(numActiviteit!="NA")
# 4. Volume
activiteitVolume  <- df_logistic%>%group_by(numActiviteit)%>%
  summarize(avgVolumeL = mean(TotaalVolume, na.rm=T)/1000,
            totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000)%>%
  filter(numActiviteit!="NA")
# activiteit full dataframe
activiteit <- activiteitOmzet%>%
  left_join(activiteitOmzetPerLijn, by="numActiviteit")%>%
  left_join(activiteitFysiekeLijnen, by="numActiviteit")%>%
  left_join(activiteitAdminLijnen, by="numActiviteit")%>%
  left_join(activiteitGewicht, by="numActiviteit")%>%
  left_join(activiteitVolume, by="numActiviteit")

rm(activiteitOmzet,activiteitOmzetPerLijn, activiteitFysiekeLijnen, activiteitAdminLijnen, activiteitGewicht, activiteitVolume)

# DC - algemene statistieken
# 1. Omzet
DCOmzet <- setNames(aggregate(`waardeNetto(Deduite)`~DC, df_financial, sum), c("DC","Omzet"))
DCOmzetPerLijn <- setNames(aggregate(`waardeNetto(Deduite)`~DC, df_financial, mean), c("DC","Omzet Per Lijn"))
# 2. Aantal lijnen
DCFysiekeLijnen <- df_logistic%>%group_by(DC)%>%
  summarize(FysiekeLijnen = n())%>%
  filter(DC!="NA")
DCAdminLijnen <- df_financial%>%filter(typeOrder =="CPT")%>%
  group_by(DC)%>%
  summarize(AdminLijnen = n())%>%
  filter(DC!="NA")
# 3. Gewicht
DCGewicht  <- df_logistic%>%group_by(DC)%>%
  summarize(avgGewichtKG = mean(out.gewichtGram, na.rm=T)/1000,
            totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000)%>%
  filter(DC!="NA")
# 4. Volume
DCVolume  <- df_logistic%>%group_by(DC)%>%
  summarize(avgVolumeL = mean(TotaalVolume, na.rm=T)/1000,
            totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000)%>%
  filter(DC!="NA")
# DC full dataframe
Dc <- DCOmzet%>%
  left_join(DCOmzetPerLijn, by="DC")%>%
  left_join(DCFysiekeLijnen, by="DC")%>%
  left_join(DCAdminLijnen, by="DC")%>%
  left_join(DCGewicht, by="DC")%>%
  left_join(DCVolume, by="DC")

rm(DCOmzet,DCOmzetPerLijn, DCFysiekeLijnen, DCAdminLijnen, DCGewicht, DCVolume)
percDC <- Dc%>%
  group_by(DC)%>%
  mutate(PercOmzet = Omzet/sum(Dc$Omzet)*100,
         PercFysiekeLijnen = FysiekeLijnen/sum(Dc$FysiekeLijnen)*100,
         PercAdminLijnen = AdminLijnen/sum(Dc$AdminLijnen)*100,
         PercKG = totaalGewichtKG/sum(Dc$totaalGewichtKG)*100,
         PercL = totaalVolumeL /sum(Dc$totaalVolumeL )*100)

# Analyse klanten
df_klant <- readData("klantenmaster")
#BE nummers per adres
BEperAdres <- df_klant%>%
  group_by(Master_Klant_Leveradres)%>%
  summarize(AantalBE = n_distinct(Master_Klant_BE))
#Adressen per BE nummers
AdresPerBE <- df_klant%>%
  group_by(Master_Klant_BE)%>%
  summarize(AantalBE = n_distinct(Master_Klant_Leveradres))
#BE Nummers per bedrijf
BEPerBedrijf <- df_klant%>%
  group_by(Master_Klant_Naam)%>%
  summarize(AantalBE = n_distinct(Master_Klant_BE))
#Adressen per Bedrijf
AdresPerBedrijf <- df_klant%>%
  group_by(Master_Klant_Naam)%>%
  summarize(AantalBE = n_distinct(Master_Klant_Leveradres))
# Variabiliteit orderlijnen over tijd
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())

ggplot(weekdag, aes(x=weekday, y=Omzet, fill=weekday)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/weekdag") +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_manual(values=parameters$color_palette) +
  theme(legend.title=element_blank())+
  geom_text(data=weekdag,aes(x=weekday,y=Omzet,label=weekday), vjust=-0.5, size=2.5)

ggplot(jaardag, aes(x=datumLevering, y=Omzet, fill=datumLevering)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/jaardag") +
  scale_y_continuous(labels=scales::comma)

ggplot(week, aes(x=weeknummer, y=Omzet, fill=weeknummer)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/week") +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.title=element_blank())

ggplot(maand, aes(x=month, y=Omzet, fill=month)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/maand") +
  scale_y_continuous(labels=scales::comma)

ggplot(weekdag, aes(x=weekday, y=Aantal, fill=weekday)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal deliverylijnen",
       title="") +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_manual(values=parameters$color_palette) +
  theme(legend.title=element_blank())+
  geom_text(data=weekdag,aes(x=weekday,y=Aantal,label=weekday), vjust=-0.5, size=2.5)

ggplot(jaardag, aes(x=datumLevering, y=Aantal, fill=datumLevering)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal deliverylijnen",
       title="") +
  scale_y_continuous(labels=scales::comma)

ggplot(week, aes(x=weeknummer, y=Aantal, fill=weeknummer)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal",
       title="Aantal/week") +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.title=element_blank())

ggplot(maand, aes(x=month, y=Aantal, fill=month)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal",
       title="Aantal/maand") +
  scale_y_continuous(labels=scales::comma)

# Segment - Variabiliteit
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(segment, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(segment, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(segment, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(segment, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")

# Activiteit - Variabiliteit
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numActiviteit, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, numActiviteit, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numActiviteit, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numActiviteit, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")

# Delivery lijnen - product perspectief
DCSegment <- df_logistic%>%
  group_by(segment,DC,numActiviteit)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")%>%
  ungroup()%>%
  group_by(DC,numActiviteit)%>%
  mutate(Percentage = Aantal/sum(Aantal)*100,
         PercentageOmzet = Omzet/sum(Omzet)*100)
# 
# Volume of KG 0 Product Master
df_product <- readData("productmaster") #---------------------------------- 14070 artikels
gewichtNull <- df_product%>%
  filter((gewichtNettoArtikelGram == 0 | gewichtNettoArtikelGram == "NA") 
         & !(TotaalVolume == 0 | TotaalVolume == "NA")) #--------------------- 115 artikels (79 zonder gewicht en zonder volume, 36 zonder gewicht)
volumeNull <- df_product%>%
  filter(TotaalVolume == 0 | TotaalVolume == "NA") #------------------------- 10809 artikels
volumeEnGewichtNull <- df_product%>%
  filter((gewichtNettoArtikelGram == 0 | gewichtNettoArtikelGram == "NA") 
         & (TotaalVolume == 0 | TotaalVolume == "NA")) #--------------------- 79 artikels

volumeEnGewichtFill <- round((nrow(df_product)-nrow(volumeNull)-nrow(gewichtNull)-nrow(volumeEnGewichtNull))/nrow(df_product)*100,2)
volumeEnGewichtNull <- round(nrow(volumeEnGewichtNull)/nrow(df_product)*100,2)
gewichtNull <- round(nrow(gewichtNull)/nrow(df_product)*100,2)
volumeNull <- round(nrow(volumeNull)/nrow(df_product)*100,2)


# Logistiek perspectief -------------------- drop niveau N ####
# Drop definitie - Aantal Drops
#1. Rapportering
adminDrops <- df_logistic%>% # --------------------------------------------------------------------- RAPPORTERING DEFINITIE -----#
  group_by(numKlant, datumLevering)%>%
  summarize()

adminDropsRitNr <- df_logistic%>%
  group_by(numKlant, datumLevering, numRoute)%>%
  summarize()

FysiekeDrops <- df_logistic%>%
  group_by(Master_Klant_Leveradres, datumLevering)%>%
  summarize()
#2. Fysiek
FysiekeDropsRitNr <- df_logistic%>% # -------------------------------------------------------------- FYSIEKE REALISTISCHE DEFINITIE -----#
  group_by(Master_Klant_Leveradres, datumLevering, numRoute)%>%
  summarize()
# Drops - High Level Statistics
# 1. Omzet 
perDropOmzet <- df_logistic%>% #--------------------------------------------------------- 518,67 EUR/Drop #247.502 Drops
  group_by(numDrop)%>%
  summarize(`waardeNetto(Deduite)` = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
perDropOmzet <- perDropOmzet%>%
  summarize(
    Aantaldrops = n(),
    OmzetPerDrop = mean(perDropOmzet$`waardeNetto(Deduite)`,na.rm=T)
  )
# 2. Aantal lijnen
aantalFysiekeLijnen <- df_logistic%>%group_by(numDrop)%>%summarize(aantal = n())
aantalAdminLijnen <- df_financial%>%filter(typeOrder == "CPT")%>%group_by(numDrop)%>%summarize(aantal = n())

perDropFysiekeLijnen <- mean(aantalFysiekeLijnen$aantal, na.rm = T) #-------------------- 28.31
perDropAdminLijnen <- mean(aantalAdminLijnen$aantal, na.rm = T) #------------------------ 3.90

rm(aantalFysiekeLijnen, aantalAdminLijnen)
# 3. Gewicht
perDropGewichtKG <- df_logistic%>%
  group_by(numDrop)%>%
  summarize(totaalGewicht = sum(out.gewichtGram,na.rm = T))%>%
  ungroup()
perDropGewichtKG <- perDropGewichtKG%>%
  summarize(
    Aantaldrops = n(),
    GewichtPerDrop = round(mean(perDropGewichtKG$totaalGewicht,na.rm=T)/1000,2) #-------- 269.27 KG
  )
# 4. Volume
perDropVolumeL <- df_logistic%>%
  group_by(numDrop)%>%
  summarize(totaalVolume = sum(TotaalVolume,na.rm = T))%>%
  ungroup()
perDropVolumeL <- perDropVolumeL%>%
  summarize(
    Aantaldrops = n(),
    VolumePerDrop = round(mean(perDropVolumeL$totaalVolume,na.rm=T)/1000,2) #-------- 269.27 KG
  )
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Drops - Variabiliteit
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdag <- weekdag%>%
  group_by(weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
jaardag <- jaardag%>%
  group_by(datumLevering)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numDropF, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
week <- week%>%
  group_by(weeknummer)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
maand <- maand%>%
  group_by(month)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())

ggplot(weekdag, aes(x=weekday, y=Omzet, fill=weekday)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/weekdag") +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_manual(values=parameters$color_palette) +
  theme(legend.title=element_blank())+
  geom_text(data=weekdag,aes(x=weekday,y=Omzet,label=weekday), vjust=-0.5, size=2.5)

ggplot(jaardag, aes(x=datumLevering, y=Omzet, fill=datumLevering)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/jaardag") +
  scale_y_continuous(labels=scales::comma)

ggplot(week, aes(x=weeknummer, y=Omzet, fill=weeknummer)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/week") +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.title=element_blank())

ggplot(maand, aes(x=month, y=Omzet, fill=month)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Omzet",
       title="Omzet/maand") +
  scale_y_continuous(labels=scales::comma)

ggplot(weekdag, aes(x=weekday, y=Aantal, fill=weekday)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal deliverylijnen",
       title="") +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_manual(values=parameters$color_palette) +
  theme(legend.title=element_blank())+
  geom_text(data=weekdag,aes(x=weekday,y=Aantal,label=weekday), vjust=-0.5, size=2.5)

ggplot(jaardag, aes(x=datumLevering, y=Aantal, fill=datumLevering)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal deliverylijnen",
       title="") +
  scale_y_continuous(labels=scales::comma)

ggplot(week, aes(x=weeknummer, y=Aantal, fill=weeknummer)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal",
       title="Aantal/week") +
  scale_y_continuous(labels=scales::comma) +
  theme(legend.title=element_blank())

ggplot(maand, aes(x=month, y=Aantal, fill=month)) +
  geom_bar(stat="identity")+
  labs(x="",
       y="Aantal",
       title="Aantal/maand") +
  scale_y_continuous(labels=scales::comma)
# Segment - dropBE algemene statistieken
# 1. Omzet
perDropOmzetSegment <- df_logistic%>% #--------------------------------------------------------- 518,67 EUR/Drop #247.502 Drops
  group_by(numDrop,segment)%>%
  summarize(`waardeNetto(Deduite)` = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
perDropOmzetSegment <- perDropOmzetSegment%>%
  group_by(segment)%>%
  summarize(
    Aantaldrops = n(),
    OmzetPerDrop = mean(`waardeNetto(Deduite)`,na.rm=T)
  )%>%
  filter(segment!="NA")
# 2. Aantal lijnen
aantalFysiekeLijnen <- df_logistic%>%group_by(numDrop,segment)%>%summarize(aantalFysiek = n())
aantalAdminLijnen <- df_financial%>%filter(typeOrder == "CPT")%>%group_by(numDrop,segment)%>%summarize(aantalAdmin = n())

perDropFysiekeLijnenSegment <- aggregate(aantalFysiek~segment,aantalFysiekeLijnen, mean)%>%
  filter(segment!="NA")
perDropAdminLijnenSegment <- aggregate(aantalAdmin~segment,aantalAdminLijnen, mean)%>%
  filter(segment!="NA")

rm(aantalFysiekeLijnen, aantalAdminLijnen)
# 3. Gewicht
perDropGewichtKGSegment <- df_logistic%>%
  group_by(numDrop,segment)%>%
  summarize(totaalGewicht = sum(out.gewichtGram,na.rm = T))%>%
  ungroup()
perDropGewichtKGSegment <- perDropGewichtKGSegment%>%
  group_by(segment)%>%
  summarize(
    GewichtPerDrop = round(mean(totaalGewicht,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(segment!="NA")
# 4. Volume
perDropVolumeLSegment <- df_logistic%>%
  group_by(numDrop,segment)%>%
  summarize(totaalVolume = sum(TotaalVolume,na.rm = T))%>%
  ungroup()
perDropVolumeLSegment <- perDropVolumeLSegment%>%
  group_by(segment)%>%
  summarize(
    VolumePerDrop = round(mean(totaalVolume,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(segment!="NA")
# Segment full dataframe
perDropSegment <- perDropOmzetSegment%>%
  left_join(perDropFysiekeLijnenSegment, by="segment")%>%
  left_join(perDropAdminLijnenSegment, by="segment")%>%
  left_join(perDropGewichtKGSegment, by="segment")%>%
  left_join(perDropVolumeLSegment, by="segment")

rm(perDropOmzetSegment,perDropFysiekeLijnenSegment,perDropAdminLijnenSegment,perDropGewichtKGSegment,perDropVolumeLSegment)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Segment - dropF algemene statistieken
# 1. Omzet
perDropFOmzetSegment <- df_logistic%>% #--------------------------------------------------------- 518,67 EUR/DropF #247.502 DropFs
  group_by(numDropF,segment)%>%
  summarize(`waardeNetto(Deduite)` = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
perDropFOmzetSegment <- perDropFOmzetSegment%>%
  group_by(segment)%>%
  summarize(
    AantalDropFs = n(),
    OmzetPerDropF = mean(`waardeNetto(Deduite)`,na.rm=T)
  )%>%
  filter(segment!="NA")
# 2. Aantal lijnen
aantalFysiekeLijnen <- df_logistic%>%group_by(numDropF,segment)%>%summarize(aantalFysiek = n())
aantalAdminLijnen <- df_financial%>%filter(typeOrder == "CPT")%>%group_by(numDropF,segment)%>%summarize(aantalAdmin = n())

perDropFysiekeLijnenSegment <- aggregate(aantalFysiek~segment,aantalFysiekeLijnen, mean)%>%
  filter(segment!="NA")
perDropFAdminLijnenSegment <- aggregate(aantalAdmin~segment,aantalAdminLijnen, mean)%>%
  filter(segment!="NA")

rm(aantalFysiekeLijnen, aantalAdminLijnen)
# 3. Gewicht
perDropFGewichtKGSegment <- df_logistic%>%
  group_by(numDropF,segment)%>%
  summarize(totaalGewicht = sum(out.gewichtGram,na.rm = T))%>%
  ungroup()
perDropFGewichtKGSegment <- perDropFGewichtKGSegment%>%
  group_by(segment)%>%
  summarize(
    GewichtPerDropF = round(mean(totaalGewicht,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(segment!="NA")
# 4. Volume
perDropFVolumeLSegment <- df_logistic%>%
  group_by(numDropF,segment)%>%
  summarize(totaalVolume = sum(TotaalVolume,na.rm = T))%>%
  ungroup()
perDropFVolumeLSegment <- perDropFVolumeLSegment%>%
  group_by(segment)%>%
  summarize(
    VolumePerDropF = round(mean(totaalVolume,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(segment!="NA")
# Segment full dataframe
perDropFSegment <- perDropFOmzetSegment%>%
  left_join(perDropFysiekeLijnenSegment, by="segment")%>%
  left_join(perDropFAdminLijnenSegment, by="segment")%>%
  left_join(perDropFGewichtKGSegment, by="segment")%>%
  left_join(perDropFVolumeLSegment, by="segment")

rm(perDropFOmzetSegment,perDropFysiekeLijnenSegment,perDropFAdminLijnenSegment,perDropFGewichtKGSegment,perDropFVolumeLSegment)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#



# Segment - Variabiliteit
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, segment, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdag <- weekdag%>%
  group_by(segment, weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, segment, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
jaardag <- jaardag%>%
  group_by(segment, datumLevering)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numDropF, segment, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
week <- week%>%
  group_by(segment, weeknummer)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, segment, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
maand <- maand%>%
  group_by(segment, month)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(segment != "NA")

# Hub - drop statistieken
# 1. Omzet
perDropFOmzetHub <- df_logistic%>% #--------------------------------------------------------- 518,67 EUR/DropF #247.502 DropFs
  group_by(numDropF,numHub)%>%
  summarize(`waardeNetto(Deduite)` = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
perDropFOmzetHub <- perDropFOmzetHub%>%
  group_by(numHub)%>%
  summarize(
    AantalDropFs = n(),
    OmzetPerDropF = mean(`waardeNetto(Deduite)`,na.rm=T)
  )%>%
  filter(numHub!="NA")
# 2. Aantal lijnen
aantalFysiekeLijnen <- df_logistic%>%group_by(numDropF,numHub)%>%summarize(aantalFysiek = n())
aantalAdminLijnen <- df_financial%>%filter(typeOrder == "CPT")%>%group_by(numDropF,numHub)%>%summarize(aantalAdmin = n())

perDropFysiekeLijnenHub <- aggregate(aantalFysiek~numHub,aantalFysiekeLijnen, mean)%>%
  filter(numHub!="NA")
perDropFAdminLijnenHub <- aggregate(aantalAdmin~numHub,aantalAdminLijnen, mean)%>%
  filter(numHub!="NA")

rm(aantalFysiekeLijnen, aantalAdminLijnen)
# 3. Gewicht
perDropFGewichtKGHub <- df_logistic%>%
  group_by(numDropF,numHub)%>%
  summarize(totaalGewicht = sum(out.gewichtGram,na.rm = T))%>%
  ungroup()
perDropFGewichtKGHub <- perDropFGewichtKGHub%>%
  group_by(numHub)%>%
  summarize(
    GewichtPerDropF = round(mean(totaalGewicht,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(numHub!="NA")
# 4. Volume
perDropFVolumeLHub <- df_logistic%>%
  group_by(numDropF,numHub)%>%
  summarize(totaalVolume = sum(TotaalVolume,na.rm = T))%>%
  ungroup()
perDropFVolumeLHub <- perDropFVolumeLHub%>%
  group_by(numHub)%>%
  summarize(
    VolumePerDropF = round(mean(totaalVolume,na.rm=T)/1000,2) #-------- 269.27 KG
  )%>%
  filter(numHub!="NA")
# Hub full dataframe
perDropFHub <- perDropFOmzetHub%>%
  left_join(perDropFysiekeLijnenHub, by="numHub")%>%
  left_join(perDropFAdminLijnenHub, by="numHub")%>%
  left_join(perDropFGewichtKGHub, by="numHub")%>%
  left_join(perDropFVolumeLHub, by="numHub")

rm(perDropFOmzetHub,perDropFysiekeLijnenHub,perDropFAdminLijnenHub,perDropFGewichtKGHub,perDropFVolumeLHub)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#



# Hub - lijn statistieken
# 1. Omzet
numHubOmzet <- setNames(aggregate(`waardeNetto(Deduite)`~numHub, df_financial, sum), c("numHub","Omzet"))
numHubOmzetPerLijn <- setNames(aggregate(`waardeNetto(Deduite)`~numHub, df_financial, mean), c("numHub","Omzet Per Lijn"))
# 2. Aantal lijnen
numHubFysiekeLijnen <- df_logistic%>%group_by(numHub)%>%
  summarize(FysiekeLijnen = n())%>%
  filter(numHub!="NA")
numHubAdminLijnen <- df_financial%>%filter(typeOrder =="CPT")%>%
  group_by(numHub)%>%
  summarize(AdminLijnen = n())%>%
  filter(numHub!="NA")
# 3. Gewicht
numHubGewicht  <- df_logistic%>%group_by(numHub)%>%
  summarize(avgGewichtKG = mean(out.gewichtGram, na.rm=T)/1000,
            totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000)%>%
  filter(numHub!="NA")
# 4. Volume
numHubVolume  <- df_logistic%>%group_by(numHub)%>%
  summarize(avgVolumeL = mean(TotaalVolume, na.rm=T)/1000,
            totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000)%>%
  filter(numHub!="NA")
# 5. Aantal Leveringen
numHubLeveringen <- df_logistic%>%group_by(numHub)%>%
  summarize(aantalLeveringen = n_distinct(numLevering))%>%
  filter(numHub!="NA")
# 6. Aantal Ritten
numHubRitten<- df_logistic%>%filter(aantalRitten != "NA")%>%
  mutate(datumRit = paste(datumLevering,numRoute,sep=""))%>%
  group_by(numHub)%>%
  summarize(aantalRitten = n_distinct(datumRit))%>%
  filter(numHub!="NA")
# 7. Aantal factuurNummers
numHubFacturen <- df_logistic%>%group_by(numHub)%>%
  summarize(aantalFacturen = n_distinct(numFactuur))%>%
  filter(numHub!="NA") 
# 8. Aantal ordernummers
numHubOrders <- df_logistic%>%group_by(numHub)%>%
  summarize(aantalOrders= n_distinct(numOrder))%>%
  filter(numHub!="NA")
# Segment full dataframe
numHub <- numHubOmzet%>%
  left_join(numHubOmzetPerLijn, by="numHub")%>%
  left_join(numHubFysiekeLijnen, by="numHub")%>%
  left_join(numHubAdminLijnen, by="numHub")%>%
  left_join(numHubGewicht, by="numHub")%>%
  left_join(numHubVolume, by="numHub")%>%
  left_join(numHubLeveringen, by="numHub")%>%
  left_join(numHubRitten, by="numHub")%>%
  left_join(numHubFacturen, by="numHub")%>%
  left_join(numHubOrders, by="numHub")

rm(numHubOmzet,numHubOmzetPerLijn, numHubFysiekeLijnen, numHubAdminLijnen, numHubGewicht, numHubVolume, numHubLeveringen, numHubRitten, numHubFacturen, numHubOrders)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Hub - Variabiliteit
# 1. Per drop
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, numHub, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdag <- weekdag%>%
  group_by(numHub, weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(numHub != "NA")
# 2. Per delivery lijn
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numHub, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            Aantal = n())%>%
  filter(numHub != "NA")

# Activiteit - Variabiliteit
# 1. Per weekdag
weekdag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, numActiviteit, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdag <- weekdag%>%
  group_by(numActiviteit, weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 2. Per jaardag
jaardag <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, numActiviteit, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
jaardag <- jaardag%>%
  group_by(numActiviteit, datumLevering)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 3. Per week
week <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numDropF, numActiviteit, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
week <- week%>%
  group_by(numActiviteit, weeknummer)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# 4. Per maand
maand <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, numActiviteit, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
maand <- maand%>%
  group_by(numActiviteit, month)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(numActiviteit != "NA")
# Delivery punten & Omzet per segment
adres <- df_logistic%>%
  group_by(segment)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm = T),
            DeliveryPunten = n_distinct(Master_Klant_Leveradres))

dropsPunt <- df_logistic%>%
  group_by(segment, Master_Klant_Leveradres,numDropF)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()%>%
  group_by(segment, Master_Klant_Leveradres)%>%
  summarize(aantalDrops = n(),
            Omzet = sum(Omzet,na.rm=T))

lijnenPerDropChart <- df_logistic%>%
  group_by(segment,numDropF)%>%
  summarise(aantalLijnen = n())
# Logistiek perspectief -------------------- drop niveau N-1 ####
# Omzet/DC/Activité
df_omzetN1 <- df_logistic%>%
  group_by(doelgroep,DC,numActiviteit)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  filter(doelgroep !="NA")
# Drop karakteristieken
## Variabiliteit Omzet over tijd
### Per weekdag
weekdagN1 <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdagN1 <- weekdagN1%>%
  group_by(doelgroep, weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep != "NA")
### Per jaardag
jaardagN1 <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
jaardagN1 <- jaardagN1%>%
  group_by(doelgroep, datumLevering)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep != "NA")
### Per week
weekN1 <- df_logistic%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numDropF, doelgroep, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekN1 <- weekN1%>%
  group_by(doelgroep, weeknummer)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep != "NA")
### Per maand
maandN1 <- df_logistic%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
maandN1 <- maandN1%>%
  group_by(doelgroep, month)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep != "NA")
## Lijnen/Drop - Histogram
## Gemiddelden
df_avgDropN1 <- df_logistic%>%
  group_by(doelgroep,numDropF)%>%
  summarize(aantalLijnen = n(),
            totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000,
            totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000,
            Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
df_avgDropN1 <- df_avgDropN1%>%
  group_by(doelgroep)%>%
  summarize(Omzet = sum(Omzet),
            aantalDrops = n(),
            aantalLijnen = sum(aantalLijnen),
            OmzetPerDrop = Omzet/aantalDrops,
            lijnenPerDrop = aantalLijnen/aantalDrops,
            totaalGewichtKG = sum(totaalGewichtKG),
            totaalVolumeL = sum(totaalVolumeL),
            avgGewichtPerDropKG = totaalGewichtKG/aantalDrops,
            avgVolumePerDropL = totaalVolumeL/aantalDrops)%>%
  filter(doelgroep !="NA")
# Droppunten
dropsPuntN1 <- df_logistic%>%
  group_by(doelgroep, Master_Klant_Leveradres,numDropF)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()%>%
  group_by(doelgroep, Master_Klant_Leveradres)%>%
  summarize(aantalDrops = n(),
            Omzet = sum(Omzet,na.rm=T))
# Drops/jaar/droppunt
# Logistiek perspectief -------------------- drop niveau N-2 ####
# Omzet/DC/Activité
df_omzetN2 <- df_logistic%>%
  group_by(doelgroep_split,DC,numActiviteit)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  filter(doelgroep_split !="NA")
# Drop karakteristieken
## Variabiliteit omzet over tijd
### Per weekdag
weekdagN2 <- df_logistic%>%
  filter(segment == "Commercial")%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep_split, weekday)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekdagN2 <- weekdagN2%>%
  group_by(doelgroep_split, weekday)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep_split != "NA")
### Per jaardag
jaardagN2 <- df_logistic%>%
  filter(segment == "Commercial")%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep_split, datumLevering)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
jaardagN2 <- jaardagN2%>%
  group_by(doelgroep_split, datumLevering)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep_split != "NA")
### Per week
weekN2 <- df_logistic%>%
  filter(segment == "Commercial")%>%
  filter(weekday !="NA")%>%
  mutate(weeknummer = strftime(datumLevering, format = "%V"))%>%
  group_by(numDropF, doelgroep_split, weeknummer)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
weekN2 <- weekN2%>%
  group_by(doelgroep_split, weeknummer)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep_split != "NA")
### Per maand
maandN2 <- df_logistic%>%
  filter(segment == "Commercial")%>%
  filter(weekday !="NA")%>%
  group_by(numDropF, doelgroep_split, month)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()
maandN2 <- maandN2%>%
  group_by(doelgroep_split, month)%>%
  summarize(Omzet = sum(Omzet,na.rm=T),
            Aantal = n())%>%
  filter(doelgroep_split != "NA")
## Lijnen/Drop - Histogram
## Gemiddelden
df_avgDropN2 <- df_logistic%>%
  group_by(doelgroep_split,numDropF)%>%
  summarize(aantalLijnen = n(),
            totaalGewichtKG = sum(out.gewichtGram, na.rm=T)/1000,
            totaalVolumeL = sum(TotaalVolume, na.rm=T)/1000,
            Omzet = sum(`waardeNetto(Deduite)`,na.rm=T),
            aantalDroppunten = n_distinct(Master_Klant_Leveradres))%>%
  ungroup()
df_avgDropN2 <- df_avgDropN2%>%
  group_by(doelgroep_split)%>%
  summarize(aantalDrops = n(),
            aantalLijnen = sum(aantalLijnen),
            aantalDroppunten = sum(aantalDroppunten),
            Omzet = sum(Omzet),
            OmzetPerDrop = Omzet/aantalDrops,
            lijnenPerDrop = aantalLijnen/aantalDrops,
            totaalGewichtKG = sum(totaalGewichtKG),
            totaalVolumeL = sum(totaalVolumeL),
            avgGewichtPerDropKG = totaalGewichtKG/aantalDrops,
            avgVolumePerDropL = totaalVolumeL/aantalDrops)%>%
  filter(doelgroep_split !="NA")
# Drops/jaar/droppunt
dropsPuntN2 <- df_logistic%>%
  group_by(doelgroep_split, Master_Klant_Leveradres,numDropF)%>%
  summarize(Omzet = sum(`waardeNetto(Deduite)`,na.rm=T))%>%
  ungroup()%>%
  group_by(doelgroep_split, Master_Klant_Leveradres)%>%
  summarize(aantalDrops = n(),
            Omzet = sum(Omzet,na.rm=T))
#----####
#----------------------------------------------------------------------------------------------------------------------------------------------------------------#












