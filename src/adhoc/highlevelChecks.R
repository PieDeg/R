check_inbound <- function(){
  df_in   <- readDf("df_aanleveringen")
  df_product <- readDf("df_productMaster")
  
  
  # Check duplicates
  df_in <- distinct(df_in)
  
  # Inbound volume
  df1 <- df_in %>%
    filter(datumOntvangst>"2018-08-01" & datumOntvangst<"2019-07-31") %>%
    select(numArtikel, datumOntvangst, in.hoeveelheid) %>%
    left_join(df_product, by=c("numArtikel"="numArtikel")) %>%
    mutate(totaal_gewicht=(in.hoeveelheid*gewichtNettoArtikelGram)/1000,
           month=format_monthyear(datumOntvangst)) %>%
    group_by(month, DC) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T))
  
  
  mylabels1 <- df1 %>%
    group_by(month) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T)) %>%
    mutate(labels=sprintf("%s ton",format(round(totaal_gewicht/1000,0),big.mark=",")))
  
  ggplot(df1, aes(x=month, y=totaal_gewicht)) +
    geom_bar(stat="identity", aes(fill=factor(DC,levels=c("diepvries","vers","droog"),labels=c("diepvries","vers","droog")))) +
    scale_fill_manual(values=parameters$color_palette[c(3,2,1)])+
    labs(x="",
         y="Total Weight (kg)",
         title="Total weight (kg) inbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank())+
    geom_text(data=mylabels1,aes(x=month,y=totaal_gewicht,label=labels), vjust=-0.5, size=2.5)
  
  
  
  # Inbound volume
  df1 <- df_in %>%
    filter(datumOntvangst>"2018-08-01" & datumOntvangst<"2019-07-31") %>%
    select(numArtikel, datumOntvangst, in.hoeveelheid) %>%
    left_join(df_product, by=c("numArtikel"="numArtikel")) %>%
    mutate(totaal_gewicht=(in.hoeveelheid*gewichtNettoArtikelGram)/1000,
           month=format_monthyear(datumOntvangst)) %>%
    group_by(month, activiteit) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T))
  
  
  mylabels1 <- df1 %>%
    group_by(month) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T)) %>%
    mutate(labels=sprintf("%s ton",format(round(totaal_gewicht/1000,0),big.mark=",")))
  
  ggplot(df1, aes(x=month, y=totaal_gewicht)) +
    geom_bar(stat="identity", aes(fill=activiteit)) +
    scale_fill_manual(values=parameters$color_palette)+
    labs(x="",
         y="Total Weight (kg)",
         title="Total weight (kg) inbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank())+
    geom_text(data=mylabels1,aes(x=month,y=totaal_gewicht,label=labels), vjust=-0.5, size=2.5)
  
  
  
  # Inbound volume
  df1 <- df_in %>%
    filter(datumOntvangst>"2018-08-01" & datumOntvangst<"2019-07-31") %>%
    select(numArtikel, datumOntvangst, in.hoeveelheid) %>%
    left_join(df_product, by=c("numArtikel"="numArtikel")) %>%
    mutate(totaal_volume=(in.hoeveelheid*volumeNetMl)/1000,
           month=format_monthyear(datumOntvangst)) %>%
    group_by(month, DC) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T))
  
  
  mylabels1 <- df1 %>%
    group_by(month) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T)) %>%
    mutate(labels=sprintf("%s l",format(round(totaal_volume/1000,0),big.mark=",")))
  
  ggplot(df1, aes(x=month, y=totaal_volume)) +
    geom_bar(stat="identity", aes(fill=factor(DC,levels=c("diepvries","vers","droog"),labels=c("diepvries","vers","droog")))) +
    scale_fill_manual(values=parameters$color_palette[c(3,2,1)])+
    labs(x="",
         y="Total volume (l)",
         title="Total volume (l) inbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank())+
    geom_text(data=mylabels1,aes(x=month,y=totaal_volume,label=labels), vjust=-0.5, size=2.5)
  
  
  # Inbound volume
  df1 <- df_in %>%
    filter(in.datumOntvangst>"2018-08-01" & in.datumOntvangst<"2019-07-31") %>%
    select(in.numArtikel, in.datumOntvangst, in.hoeveelheid) %>%
    left_join(df_product, by=c("in.numArtikel"="art.numArtikel")) %>%
    mutate(totaal_volume=(in.hoeveelheid*art.volumeNet)/1000,
           month=format_monthyear(in.datumOntvangst)) %>%
    group_by(month, art.activiteit) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T))
  
  
  mylabels1 <- df1 %>%
    group_by(month) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T)) %>%
    mutate(labels=sprintf("%s l",format(round(totaal_volume/1000,0),big.mark=",")))
  
  ggplot(df1, aes(x=month, y=totaal_volume)) +
    geom_bar(stat="identity", aes(fill=art.activiteit)) +
    scale_fill_manual(values=parameters$color_palette)+
    labs(x="",
         y="Total volume (l)",
         title="Total volume (l) inbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank())+
    geom_text(data=mylabels1,aes(x=month,y=totaal_volume,label=labels), vjust=-0.5, size=2.5)
}

check_outbound <- function(){
  df_out  <- readDf("df_orders")
  df_product <- readDf("df_productMaster")
  
  # Outbound weight
  
  df2 <- df_out %>%
    filter(out.datumLevering>"2018-08-01" & out.datumLevering<"2019-07-31") %>%
    select(out.numArtikel, out.datumLevering, out.hoeveelheid) %>%
    left_join(select(df_product, art.numArtikel, art.omschrijvingArtikel, art.gewichtNettoArtikel, art.aantalEenheden, art.DC), by=c("out.numArtikel"="art.numArtikel")) %>%
    mutate(totaal_gewicht=(out.hoeveelheid*art.gewichtNettoArtikel)/1000,
           month=format_monthyear(out.datumLevering)) %>%
    group_by(month, art.DC) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T))
  
  
  mylabels2 <- df2 %>%
    group_by(month) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T)) %>%
    mutate(labels=sprintf("%s ton",format(round(totaal_gewicht/1000,0),big.mark=",")))
  
  # Outbound weight per month
  
  ggplot(df2, aes(x=month, y=totaal_gewicht)) +
    geom_bar(stat="identity", aes(fill=factor(art.DC,levels=c("diepvries","vers","droog"),labels=c("diepvries","vers","droog")))) +
    scale_fill_manual(values=parameters$color_palette[c(3,2,1)])+
    labs(x="",
         y="Total Weight (kg)",
         title="Total weight (kg) outbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank()) +
    geom_text(data=mylabels2,aes(x=month,y=totaal_gewicht,label=labels), vjust=-0.5, size=2.5)
  
  # Outbound volume
  
  df2 <- df_out %>%
    filter(out.datumLevering>"2018-08-01" & out.datumLevering<"2019-07-31") %>%
    select(out.numArtikel, out.datumLevering, out.hoeveelheid) %>%
    left_join(df_product, by=c("out.numArtikel"="art.numArtikel")) %>%
    mutate(totaal_volume=(out.hoeveelheid*art.volumeNet)/1000,
           month=format_monthyear(out.datumLevering)) %>%
    group_by(month, art.DC) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T))
  
  
  mylabels2 <- df2 %>%
    group_by(month) %>%
    summarize(totaal_volume=sum(totaal_volume, na.rm=T)) %>%
    mutate(labels=sprintf("%s l",format(round(totaal_volume/1000,0),big.mark=",")))
  
  # Outbound volume per month
  
  ggplot(df2, aes(x=month, y=totaal_volume)) +
    geom_bar(stat="identity", aes(fill=factor(art.DC,levels=c("diepvries","vers","droog"),labels=c("diepvries","vers","droog")))) +
    scale_fill_manual(values=parameters$color_palette[c(3,2,1)])+
    labs(x="",
         y="Total volume (ml)",
         title="Total volume (ml) outbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank()) +
    geom_text(data=mylabels2,aes(x=month,y=totaal_volume,label=labels), vjust=-0.5, size=2.5)
  
  
  # Check outlier outbound jul2019
  df <- df_out %>%
    # filter(out.datumLevering>"2019-07-01" & out.datumLevering<"2019-07-31") %>%
    
    left_join(select(df_product, art.numArtikel, art.omschrijvingArtikel, art.gewichtNettoArtikel, art.aantalEenheden, art.DC), by=c("out.numArtikel"="art.numArtikel")) %>%
    mutate(totaal_gewicht=(out.hoeveelheid*art.gewichtNettoArtikel)/1000,
           month=format_monthyear(out.datumLevering)) %>%
    group_by(out.datumLevering,art.DC) %>%
    summarize(totaal_gewicht=sum(totaal_gewicht, na.rm=T))
  
  ggplot(df, aes(x=out.datumLevering, y=totaal_gewicht)) +
    geom_bar(stat="identity", aes(fill=factor(art.DC,levels=c("diepvries","vers","droog"),labels=c("diepvries","vers","droog")))) +
    scale_fill_manual(values=parameters$color_palette[c(3,2,1)])+
    labs(x="",
         y="Total Weight (kg)",
         title="Total weight (kg) outbound") +
    scale_y_continuous(labels=scales::comma) +
    theme(legend.title=element_blank()) 
  
  
}

check_inboundVsOutbound <- function(){
  df <- df1 %>%
    left_join(df2, by=c("month","art.DC")) %>%
    rename(inbound_gewicht=totaal_gewicht.x,
           outbound_gewicht=totaal_gewicht.y) %>%
    mutate(inbound_gewicht=inbound_gewicht/1000,
           outbound_gewicht=outbound_gewicht/1000)
  
  totals <- df1 %>%
    left_join(df2, by=c("month","art.DC")) %>%
    group_by(month) %>%
    summarize(totaal_gewicht.x=sum(totaal_gewicht.x, na.rm=T),
              totaal_gewicht.y=sum(totaal_gewicht.y, na.rm=T)) %>%
    rename(inbound_gewicht=totaal_gewicht.x,
           outbound_gewicht=totaal_gewicht.y) %>%
    mutate(inbound_gewicht=inbound_gewicht/1000,
           outbound_gewicht=outbound_gewicht/1000)
  
  totals <- melt(totals)
  
  df <- melt(df)
  
  ggplot(totals, aes(x=month, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(data=totals, aes(x=month, y=value,label=round(value,0)), position=position_dodge(width=0.9), vjust=-0.5, size=2)+
    labs(x="", y="Gewicht in ton") +
    scale_fill_manual(values=parameters$color_palette)
  
}

check_naArtikels_outbound <- function(){
  df_out <- readDf("df_orders")
  df_product <- readDf("df_productMaster")
  
  df <- df_in %>%
    select(out.numArtikel) %>%
    distinct() %>%
    left_join(df_product, by=c("out.numArtikel"="art.numArtikel")) %>%
    filter(is.na(art.DC))
}

check_ritten <- function(){
  df <- readDf("df_ritten")%>%
    filter(aantalStops != "NA")
  parameters <- readDf("parameters")
  df$month <- format_monthyear(df$datum)
  df$weekday <- factorize_weekdays(weekdays(df$datum))
  
  # Aantal ritten per dag
  
  df1 <- df %>%
    filter(datum>="2018-08-01" & datum<="2019-07-31") %>%
    group_by(month, hubRit) %>%
    summarize(aantalRitten=n())
  
  totals <- df1 %>%
    group_by(month) %>%
    summarize(totaal_ritten=sum(aantalRitten, na.rm=T))
  
  ggplot(df1, aes(x=month, y=aantalRitten)) +
    geom_bar(stat="identity", aes(fill=hubRit))+
    scale_fill_manual(values=parameters$color_palette) +
    labs(x="",
         y="Aantal Ritten",
         title="Aantal ritten per maand") +
    theme(legend.title=element_blank())+
    geom_text(data=totals, aes(x=month, y=totaal_ritten,label=totaal_ritten), vjust=-0.5, size=3)
  
  # Aantal ritten gemiddeld per weekdag
  df2 <- df %>%
    filter(datum>="2018-08-01" & datum<="2019-07-31") %>%
    group_by(datum, weekday) %>%
    summarize(totaal_ritten=n()) %>%
    group_by(weekday) %>%
    summarize(totaal_ritten=mean(totaal_ritten, na.rm=T))
  
  ggplot(df2, aes(x=weekday, y=totaal_ritten)) +
    geom_bar(stat="identity")
  
  df2 <- df %>%
    filter(datum>="2018-08-01" & datum<="2019-07-31") %>%
    group_by(datum, weekday) %>%
    summarize(totaal_ritten=n()) 
  
  ggplot(df2, aes(x=weekday, y=totaal_ritten)) +
    geom_boxplot() +
    labs(x="", y="Aantal ritten per dag",
         title="Aantal ritten per weekdag")
}

check_rittenVsOutbound <- function(){
  df_out <- distinct(readDf("df_orders")) 
  df_rit <- distinct(readDf("df_ritten"))
  
  df_checkrit <- df_rit %>%
    group_by(rit.datum, rit.numRit) %>%
    summarize(n=n()) %>%
    filter(rit.datum>="2018-08-01" & rit.datum<="2019-07-31")
  
  df <- df_out %>%
    left_join(df_rit, by=c("out.datumLevering"="rit.datum","out.numRoute"="rit.numRit"))
  
  
}

orders250 <- function(){
  df <- readDf("df_orders")
  df_klanten <- readDf("df_klantenmaster")
  
  # eur per order
  df2 <- df %>%
    left_join(select(df_klanten, Master_Klant_BE, Master_Klant_SFamilie, segment), by=c("numKlant"="Master_Klant_BE")) %>%
    filter(out.datumLevering>"2018-08-01" & out.datumLevering<"2019-07-31") %>%
    mutate(eur=out.hoeveelheid * out.prijsNetto) %>%
    group_by(out.numOrder, segment) %>%
    summarize(eur=sum(eur, na.rm=T)) %>%
    filter(eur>0 & eur<2500, !is.na(segment))
  
  df3 <- df %>%
    left_join(select(df_klanten, Master_Klant_BE, Master_Klant_SFamilie, segment), by=c("numKlant"="Master_Klant_BE")) %>%
    filter(out.datumLevering>"2018-08-01" & out.datumLevering<"2019-07-31") %>%
    mutate(eur=out.hoeveelheid * `waardeNetto(Deduite)`) %>%
    group_by(out.numOrder) %>%
    summarize(eur=sum(eur, na.rm=T)) %>%
    filter(eur>0 & eur<2500)
    
  ggplot(df2, aes(x=eur))+
    geom_histogram(bins=300, color="grey", fill="grey") +
    geom_vline(xintercept=250, linetype="dashed", color="red", size=1)+
    labs(x="EUR/order", y="Number of Orders") +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(labels=scales::comma) +
    facet_grid(cols=vars(segment))
  
  ggplot(df3, aes(x=eur))+
    geom_histogram(bins=300, color="grey", fill="grey") +
    geom_vline(xintercept=250, linetype="dashed", color="red", size=1)+
    labs(x="EUR/order", y="Number of Orders") +
    scale_y_continuous(labels=scales::comma) +
    scale_x_continuous(labels=scales::comma)
}

checks_terplaatse <- function(){
  omzet <- df_out %>%
    mutate(eur=out.hoeveelheid*out.prijsNetto) %>%
    filter(out.datumBestelling>"2018-08-01" & out.datumBestelling<"2019-07-31") %>%
    
  
  omzet <- sum(omzet$eur)
  
  df <- df_ritten %>%
    group_by(rit.numRit, rit.datum) %>%
    summarize(n=n())
  
}