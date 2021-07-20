library(plotly)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(data.table)

# catalogue <- czso_get_catalogue()
#
# czso_get_codelist()
# catalogue %>%
#   filter(str_detect(title, "[Mm]zd[ay]")) %>%
#   select(dataset_id, title)
#
# tt<-czso_get_table("110079")


dt <- readRDS("./data-interim/sections.rds")
sum <- readRDS("./data-interim/summary.rds")


#počty zaměstnanců
abs_counts<-dt %>%
  filter(rok == 2020, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO","SS","OSS")) %>%
  group_by(kap_name) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu))# %>% filter(pocet_zamestnancu<10000)

bar_dt <- dt %>%
  filter(rok == 2020, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO","SS","OSS")) #%>%
  #filter(!kap_num %in% abs_counts$kap_num)


p<-plot_ly(bar_dt,x=~kap_name,y=~pocet_zamestnancu,color=~name,
        text = ~paste(" Zdroj:", name, "<br>","Kapitola:", full_kap_name, "<br>", "Zaměstnanců:", format(pocet_zamestnancu, big.mark = ' ')),
        hoverinfo = "text")  %>%
  add_bars() %>%
  layout(title="Vizualizace zaměstnanců",
         xaxis = list(title="Kapitoly",categoryorder = "array", categoryarray =arrange(abs_counts,pocet_zamestnancu)$kap_name),
         yaxis = list(title = "Počet zaměstnanců"),
         barmode="stack") %>%
  layout(hovermode = 'compare')  %>% plotly_build()
saveRDS(p,"data-output/bar_pocty_2020tz.html")

saveWidget(as_widget(p), "data-output/bar_pocty_2020.html")


#počet zaměstannců v tisících - odpovídá tabulce na https://petrbouchal.xyz/urednici/2_explore-counts.html
sum %>%
  filter(typ_rozpoctu == "SKUT") %>%
  mutate(pocet_zamestnancu = pocet_zamestnancu/1e3) %>%
  ggplot(aes(x = rok, y = pocet_zamestnancu)) + geom_line() + facet_wrap(~name, scales = "free") +ggtitle("Vývoj počtu zaměstnanců v tisících")


## platy
totals_abs<-dt %>%
  filter(rok == 2020, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO","SS","OSS")) %>%
  group_by(kap_name) %>%
  summarise(prostredky_na_platy = sum(prostredky_na_platy)) #%>% filter(pocet_zamestnancu<10000)


p<-plot_ly(bar_dt,x=~kap_name,y=~prostredky_na_platy,color=~name,
           text = ~paste(" Zdroj:", name, "<br>","Kapitola:", full_kap_name, "<br>", "Platy kumulativně:", format(prostredky_na_platy, big.mark = ' ')),
           hoverinfo = "text")  %>%
  add_bars() %>%
  layout(title="Vizualizace platů",
         xaxis = list(title="Kapitoly",categoryorder = "array", categoryarray =arrange(totals_abs,prostredky_na_platy)$kap_name),
         yaxis = list(title = "Platy"),
         barmode="stack") %>%
  layout(hovermode = 'compare')
saveWidget(as_widget(p), "data-output/bar_platy_2020.html")

##prumerne platy
totals_mean<-dt %>%
  filter(rok == 2020, typ_rozpoctu == "SCHV") %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(!name %in% c("ROPO","SS","OSS")) %>%
  group_by(kap_name) %>%
  summarise(prumerny_plat = sum(prumerny_plat)) #%>% filter(pocet_zamestnancu<10000)



p<-plot_ly(bar_dt,x=~kap_name,y=~prumerny_plat,color=~name,
           text = ~paste(" Zdroj:", name, "<br>","Kapitola:", full_kap_name, "<br>", "Prumerny plat:", format(prumerny_plat, big.mark = ' ')),
           hoverinfo = "text")  %>%
  add_bars() %>%
  layout(title="Vizualizace prumernych platů",
         xaxis = list(title="Kapitoly",categoryorder = "array", categoryarray =arrange(totals_mean,prumerny_plat)$kap_name),
         yaxis = list(title = "Prumerne platy"),
         barmode="stack") %>%
  layout(hovermode = 'compare')
saveWidget(as_widget(p), "data-output/bar_prumer_platy_2020.html")


#časové řasy - nárust metriky - spojit s čím



#srovnání schvaleny X skutecny plat

sum %>% filter(typ_rozpoctu == "SKUT") %>%
  ggplot(aes(x = rok, y = skut_k_rozp)) + geom_line() + facet_wrap(~name)+ggtitle("Srovnání indexu skutečného a upraveného platu")



sum_all<- sum %>%
  select(name,rok,typ_rozpoctu,prostredky_na_platy_a_oppp,pocet_zamestnancu) %>%
  reshape(timevar = "typ_rozpoctu",
          idvar = c("name","rok"), direction = "wide") %>%
  rename("schvaleny" = prostredky_na_platy_a_oppp.SCHV,
         "upraveny" = prostredky_na_platy_a_oppp.UPRAV,
         "skutecnost" = prostredky_na_platy_a_oppp.SKUT,
         "zam_schvaleny" = pocet_zamestnancu.SCHV,
         "zam_upraveny" = pocet_zamestnancu.UPRAV ,
         "zam_skutecnost" = pocet_zamestnancu.SKUT,
         "type" = name,
         "year" = rok) %>%
  mutate(rozdil_k_rozp = skutecnost - schvaleny,
         rozdil_ke_skut = schvaleny-skutecnost) %>%
  group_by(type)%>%
  mutate(plat_growth =  (skutecnost - dplyr::lag(skutecnost))/lag(skutecnost)*100,
         zam_growth =  (zam_skutecnost -  dplyr::lag(zam_skutecnost))/lag(zam_skutecnost)*100)



ts_growth<- sum_all %>% filter(!type %in% c("ROPO","SS","OSS","SS_bez_SOBCPO")) %>% group_by(type) %>%
  plot_ly(x=~year,y=~plat_growth,type = "scatter",color=~type, mode="lines+markers",
          text = ~paste(" Rok: ", year, "<br>","Zdroj:", type,
                        "<br>", " Rozpocet:", format(plat_growth, big.mark = ' '),
                        "<br>", " Rozdil ke schvalenemu:", format(plat_growth, big.mark = ' ')),
          hoverinfo = "text") %>%
  add_trace(y = ~zam_growth,mode = 'lines+markers', linetype = I("dot"),
            alpha = 0.4,showlegend = F,
            text = ~paste(" Rok:", year, "<br>","Zdroj:",
                          type,"<br>", "Rozpocet:", format(zam_growth, big.mark = ' '),
                          "<br>", "Rozdil ke skutecnosti:",
                          format(zam_growth, big.mark = ' ')),hoverinfo = "text")

ts_growth
lty <- c(schvaleny = "dash", skutecnost = "solid")

ts<- sum_all %>% filter(!type %in% c("ROPO","SS","OSS","SS_bez_SOBCPO")) %>% group_by(type) %>%
  plot_ly(x=~year,y=~skutecnost,type = "scatter",color=~type, mode="lines+markers", linetypes = lty,
          text = ~paste(" Rok: ", year, "<br>","Zdroj:", type,
                        "<br>", " Rozpocet:", format(skutecnost, big.mark = ' '),
                        "<br>", " Rozdil ke schvalenemu:", format(rozdil_k_rozp, big.mark = ' ')),
          hoverinfo = "text") %>%
  layout(title="Porovnani schvalenych a skutecnych platu",
         xaxis = list(title="Rok"),
         yaxis = list(title = "Rozpocet")) %>%
  add_trace(y = ~schvaleny,mode = 'lines+markers', linetype = I("dot"),
            alpha = 0.4,showlegend = F,
            text = ~paste(" Rok:", year, "<br>","Zdroj:",
                          type,"<br>", "Rozpocet:", format(schvaleny, big.mark = ' '),
                          "<br>", "Rozdil ke skutecnosti:",
                          format(rozdil_ke_skut, big.mark = ' ')),hoverinfo = "text")
saveWidget(as_widget(ts), "data-output/ts_schval_skut.html")
ts
## Srovnání růstu zaměstnanců s růstem platů


