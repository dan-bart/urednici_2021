library(dplyr)
library(ggplot2)

dt <- readRDS("./data-interim/sections.rds")
sum <- readRDS("./data-interim/summary.rds")


#počet zaměstannců v tisících - odpovídá tabulce na https://petrbouchal.xyz/urednici/2_explore-counts.html
sum %>%
  filter(typ_rozpoctu == "SKUT") %>%
  mutate(pocet_zamestnancu = pocet_zamestnancu/1e3) %>%
  ggplot(aes(x = rok, y = pocet_zamestnancu)) + geom_line() + facet_wrap(~name, scales = "free") +ggtitle("Vývoj počtu zaměstnanců v tisících")

#srovnání skutečný X upravený plat

sum %>% filter(typ_rozpoctu == "SKUT") %>%
  ggplot(aes(x = rok, y = skut_k_rozp)) + geom_line() + facet_wrap(~name)+ggtitle("Srovnání indexu skutečného a upraveného platu")


# platy celkem - ROPO SCHV 2021
dt %>% filter(typ_rozpoctu == "SCHV",name == "ROPO", rok == "2021") %>%
  ggplot(aes(x = as.character(kap_num),y=prumerny_plat)) + geom_bar(stat="identity") + ggtitle("Srovnání schválených platů jednotlivých kapitol")
