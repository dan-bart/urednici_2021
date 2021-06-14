#################################
######## Validace platů #########
#################################
library(statnipokladna)
library(dplyr)
library(stringr)
prev_dt<-readRDS("./data-interim/objemy_pocty_scraped_raw_2012_2018.rds")
new_dt <- readRDS("./data-interim/sections.rds")


#check from monitor

#for years 2015 and later
finm_join_new<-sp_get_codelist("finmisto") %>% rename("finmisto" = "finmisto_id")

for (y in 2013:2014){
  print(paste("Checking year:",y))
  #join the dataset on kap_num
  monitor_dt <- sp_get_table("budget-central-old", y) %>%
    filter (ico != "00000000") %>%
    mutate(kapitola = as.numeric(str_sub(kapitola,2,4)))

  #check whether the wages (platy: 501x) for each kapitola match the new data
  KAPS <- new_dt %>% filter(name == "OSS", !is.na(kap_num)) %>% distinct(kap_num) %>% pull()
  for (k in KAPS){
    if (!k %in% monitor_dt$kapitola){
      print(k)
      next
    }
    subset_new <- new_dt %>% filter(name == "OSS",kap_num == k,rok == y) %>% select(typ_rozpoctu,prostredky_na_platy)

    subset_monitor<-monitor_dt %>%
      filter(kapitola == k) %>%
      filter(str_sub(polozka,1,3) %in% c("501")) %>%
      select(c(budget_adopted,budget_amended,budget_spending)) %>%
      colSums(na.rm = TRUE)

    if(is.null(subset_monitor)){print(k)}
    if(is.null(subset_new)){print(k)}

    schv_new <- subset_new %>% filter(typ_rozpoctu == "SCHV") %>% select (prostredky_na_platy)
    uprav_new <- subset_new %>% filter(typ_rozpoctu == "UPRAV") %>% select (prostredky_na_platy)
    skut_new <- subset_new %>% filter(typ_rozpoctu == "SKUT") %>% select (prostredky_na_platy)
    if (subset_monitor["budget_adopted"] != schv_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SCHV","Monitor:",subset_monitor["budget_adopted"],"Export:",schv_new))
    }
    if (subset_monitor["budget_amended"] != uprav_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: UPRAV","Monitor:",subset_monitor["budget_amended"],"Export:",uprav_new))
    }
    if (subset_monitor["budget_spending"] != skut_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SKUT","Monitor:",subset_monitor["budget_spending"],"Export:",skut_new))
    }
  }
}

