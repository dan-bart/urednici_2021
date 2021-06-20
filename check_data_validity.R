#################################
######## Validace platů #########
#################################
library(statnipokladna)
library(dplyr)
library(stringr)
library(tidyr)
library(tibble)
prev_dt<-readRDS("./data-interim/objemy_pocty_scraped_raw_2012_2018.rds")
new_dt <- readRDS("./data-interim/sections.rds")

#check from monitor
validity_check_monitor <- function(y, typ_vydaju = "platy"){
  if(!typ_vydaju %in% c("platy","oppp","platy_oppp")){
    stop("Invalid typ_vydaju, please use one of these options: platy, oppp, platy_oppp")
  }

  if(y > 2020 | y < 2013){
    stop("Invalid year, years available range from 2013 to 2020")
  }


  if(y %in% c(2013,2014)){
    monitor_dt <-sp_get_table("budget-central-old", y)
  } else{
    monitor_dt <-sp_get_table("budget-central", y)
  }

  new_dt_colname <- case_when(typ_vydaju == "platy" ~ "prostredky_na_platy",
                             typ_vydaju == "oppp" ~ "oppp",
                             typ_vydaju == "platy_oppp" ~ "prostredky_na_platy_a_oppp")
  polozka_vector <- case_when(typ_vydaju == "platy" ~ c("501"),
                              typ_vydaju == "oppp" ~ c("502"),
                              typ_vydaju == "platy_oppp" ~ c("501","502"))

  print(paste("Checking year:",y))
  #join the dataset on kap_num

  monitor_dt <- monitor_dt %>%
    filter (ico != "00000000") %>%
    mutate(kapitola = as.numeric(str_sub(kapitola,2,4)))

  #check whether the wages (platy: 501x) for each kapitola match the new data
  KAPS <- new_dt %>% filter(name == "OSS", !is.na(kap_num)) %>% distinct(kap_num) %>% pull()
  missing_KAPS <- data.frame(kap_num = numeric(0),kap_name = character(0))
  for (k in KAPS){
    if (!k %in% monitor_dt$kapitola){
      names <- paste(unique(new_dt[which(new_dt$kap_num == k),"kap_name"]), collapse = '-')
      missing_KAPS[nrow(missing_KAPS)+1,] <- c(k,names)
      next
    }
    subset_new <- new_dt %>% filter(name == "OSS",kap_num == k,rok == y) %>% select(typ_rozpoctu,new_dt_colname)

    subset_monitor<-monitor_dt %>%
      filter(kapitola == k) %>%
      filter(str_sub(polozka,1,3) %in% polozka_vector) %>%
      select(c(budget_adopted,budget_amended,budget_spending)) %>%
      colSums(na.rm = TRUE)

    schv_new <- subset_new %>% filter(typ_rozpoctu == "SCHV") %>% select (new_dt_colname) %>% pull()
    uprav_new <- subset_new %>% filter(typ_rozpoctu == "UPRAV") %>% select (new_dt_colname)%>% pull()
    skut_new <- subset_new %>% filter(typ_rozpoctu == "SKUT") %>% select (new_dt_colname)%>% pull()

    if (subset_monitor["budget_adopted"] != schv_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SCHV",
                  "Monitor:",format(round(subset_monitor["budget_adopted"], 10), nsmall = 10),
                  "Export:",format(round(schv_new, 10), nsmall = 10)))
    }
    if (subset_monitor["budget_amended"] != uprav_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: UPRAV",
                  "Monitor:",format(round(subset_monitor["budget_amended"], 10), nsmall = 10),
                  "Export:",format(round(uprav_new, 10), nsmall = 10)))
    }
    if (subset_monitor["budget_spending"] != skut_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SKUT",
                  "Monitor:",format(round(subset_monitor["budget_spending"], 10), nsmall = 10),
                  "Export:",format(round(skut_new, 10), nsmall = 10)))
    }
  }
  if(nrow(missing_KAPS) == 0){
    print("No missing kapitoly in monitor data")
  } else{
    print("Missing kapitoly in monitor data:")
    print(missing_KAPS)
  }
}


for (y in 2013:2020){
  validity_check_monitor(y,typ_vydaju = "oppp")
}



#check with previous data

#first we check whether both dataframes have identical kap_nums
prev_kaps<-prev_dt %>% distinct(kap_num) %>% pull() %>% as.numeric() %>% na.omit()
new_kaps<-new_dt %>% distinct(kap_num) %>% pull() %>% as.numeric() %>% na.omit()
setdiff(new_kaps,prev_kaps) #362 missing in the old data


#now we reshape the new data from long format to wide (to match old data)
dt_cost <- new_dt %>%
  select(name,kap_name,kap_num,rok,typ_rozpoctu,prostredky_na_platy_a_oppp) %>%
  reshape(timevar = "typ_rozpoctu",
                              idvar = c("name","kap_num","kap_name","name","rok"), direction = "wide") %>%
  mutate("indicator" = "cost") %>%
  rename("rozp" = prostredky_na_platy_a_oppp.SCHV,
         "upraveny" = prostredky_na_platy_a_oppp.UPRAV ,
         "skutecnost" = prostredky_na_platy_a_oppp.SKUT,
         "type" = name,
         "year" = rok)

dt_count <- new_dt %>%
  select(name,kap_name,kap_num,rok,typ_rozpoctu,pocet_zamestnancu) %>%
  reshape(timevar = "typ_rozpoctu",
          idvar = c("name","kap_num","kap_name","name","rok"), direction = "wide") %>%
  mutate("indicator" = "count")%>%
  rename("rozp" = pocet_zamestnancu.SCHV,
         "upraveny" = pocet_zamestnancu.UPRAV ,
         "skutecnost" = pocet_zamestnancu.SKUT,
         "type" = name,
         "year" = rok)


#now we standardize both datasets to make them comparable
new_dt_reshaped <-rbind(dt_cost,dt_count) %>%
  filter(year >= 2013, year <= 2018) %>% #other years not included in previous data
  as_tibble() %>%
  filter(type != "ROPO") %>% #not in our the old dataset, but computable from OSS and PO
  filter(kap_num %in% prev_kaps) #leave out the 362 - Narodni sportovni agentura

prev_dt_match<-prev_dt %>%
  select(colnames(new_dt_reshaped)) %>%
  filter(!is.na(as.numeric(kap_num))) %>%
  filter(type != "Příslušníci a vojáci") %>% #not in the main dataset
  mutate(kap_num = as.integer(kap_num),
         year = as.integer(year)) %>%
  mutate(type = case_when( #standardize types to those in new the dataframe
    type == "ÚO" ~ "UO",
    type %in% c("jedn. OSS státní správy","jedn. OSSstátní správy") ~ "OSS_SS",
    type == "SOBCPO" ~ "SOBCPO",
    type == "ST.SPRÁVA" ~ "SS",
    type == "ostatní OSS" ~ "OOSS",
    type == "OSS sum"~ "OSS",
    type == "PO sum" ~ "PO"))

prev_dt %>% filter(!is.na(as.numeric(kap_num))) %>% distinct(type)

head(prev_dt_match)
head(new_dt_reshaped)

new_dt_reshaped %>% select(type, kap_name,kap_num) %>% sapply(unique)
prev_dt_match %>% select(type, kap_name,kap_num) %>% sapply(unique)


#now we discover the differences

diffs<-function(row,prev_df = prev_dt_match,ukazatel = "cost"){
  corresponding<-prev_df %>%
    filter(indicator == ukazatel) %>%
    filter(kap_num == pull(row["kap_num"]),type == pull(row["type"]),year == pull(row["year"]))
  if(corresponding["rozp"] != row["rozp"]){
    print(corresponding["rozp"])
    print(row["rozp"])

  }
}

diffs(new_dt_reshaped[1,],prev_dt_match,"cost")
sapply(new_dt_reshaped,diffs)


validity_check_prev <- function(y){

  if(y > 2018 | y < 2013){
    stop("Invalid year, years available range from 2013 to 2018")
  }

  print(paste("Checking year:",y, "Employee count"))

  #check whether the wages (platy: 501x) for each kapitola match the new data
  prev_kaps<-prev_dt %>% distinct(kap_num) %>% pull() %>% as.numeric() %>% na.omit()
  KAPS<-new_dt %>% distinct(kap_num) %>% pull() %>% as.numeric() %>% na.omit()
  missing_KAPS <- c()



  for (k in KAPS){
    if (!k %in% prev_kaps){
      missing_KAPS <- append(missing_KAPS,k)
      next
    }


    subset_monitor<-monitor_dt %>%
      filter(kapitola == k) %>%
      filter(str_sub(polozka,1,3) %in% polozka_vector) %>%
      select(c(budget_adopted,budget_amended,budget_spending)) %>%
      colSums(na.rm = TRUE)

    schv_new <- subset_new %>% filter(typ_rozpoctu == "SCHV") %>% select (new_dt_colname) %>% pull()
    uprav_new <- subset_new %>% filter(typ_rozpoctu == "UPRAV") %>% select (new_dt_colname)%>% pull()
    skut_new <- subset_new %>% filter(typ_rozpoctu == "SKUT") %>% select (new_dt_colname)%>% pull()

    if (subset_monitor["budget_adopted"] != schv_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SCHV",
                  "Monitor:",format(round(subset_monitor["budget_adopted"], 10), nsmall = 10),
                  "Export:",format(round(schv_new, 10), nsmall = 10)))
    }
    if (subset_monitor["budget_amended"] != uprav_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: UPRAV",
                  "Monitor:",format(round(subset_monitor["budget_amended"], 10), nsmall = 10),
                  "Export:",format(round(uprav_new, 10), nsmall = 10)))
    }
    if (subset_monitor["budget_spending"] != skut_new){
      print(paste("Year:", y, "Kapitola:", k, "Budget: SKUT",
                  "Monitor:",format(round(subset_monitor["budget_spending"], 10), nsmall = 10),
                  "Export:",format(round(skut_new, 10), nsmall = 10)))
    }
  }
  if(length(missing_KAPS) == 0){
    print("No missing kapitoly in monitor data")
  } else{
    print("Missing kapitoly in monitor data:")
    print(missing_KAPS)
  }
}


for (y in 2013:2020){
  validity_check_monitor(y,typ_vydaju = "oppp")
}



test_df %>% filter(rok == 2015, kap_num == 335) %>% select(name,prostredky_na_platy_a_oppp.SCHV,prostredky_na_platy_a_oppp.UPRAV,prostredky_na_platy_a_oppp.SKUT)
prev_dt %>% filter(year == 2015, kap_num == 335)
new_dt %>% filter (rok == 2015, kap_num == 335)
