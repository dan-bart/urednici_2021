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
polozky_dt <- readRDS("./data-interim/organizace.rds")

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
  missing_KAPS <- data.frame(kap_num = numeric(0),kap_name = character(0)) #store data that are not present in monitor dataset
  for (k in KAPS){
    if (!k %in% monitor_dt$kapitola){
      names <- paste(unique(new_dt[which(new_dt$kap_num == k),"kap_name"]), collapse = '-')
      missing_KAPS[nrow(missing_KAPS)+1,] <- c(k,names)
      next
    }
    #select comparable subsets of data
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
setdiff(new_kaps,prev_kaps) #kapitola 362 missing in the old data

#since ROPO as a summary of OSS is not included in thje previous dataset, we want to check, that ROPO = OSS + PO for the new data
ROPO_dt <- new_dt %>% filter(name == "ROPO")
OSS_dt <- new_dt %>% filter(name == "OSS")
PO_dt <- new_dt %>% filter(name == "PO")

ROPO_check<-function(row,counter){
  # if(is.na(row["kap_num"])){
  #   print("NA in kap_num for row:")
  #   print(row)
  # }
  if(!is.na(row["kap_num"])){
    OSS <- OSS_dt %>% filter(rok == pull(row["rok"]) & typ_rozpoctu == pull(row["typ_rozpoctu"]) & kap_num == pull(row["kap_num"]))
    PO <-PO_dt %>% filter(rok == pull(row["rok"]) & typ_rozpoctu == pull(row["typ_rozpoctu"]) & kap_num == pull(row["kap_num"]))
    #print(OSS)
    OSS[is.na(OSS)] <- 0
    PO[is.na(PO)] <- 0
    row[is.na(row)] <- 0

    if(round(row["prostredky_na_platy_a_oppp"],0) != round(OSS["prostredky_na_platy_a_oppp"]+PO["prostredky_na_platy_a_oppp"],0)){
      counter=1+counter
      print(paste("Differences in wages for year:",row["rok"],"kap:",row["kap_num"],"budget:",row["typ_rozpoctu"]))
      print(row["prostredky_na_platy_a_oppp"])
      print(OSS["prostredky_na_platy_a_oppp"]+PO["prostredky_na_platy_a_oppp"])

    }

    if(row["pocet_zamestnancu"] != (OSS["pocet_zamestnancu"]+PO["pocet_zamestnancu"])){
      counter=1+counter
      print(paste("Differences in employee count:",row["rok"],"kap:",row["kap_num"],"budget:",row["typ_rozpoctu"]))
      print(row["pocet_zamestnancu"])
      print(OSS["pocet_zamestnancu"]+PO["pocet_zamestnancu"])
    }
  }
  return(counter)
}
counter = 0
for (i in 1:nrow(ROPO_dt)){
  counter <- ROPO_check(ROPO_dt[i,],counter)
}
print(counter)

#only differences seem to be in employee count, and they differ by 1 for all cases
#therefore, we can work with OSS and PO, and leave ROPO


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
  filter(!is.na(as.numeric(kap_num))) %>% #comments and totals not included
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


#observe the result
head(prev_dt_match)
head(new_dt_reshaped)
new_dt_reshaped %>% select(type, kap_name,kap_num) %>% sapply(unique)
prev_dt_match %>% select(type, kap_name,kap_num) %>% sapply(unique)


#now we observe the differences

diffs<-function(row,prev_df = prev_dt_match,ukazatel = "cost",counter){
  corresponding<-prev_df %>%
    filter(indicator == ukazatel) %>%
    filter(kap_num == pull(row["kap_num"]),type == pull(row["type"]),year == pull(row["year"]))
  if(nrow(corresponding) == 0){
    print(paste("missing data for year:",row["year"],"kap:",row["kap_num"],"type:",row["type"]))
  }
  else{
    row <- row %>% replace_na(list(rozp = 0,upraveny = 0,skutecnost = 0))
    corresponding <- corresponding %>% replace_na(list(rozp = 0,upraveny = 0,skutecnost = 0))
    if(abs(corresponding["rozp"]-row["rozp"])>1){ #check records which have difference greater than 1 (change to 0 for more precise result)
      print(corresponding)
      print(row)
      counter = counter + 1 #move this to count different things
    }
  }
  return(counter)
}


#check budgets and employee counts separately
new_dt_cost <-new_dt_reshaped %>% filter(indicator == "cost")
counter = 0
for (j in 1:nrow(new_dt_cost)){
  counter <- diffs(new_dt_cost[j,],prev_dt_match,"cost",counter)
}
print(counter)


new_dt_count <-new_dt_reshaped %>% filter(indicator == "count")
counter = 0
for (j in 1:nrow(new_dt_count)){
  counter <- diffs(new_dt_count[j,],prev_dt_match,"count",counter)
}
print(counter)

#90 out of 1764 in old dataframe have NAs for budgets while new data has values, all of it are ministries
#90 out of 1764 in old dataframe have NAs for employee counts while new data has values, all of it are ministries
#65 out of 1764 have counts that are off by less than 1, none has counts off by more
#0 out of 1764 have budgets that are off

#494 out of 1764 data points in the new dataset are not present in the old one
#0 data points from the old dataset are missing in the new dataset (done by switching the datasets in the function)



tt<-monitor_dt <-sp_get_table("budget-central", 2017)
