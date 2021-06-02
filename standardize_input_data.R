rm(list=ls())

library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
source(here::here('R', 'fun.R'))


### přehled

#předchozí
prev_dt<-readRDS("./data-interim/objemy_pocty_scraped_raw_2012_2018.rds")
colnames(prev_dt)
unique(prev_dt$kap_num)
unique(prev_dt$type)


#nový
input<-read_excel_allsheets("./data-input/data_2021.xls")
sapply(input,ncol)
sapply(input,nrow)
# OSS SS - jednotl: chybí index průměrného platu za schv.rozpočtem každého roku
# ÚČ_S_5022 a SUMÁŘ - jiný formát
# jednotlivci - musíme si odsouhlasit jak to zpracovat
#zbytek přidáme

uni_names <- c("ROPO CELKEM","OSS (RO)","PO","OOSS","STÁTNÍ SPRÁVA","ÚO","OSS SS",
               "SOBCPO","ZAMCI_5011_platy","VOJACI_5012","ST_ZAMCI_5013","ST_ZÁSTUP_5014")
input_uni <- vector(mode = "list")
for (name in names(input)) {
  print(name)
  if (name %in% uni_names){
    input_uni<-append(input_uni, input[name])
  }
}

test <- input_uni[[1]]

section_names <- c("Kategorie","Typ rozpočtu","kap_num","kap_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                  "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")
main_df<-data.frame(matrix(ncol =  length(section_names), nrow = 0))
colnames(main_df)<-section_names


divide_sections <- function(df, sheet_name){
  res = data.frame()
  last_val<-which(grepl("CELKEM",test$...1))-1
  df <- df[1:last_val,]
  for (i in 3:ncol(df)){
    if(is.na(df[1,i]) || df[1,i] == "INDEX Ø platu") {next}
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])) {return(res)}
    ncols = 1
    for (j in i+1:ncol(df)){
      if (is.na(df[1,j]) || df[1,j] == "INDEX Ø platu")
      {
        ncols = 1+ncols
      }
      else{
        break
      }
    }
    upper_index = i+ncols-1

    section <- df[6:nrow(df),c(1,2,i:upper_index)]

    #SKUTEČNOST YYYY má na konci porovnání indexu platu k rozpracovanému a ke skutečnému platu minulého roku
    if(grepl("SKUT",df[1,i]) && ncol(section) == 11){
      section<-section[,-11]
      section<-add_column(section, NA, .after = 8)
    }

    #SCHV má porovnání se schváleným rozpočtem minulého roku
    else if(grepl("SCHV",df[1,i]) && ncol(section) == 9){
      section<-add_column(section, NA, .after = 9)
      section<-add_column(section, NA, .after = 10)
    }
    #UPRAVENY rozpočet nemá porovnání
    else if(grepl("UPRAV",df[1,i]) && ncol(section) == 8){
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
      section<-add_column(section, NA, .after = 10)
    }
    else{
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1,i])
    }

    section<-cbind(a = sheet_name, section)
    section<-cbind(a = df[1,i], section)
    colnames(section) <- section_names
    res<- rbind(res, section)
  }
  return(res)
}

for (i in 1:length(input_uni)){
  res <- divide_sections(input_uni[[i]],names(input_uni[i]))
  main_df <- rbind(main_df, res)
}

saveRDS(main_df, file = "./data-interim/main_df.rds")

