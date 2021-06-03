rm(list=ls())

library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
source(here::here('R', 'listSheets.R'))
source(here::here('R', 'divideMain.R'))

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
# zbytek přidáme


#sheets that are in similar format
uni_names <- c("ROPO CELKEM","OSS (RO)","PO","OOSS","STÁTNÍ SPRÁVA","ÚO","OSS SS",
               "SOBCPO","ZAMCI_5011_platy","VOJACI_5012","ST_ZAMCI_5013","ST_ZÁSTUP_5014")
input_uni <- vector(mode = "list")
for (name in names(input)) {
  print(name)
  if (name %in% uni_names){
    input_uni<-append(input_uni, input[name])
  }
}

#every information that we will gather from the source excel file
section_names <- c("Rozpocet_a_rok","Skupina","kap_num","kap_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                   "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")
main_df<-data.frame(matrix(ncol =  length(section_names), nrow = 0))
colnames(main_df)<-section_names



for (i in 1:length(input_uni)){
  res <- divide_sections(input_uni[[i]],names(input_uni[i]),section_names)
  main_df <- rbind(main_df, res)
}

main_df<-main_df %>% separate(Rozpocet_a_rok, into = c("Rozpocet", "Rok"), sep = -4, remove = TRUE)
saveRDS(main_df, file = "./data-interim/main_df.rds")

