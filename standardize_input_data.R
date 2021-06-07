library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
library(janitor)
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
# jednotlivci - musíme si odsouhlasit jak to zpracovat

#sheets that are in similar format
main_sheets <- c("ROPO CELKEM","OSS (RO)","PO","OOSS","STÁTNÍ SPRÁVA","ÚO","OSS SS","SOBCPO")
main_names <- vector(mode = "list")
for (name in names(input)) {
  print(name)
  if (name %in% main_sheets){
    main_names<-append(main_names, input[name])
  }
}

#TBD: jak využít data z jednotlivých skupin (popis na https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/28)
subgroup_sheets<-c("ZAMCI_5011_platy","VOJACI_5012","ST_ZAMCI_5013","ST_ZÁSTUP_5014",
                   "ÚČ_S_5022","SOBCPO  JEDNOTLIVÝ","OSS SS - jednotl")


#every information that we will gather from the source excel file
section_names <- c("Rozpocet_a_rok","full_name","kap_num","kap_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                   "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")
main_df<-data.frame(matrix(ncol =  length(section_names), nrow = 0))
colnames(main_df)<-section_names

for (i in 1:length(main_names)){
  res <- divide_sections(main_names[[i]],names(main_names[i]),section_names)
  main_df <- rbind(main_df, res)
}

#also load and clean the summary for all sections
summary_names <- c("Rozpocet_a_rok","full_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                   "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")

summary <- divide_summary(input[[length(input)]],"Summary",summary_names)


#save both dataframes
saveRDS(main_df, file = "./data-interim/sections.rds")
saveRDS(summary, file = "./data-interim/summary.rds")



