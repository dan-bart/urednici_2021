library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(tidyr)
library(here)
library(stringr)
library(janitor)
source(here::here('R', 'listSheets.R'))
source(here::here('R', 'divideMain.R'))



#load new data
input<-read_excel_allsheets("./data-input/data_2021.xls")
sapply(input,ncol)
sapply(input,nrow)


# divide sheets to groups with similar format
main_sheets <- c("ROPO CELKEM","OSS (RO)","PO","OOSS","STÁTNÍ SPRÁVA","ÚO","OSS SS","SOBCPO")
sub_sheets <- c("ZAMCI_5011_platy", "VOJACI_5012","ST_ZAMCI_5013","ST_ZASTUP_5014","UC_S_5022")
jednotl_sheets <-c("SOBCPO  JEDNOTLIVY","OSS SS - jednotl")

main_names <- vector(mode = "list")
polozky_names<-vector(mode = "list")
jednotl_names<-vector(mode = "list")

for (name in names(input)) {
  print(name)
  if (name %in% main_sheets){
    main_names<-append(main_names, input[name])
  }
  else if(name %in% sub_sheets){
    polozky_names<-append(polozky_names, input[name])
  }
  else if(name %in% jednotl_sheets){
    jednotl_names<-append(jednotl_names, input[name])
  }
}

#load excel data into clean dataframe


#main sheets
section_names <- c("Rozpocet_a_rok","full_name","kap_num","kap_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                   "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")
main_df<-data.frame(matrix(ncol =  length(section_names), nrow = 0))
colnames(main_df)<-section_names

for (i in 1:length(main_names)){
  res <- divide_sections(main_names[[i]],names(main_names[i]),section_names)
  main_df <- rbind(main_df, res)
}


#sheets with various types of polozky
polozky<-data.frame(matrix(ncol =  length(section_names), nrow = 0))
colnames(polozky)<-section_names

for (i in 1:length(polozky_names)){
  res <- divide_sections(polozky_names[[i]],names(polozky_names[i]),section_names)
  polozky <- rbind(polozky, res)
}

#sheets with subgroups
source(here::here('R', 'divideMain.R'))

jednotl_section_names <- c("Rozpocet_a_rok","full_name","kap_num","kap_name","Organizace","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                           "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")
jednotl_df<-data.frame(matrix(ncol =  length(jednotl_section_names), nrow = 0))
for (i in 1:length(jednotl_names)){
  res <- divide_jednotl(jednotl_names[[i]],names(jednotl_names[i]),jednotl_section_names)
  jednotl_df <- rbind(jednotl_df, res)
}


#summary sheet
summary_names <- c("Rozpocet_a_rok","full_name","Prostředky na platy a OPPP", "OPPP", "Prostředky na platy",
                   "Počet zaměstnanců", "Průměrný plat", "Pořadí průměrného platu","Schv ke schv", "Skut k rozp", "Skut ke skut")

summary <- divide_summary(input[[length(input)]],"Summary",summary_names)


#save all dataframes
saveRDS(main_df, file = "./data-interim/sections.rds")
saveRDS(jednotl_df, file = "./data-interim/jednotlivci.rds")
saveRDS(polozky, file = "./data-interim/organizace.rds")
saveRDS(summary, file = "./data-interim/summary.rds")
