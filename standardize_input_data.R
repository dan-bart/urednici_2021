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
#load old data
chapters_old <- read.csv("~/urednici_2021/data-output/legacy/chapters_ALL.csv", encoding="UTF-8")
grp_old <- read.csv("~/urednici_2021/data-output/legacy/groups_ALL.csv", encoding="UTF-8")

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


#now we will extend the new datasets with data from previous years

#chapter dataframe
kap_slovnik <- main_df %>% select(kap_num,kap_name,full_kap_name) %>% filter(!is.na(kap_num)) %>% unique()
df_addition <- data.frame(c(347,338,341),c("KCP","MI","UVIS"),c("komise_pro_cenne_papiry","ministesrvto_informatiky","urad_pro_verejne_informacni_systemy"))
names(df_addition)<-c("kap_num","kap_name","full_kap_name")
kap_slovnik<-rbind(kap_slovnik,df_addition)


old_dt<-chapters_old %>% select(rok = Year, name = grp, kap_num = KapNum,variable,value) %>%
  mutate(typ_rozpoctu = case_when(str_detect(variable,"schvaleny") ~ "SCHV",
                                  str_detect(variable,"upraveny") ~ "UPRAV",
                                  str_detect(variable,"skutecnost") ~ "SKUT")) %>%
  mutate(variable = case_when(str_detect(variable,"schvaleny") ~ str_remove(variable, "_schvaleny"),
                                  str_detect(variable,"upraveny") ~ str_remove(variable, "_upraveny"),
                                  str_detect(variable,"skutecnost") ~ str_remove(variable, "_skutecnost"))) %>%
  filter(!is.na(typ_rozpoctu))%>% filter(!is.na(value)) %>% filter(name != "Exekutiva")%>%
  mutate(name = case_when( name == "SOBCPO" ~ "SOBCPO",
                          name == "St. sprava se SOBCPO" ~ "SS",
                          name == "UO - Ministerstva" ~ "UO",
                          name == "UO - Ostatní" ~ "UO",
                          name == "UO" ~ "UO",
                          name == "OSS-RO" ~ "OSS",
                          name == "OSS-SS"~"OSS_SS",
                          name == "PO"~"PO",
                          name == "OOSS"~"OOSS",
                          name =="ROPO celkem" ~ "ROPO")) %>%
  mutate(rok = substr(rok, 1, 4)) %>%
  reshape(timevar = "variable",idvar = c("rok","name","kap_num","typ_rozpoctu"), direction = "wide") %>%
  rename("prostredky_na_platy_a_oppp" = value.PlatyOPPP,
         "prostredky_na_platy" = "value.Platy",
         "oppp" = value.OPPP,
         "pocet_zamestnancu" =  value.Zam,
         "prumerny_plat" = value.AvgSal) %>%
  left_join(kap_slovnik,by = "kap_num") %>%
  mutate (skut_k_rozp = NA, schv_ke_schv = NA, skut_ke_skut = NA,poradi_prumerneho_platu = NA) %>%
  filter(rok != 2013)
unique(main_df$rok)

main_df<-rbind(main_df,old_dt)

#group dataframe

grp_slovnik <- summary %>% select(name,full_name) %>% unique()

old_dt<-grp_old %>% select(rok = Year, name = grp, variable,value) %>%
  mutate(typ_rozpoctu = case_when(str_detect(variable,"schvaleny") ~ "SCHV",
                                  str_detect(variable,"upraveny") ~ "UPRAV",
                                  str_detect(variable,"skutecnost") ~ "SKUT")) %>%
  mutate(variable = case_when(str_detect(variable,"schvaleny") ~ str_remove(variable, "_schvaleny"),
                              str_detect(variable,"upraveny") ~ str_remove(variable, "_upraveny"),
                              str_detect(variable,"skutecnost") ~ str_remove(variable, "_skutecnost"))) %>%
  filter(!is.na(typ_rozpoctu))%>% filter(!is.na(value)) %>% filter(name != "Exekutiva")%>%
  mutate(name = case_when( name == "SOBCPO" ~ "SOBCPO",
                           name == "St. sprava se SOBCPO" ~ "SS",
                           name == "UO - Ministerstva" ~ "UO",
                           name == "UO - Ostatní" ~ "UO",
                           name == "UO" ~ "UO",
                           name == "OSS-RO" ~ "OSS",
                           name == "OSS-SS"~"OSS_SS",
                           name == "PO"~"PO",
                           name == "OOSS"~"OOSS",
                           name =="ROPO celkem" ~ "ROPO")) %>%
  mutate(rok = substr(rok, 1, 4)) %>%
  reshape(timevar = "variable",idvar = c("rok","name","typ_rozpoctu"), direction = "wide") %>%
  rename("prostredky_na_platy_a_oppp" = value.PlatyOPPP,
         "prostredky_na_platy" = "value.Platy",
         "oppp" = value.OPPP,
         "pocet_zamestnancu" =  value.Zam,
         "prumerny_plat" = value.AvgSal) %>%
  left_join(grp_slovnik,by = "name") %>%
  mutate (skut_k_rozp = NA, schv_ke_schv = NA, skut_ke_skut = NA,poradi_prumerneho_platu = NA) %>%
  filter(rok != 2013)

summary<-rbind(summary,old_dt)

#save all dataframes
saveRDS(main_df, file = "./data-interim/sections.rds")
saveRDS(jednotl_df, file = "./data-interim/jednotlivci.rds")
saveRDS(polozky, file = "./data-interim/organizace.rds")
saveRDS(summary, file = "./data-interim/summary.rds")
