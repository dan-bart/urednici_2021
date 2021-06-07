# loads and clean the main sections of input data
divide_sections <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","kap_num","poradi_prumerneho_platu","pocet_zamestnancu")
  res = data.frame()

  #leave out rows with unwanted commentary at the end of the data
  last_val<-which(grepl("C E L K E M",df$...3))
  df <- df[1:last_val,]

  for (i in 3:ncol(df)){
    #searching for the start of the next category
    if(is.na(df[1,i]) || grepl("INDEX",df[1,i])) {next}
    # last section, that has not been provided, we do not want it in the main dataframe
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])) {break}
    ncols = 1 # count the number of columns belonging to this section
    for (j in i+1:ncol(df)){
      if (is.na(df[1,j]) || grepl("INDEX",df[1,j]))
      {
        ncols = 1+ncols
      }
      else{ #next section beginning
        break
      }
    }

    upper_index = i+ncols-1
    section <- df[6:nrow(df),c(1,2,i:upper_index)]
    #treating the last summary row
    section[nrow(section),2]<-section[nrow(section)-1,1]
    section[nrow(section),1]<-NA
    section<-section[-c(nrow(section)-1),]
    #there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    #the sections end with index comparison to previous years, we treat each section differently
    if(grepl("SKUT",df[1,i]) && ncol(section) == 11){
      section<-section[,-11]
      section<-add_column(section, NA, .after = 8)
    }
    else if(grepl("SCHV",df[1,i]) && ncol(section) == 9){
      section<-add_column(section, NA, .after = 9)
      section<-add_column(section, NA, .after = 10)
    }
    else if(grepl("UPRAV",df[1,i]) && ncol(section) == 8){
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
      section<-add_column(section, NA, .after = 10)
    }
    #no other name or length expected
    else{
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1,i])
    }

    #additional descriptive columns
    section<-cbind(a = sheet_name, section)
    section<-cbind(a = df[1,i], section)
    #finally assign the colnames and merge with the main dataframe
    colnames(section) <- section_names
    res<- rbind(res, section)
  }

  #cleaning the data
  res <- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names() %>%
    mutate(aux_names = iconv(res$full_name,from="UTF-8",to="ASCII//TRANSLIT")) %>%
    mutate(name = case_when(
      aux_names == "UO" ~ "UO",
      aux_names == "OSS SS" ~ "OSS_SS",
      aux_names == "SOBCPO" ~ "SOBCPO",
      aux_names == "STATNI SPRAVA" ~ "SS",
      aux_names == "OOSS" ~ "OOSS",
      aux_names == "OSS (RO)"~ "OSS",
      aux_names == "PO" ~ "PO",
      aux_names == "ROPO CELKEM" ~ "ROPO")) %>%
    select(-"aux_names")
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  res$typ_rozpoctu[which(grepl("SKUT",res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV",res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV",res$typ_rozpoctu))] <- "UPRAV"
  return(res)
}


#slightly different procedure used for loading the SUMMARY data
divide_summary <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","poradi_prumerneho_platu","pocet_zamestnancu")

  res = data.frame()
  for (i in 3:ncol(df)){
    #searching for the start of the next category
    if(is.na(df[1,i]) || grepl("INDEX",df[1,i])) {next}
    # last section, that has not been provided, we do not want it in the main dataframe
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])){break}
    ncols = 1 # count the number of columns belonging to this section
    for (j in i+1:ncol(df)){
      if (is.na(df[1,j]) || grepl("INDEX",df[1,j]))
      {
        ncols = 1+ncols
      }
      else{ #next section beginning
        break
      }
    }

    upper_index = i+ncols-1
    section <- df[6:nrow(df),c(2,i:upper_index)]

    #there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    #the sections end with index comparison to previous years, we treat each section differently
    if(grepl("SKUT",df[1,i]) && ncol(section) == 10){
      section<-section[,-10]
      section<-add_column(section, NA, .after = 7)
    }
    else if(grepl("SCHV",df[1,i]) && ncol(section) == 8){
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
    }
    else if(grepl("UPRAV",df[1,i]) && ncol(section) == 7){
      section<-add_column(section, NA, .after = 7)
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
    }
    #no other name or length expected
    else{
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1,i])
    }
    #additional descriptive columns
    section<-cbind(a = df[1,i], section)
    #finally assign the colnames and merge with the main dataframe
    colnames(section) <- section_names
    res<- rbind(res, section)
  }

  #cleaning the data
  res<- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names() %>%
    mutate(aux_names = iconv(res$full_name,from="UTF-8",to="ASCII//TRANSLIT"))%>%
    mutate(name = case_when(
      aux_names == "Ustredni organy statni spravy (UO)" ~ "UO",
      aux_names == "OSS -  Statni sprava" ~ "OSS_SS",
      aux_names == "Sprava ve sloz. Obrany, bezpecnost. Celni a pravni ochrany (SOBCPO)" ~ "SOBCPO",
      aux_names == "Statni sprava" ~ "SS",
      aux_names == "Statni sprava bez SOBCPO" ~ "SS_bez_SOBCPO",
      aux_names == "Ostatni organizacni slozky statu (OOSS)" ~ "OOSS",
      aux_names == "Organizacni slozky statu (OSS)" ~ "OSS",
      aux_names == "Prispevkove organizace (PO)" ~ "PO",
      aux_names == "OSS A PO CELKEM" ~ "ROPO")) %>%
    select(-"aux_names")
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  res$typ_rozpoctu[which(grepl("SKUT",res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV",res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV",res$typ_rozpoctu))] <- "UPRAV"

  return(res)
}
