
divide_sections <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","kap_num","poradi_prumerneho_platu","pocet_zamestnancu")
  res = data.frame()

  #řádek obsahující "CELKEM" je poslední řádek, který chceme sbírat
  last_val<-which(grepl("C E L K E M",df$...3))
  df <- df[1:last_val,]

  for (i in 3:ncol(df)){
    #hledáme začátek další sekce
    if(is.na(df[1,i]) || grepl("INDEX",df[1,i])) {next}
    # upravený rozpočet 2021 ještě v datech není, takže ho nepotřebujeme zpracovat
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])) {break}
    ncols = 1 #počet sloupců vymezujících danou sekci
    for (j in i+1:ncol(df)){
      if (is.na(df[1,j]) || grepl("INDEX",df[1,j]))
      {
        ncols = 1+ncols
      }
      else{ #narazili jsme na název další sekce
        break
      }
    }

    upper_index = i+ncols-1

    section <- df[6:nrow(df),c(1,2,i:upper_index)]
    section[nrow(section),2]<-section[nrow(section)-1,1]
    section[nrow(section),1]<-NA
    section<-section[-c(nrow(section)-1),]


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
  res<- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names()
  # res[, c(num_names,int_names)][is.na(res[,c(num_names,int_names)])] <- 0
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  return(res)
}


divide_summary <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","poradi_prumerneho_platu","pocet_zamestnancu")
  res = data.frame()
  for (i in 3:ncol(df)){
    #hledáme začátek další sekce
    if(is.na(df[1,i]) || grepl("INDEX",df[1,i])) {next}
    # upravený rozpočet 2021 ještě v datech není, takže ho nepotřebujeme zpracovat
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])){break}
    ncols = 1 #počet sloupců vymezujících danou sekci
    for (j in i+1:ncol(df)){
      if (is.na(df[1,j]) || grepl("INDEX",df[1,j]))
      {
        ncols = 1+ncols
      }
      else{ #narazili jsme na název další sekce
        break
      }
    }
    upper_index = i+ncols-1
    section <- df[6:nrow(df),c(2,i:upper_index)]

    #SKUTEČNOST YYYY má na konci porovnání indexu platu k rozpracovanému a ke skutečnému platu minulého roku
    if(grepl("SKUT",df[1,i]) && ncol(section) == 10){
      section<-section[,-10]
      section<-add_column(section, NA, .after = 7)
    }

    #SCHV má porovnání se schváleným rozpočtem minulého roku
    else if(grepl("SCHV",df[1,i]) && ncol(section) == 8){
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
    }
    #UPRAVENY rozpočet nemá porovnání
    else if(grepl("UPRAV",df[1,i]) && ncol(section) == 7){
      section<-add_column(section, NA, .after = 7)
      section<-add_column(section, NA, .after = 8)
      section<-add_column(section, NA, .after = 9)
    }
    else{
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1,i])
    }

    section<-cbind(a = df[1,i], section)
    colnames(section) <- section_names
    res<- rbind(res, section)
  }
  res<- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names()
  #res[, c(num_names,int_names)][is.na(res[,c(num_names,int_names)])] <- 0
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  return(res)
}
