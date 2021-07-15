
# loads and clean the main sections of input data
divide_sections <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","kap_num","poradi_prumerneho_platu","pocet_zamestnancu")
  kap_num<-c(301,302,303,304,306,307,308,309,312,313,314,315,317,321,322,327,328,329,333,334,335,336,343,344,345,346,348,349,
             353,355,358,359,361,362,371,372,373,374,375,376,377,378,381)
  full_kap_name<-c("kancelar_prezidenta","parlament","kancelar_senatu","urad_vlady","ministerstvo_zahranicnich_veci","ministerstvo_obrany",
                   "narodni_bezpecnostni_urad","kancelar_verejneho_ochrance_prav","ministerstvo_financi","ministerstvo_prace_a_socialnich_veci",
                   "ministerstvo_vnitra","ministerstvo_zivotniho_prostredi","ministerstvo_pro_mistni_rozvoj","grantova_agentura",
                   "ministerstvo_prumyslu_a_obchodu","ministerstvo_dopravy","cesky_telekomunikacni_urad","ministerstvo_zemedelstvi",
                   "ministerstvo_skolstvi_mladeze_a_telovychovy","ministerstvo_kultury","ministerstvo_zdravotnictvi","ministerstvo_spravedlnosti",
                   "urad_pro_ochranu_osobnich_udaju","urad_prumysloveho_vlastnictvi","cesky_statisticky_urad","cesky_urad_zememericky_a_katastralni",
                   "cesky_bansky_urad","energeticky_regulacni_urad","ministerstvo_pro_hospodarskou_soutez","ustav_pro_studium_totalitnich_rezimu",
                   "ustavni_soud","urad_narodni_rozpoctove_rady","akademie_ved","narodni_sportovni_agentura",
                   "urad_pro_dohled_nad_hospodarenim_politickych_stran_a_politickych_hnuti","rada_pro_rozhlasove_a_televizni_vysilani",
                   "urad_pro_pristup_k_dopravni_infrastrukture","sprava_statnich_hmotnych_rezerv","statni_urad_pro_jadernou_bezpecnost",
                   "generalni_inspekce_bezpecnostnich_sboru","technologicka_agentura_cr","narodni_urad_pro_kybernetickou_a_informacni_bezpecnost",
                   "nejvyssi_kontrolni_urad")
  names_df<-data.frame(kap_num,full_kap_name)
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
      aux_names == "ROPO CELKEM" ~ "ROPO",
      aux_names == "ZAMCI_5011_platy" ~ "ZAMCI_5011_platy",
      aux_names == "VOJACI_5012" ~ "VOJACI_5012",
      aux_names == "ST_ZAMCI_5013"~ "ST_ZAMCI_5013",
      aux_names == "ST_ZASTUP_5014" ~ "ST_ZASTUP_5014",
      aux_names == "UC_S_5022" ~ "UC_S_5022",
      aux_names == "SOBCPO  JEDNOTLIVY" ~ "SOBCPO_JEDNOTL",
      aux_names == "OSS SS - jednotl" ~ "OSS_SS_JEDNOTL")) %>%
    select(-c("aux_names","full_name")) %>%
    mutate(kap_name = ifelse(str_detect(iconv(kap_name,from="UTF-8",to="ASCII//TRANSLIT"), "UDHPSH|UPDSH|UDHPS", negate = FALSE),"UDHPS",kap_name))%>%
    mutate(kap_num = as.numeric(kap_num)) %>%
    left_join(names_df,by="kap_num")
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  res$typ_rozpoctu[which(grepl("SKUT",res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV",res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV",res$typ_rozpoctu))] <- "UPRAV"
  return(res)
}


# loads and clean the sections of input data containing detailed granularity
divide_jednotl <- function(df, sheet_name, section_names){
  num_names<-c("prostredky_na_platy_a_oppp","oppp","prostredky_na_platy","prumerny_plat","schv_ke_schv","skut_k_rozp","skut_ke_skut")
  int_names<-c("rok","kap_num","poradi_prumerneho_platu","pocet_zamestnancu")
  res = data.frame()

  #applying the fill function
  aux<-df %>% select(1,2) %>% fill(1,2)
  df[1:nrow(df)-1,c(1,2)]<-aux[(1:nrow(aux)-1),]

  for (i in 4:ncol(df)){
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
    section <- df[6:nrow(df),c(1,2,3,i:upper_index)]
    #there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    #the sections end with index comparison to previous years, we treat each section differently

    if(grepl("SKUT",df[1,i]) && ncol(section) == 12){
      section<-section[,-12] #empty column
      section<-add_column(section, NA, .after = 9)
    } else if(grepl("SCHV",df[1,i])){
      # OSS SS - jednotl: chybí index průměrného platu za schv.rozpočtem každého roku
      if(sheet_name == "OSS SS - jednotl" && ncol(section) == 9){
        section<-add_column(section, NA, .after = 9)
        section<-add_column(section, NA, .after = 10)
        section<-add_column(section, NA, .after = 11)
      } else if(sheet_name == "SOBCPO  JEDNOTLIVY" && ncol(section) == 10) {
        section<-add_column(section, NA, .after = 11)
        section<-add_column(section, NA, .after = 12)
      } else{
        print(dim(section))
        print(head(section))
        print(sheet_name)
        print(df[1,i])
      }
    } else if(grepl("UPRAV",df[1,i]) && ncol(section) == 9){
      section<-add_column(section, NA, .after = 9)
      section<-add_column(section, NA, .after = 10)
      section<-add_column(section, NA, .after = 11)
    }
    #no other name or length expected
    else{
      print(dim(section))
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
      aux_names == "ROPO CELKEM" ~ "ROPO",
      aux_names == "ZAMCI_5011_platy" ~ "ZAMCI_5011_platy",
      aux_names == "VOJACI_5012" ~ "VOJACI_5012",
      aux_names == "ST_ZAMCI_5013"~ "ST_ZAMCI_5013",
      aux_names == "ST_ZASTUP_5014" ~ "ST_ZASTUP_5014",
      aux_names == "UC_S_5022" ~ "UC_S_5022",
      aux_names == "SOBCPO  JEDNOTLIVY" ~ "SOBCPO_JEDNOTL",
      aux_names == "OSS SS - jednotl" ~ "OSS_SS_JEDNOTL",
    )) %>%
    select(-"aux_names","full_name")
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
    select(-"aux_names","full_name")
  res[num_names]<-sapply(res[num_names],as.numeric)
  res[int_names]<-sapply(res[int_names],as.integer)
  res$typ_rozpoctu[which(grepl("SKUT",res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV",res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV",res$typ_rozpoctu))] <- "UPRAV"

  return(res)
}
