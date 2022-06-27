
# loads and clean the main sections of input data
divide_sections <- function(df, sheet_name, section_names) {
  num_names <- c("prostredky_na_platy_a_oppp", "oppp", "prostredky_na_platy", "prumerny_plat", "schv_ke_schv", "skut_k_rozp", "skut_ke_skut")
  int_names <- c("rok", "kap_num", "poradi_prumerneho_platu", "pocet_zamestnancu")
  kap_num <- c(
    301, 302, 303, 304, 306, 307, 308, 309, 312, 313, 314, 315, 317, 321, 322, 327, 328, 329, 333, 334, 335, 336, 343, 344, 345, 346, 348, 349,
    353, 355, 358, 359, 361, 362, 371, 372, 373, 374, 375, 376, 377, 378, 381
  )
  full_kap_name <- c(
    "kancelar_prezidenta", "parlament", "kancelar_senatu", "urad_vlady", "ministerstvo_zahranicnich_veci", "ministerstvo_obrany",
    "narodni_bezpecnostni_urad", "kancelar_verejneho_ochrance_prav", "ministerstvo_financi", "ministerstvo_prace_a_socialnich_veci",
    "ministerstvo_vnitra", "ministerstvo_zivotniho_prostredi", "ministerstvo_pro_mistni_rozvoj", "grantova_agentura",
    "ministerstvo_prumyslu_a_obchodu", "ministerstvo_dopravy", "cesky_telekomunikacni_urad", "ministerstvo_zemedelstvi",
    "ministerstvo_skolstvi_mladeze_a_telovychovy", "ministerstvo_kultury", "ministerstvo_zdravotnictvi", "ministerstvo_spravedlnosti",
    "urad_pro_ochranu_osobnich_udaju", "urad_prumysloveho_vlastnictvi", "cesky_statisticky_urad", "cesky_urad_zememericky_a_katastralni",
    "cesky_bansky_urad", "energeticky_regulacni_urad", "ministerstvo_pro_hospodarskou_soutez", "ustav_pro_studium_totalitnich_rezimu",
    "ustavni_soud", "urad_narodni_rozpoctove_rady", "akademie_ved", "narodni_sportovni_agentura",
    "urad_pro_dohled_nad_hospodarenim_politickych_stran_a_politickych_hnuti", "rada_pro_rozhlasove_a_televizni_vysilani",
    "urad_pro_pristup_k_dopravni_infrastrukture", "sprava_statnich_hmotnych_rezerv", "statni_urad_pro_jadernou_bezpecnost",
    "generalni_inspekce_bezpecnostnich_sboru", "technologicka_agentura_cr", "narodni_urad_pro_kybernetickou_a_informacni_bezpecnost",
    "nejvyssi_kontrolni_urad"
  )
  cz_kap_name <- c(
    "Kancel\u00E1\u0159 prezidenta",
    "Parlament",
    "Kancel\u00E1\u0159 Sen\u00E1tu",
    "\u00DA\u0159ad vl\u00E1dy",
    "Ministerstvo zahrani\u010Dn\u00EDch v\u011Bc\u00ED",
    "Ministerstvo obrany                                      ",
    "N\u00E1rodn\u00ED bezpe\u010Dnostn\u00ED \u00FA\u0159ad",
    "Kancel\u00E1\u0159 ve\u0159ejn\u00E9ho ochr\u00E1nce pr\u00E1v",
    "Ministerstvo financ\u00ED                       ",
    "Ministerstvo pr\u00E1ce a soci\u00E1ln\u00EDch v\u011Bc\u00ED",
    "Ministerstvo vnitra                                ",
    "Ministerstvo \u017Eivotn\u00EDho prost\u0159ed\u00ED",
    "Ministerstvo pro m\u00EDstn\u00ED rozvoj",
    "Grantov\u00E1 agentura",
    "Ministerstvo pr\u016Fmyslu a obchodu",
    "Ministerstvo dopravy",
    "\u010Cesk\u00FD telekomunika\u010Dn\u00ED \u00FA\u0159ad",
    "Ministerstvo zem\u011Bd\u011Blstv\u00ED",
    "Ministerstvo \u0161kolstv\u00ED, ml\u00E1de\u017Ee a t\u011Blov\u00FDchovy",
    "Ministerstvo kultury",
    "Ministerstvo zdravotnictv\u00ED",
    "Ministerstvo spravedlnosti",
    "\u00DA\u0159ad pro ochranu osobn\u00EDch \u00FAdaj\u016F",
    "\u00DA\u0159ad pr\u016Fmyslov\u00E9ho vlastnictv\u00ED",
    "\u010Cesk\u00FD statistick\u00FD \u00FA\u0159ad                               ",
    "\u010Cesk\u00FD \u00FA\u0159ad zem\u011Bm\u011B\u0159ick\u00FD a katastr\u00E1ln\u00ED",
    "\u010Cesk\u00FD b\u00E1\u0148sk\u00FD \u00FA\u0159ad",
    "Energetick\u00FD regula\u010Dn\u00ED \u00FA\u0159ad",
    "Ministerstvo pro hospod\u00E1\u0159skou sout\u011B\u017E",
    "\u00DAstav pro studium totalitn\u00EDch re\u017Eim\u016F",
    "\u00DAstavn\u00ED soud",
    "\u00DA\u0159ad N\u00E1rodn\u00ED rozpo\u010Dtov\u00E9 rady",
    "Akademie v\u011Bd",
    "N\u00E1rodn\u00ED sportovn\u00ED agentura",
    "\u00DA\u0159ad pro dohled nad hospoda\u0159en\u00EDm politick\u00FDch stran a politick\u00FDch hnut\u00ED",
    "Rada pro rozhlasov\u00E9 a televizn\u00ED vys\u00EDl\u00E1n\u00ED",
    "\u00DA\u0159ad pro p\u0159\u00EDstup k dopravn\u00ED infrastruktu\u0159e",
    "Spr\u00E1va st\u00E1tn\u00EDch hmotn\u00FDch rezerv",
    "St\u00E1tn\u00ED \u00FA\u0159ad pro jadernou bezpe\u010Dnost",
    "Gener\u00E1ln\u00ED inspekce bezpe\u010Dnostn\u00EDch sbor\u016F",
    "Technologick\u00E1 agentura \u010CR",
    "N\u00E1rodn\u00ED \u00FA\u0159ad pro kybernetickou a informa\u010Dn\u00ED bezpe\u010Dnost",
    "Nejvy\u0161\u0161\u00ED kontroln\u00ED \u00FA\u0159ad"
  )
  names_df <- data.frame(kap_num, full_kap_name, cz_kap_name)
  res <- data.frame()

  # leave out rows with unwanted commentary at the end of the data
  last_val <- which(grepl("C E L K E M", df$...3))
  df <- df[1:last_val, ]

  for (i in 3:ncol(df)) {
    # searching for the start of the next category
    if (is.na(df[1, i]) || grepl("INDEX", df[1, i])) {
      next
    }
    ncols <- 1 # count the number of columns belonging to this section
    for (j in i + 1:ncol(df)) {
      if (!is.null(df[1, j]) && (is.na(df[1, j]) || grepl("INDEX", df[1, j]))) {
        ncols <- 1 + ncols
      } else { # next section beginning
        break
      }
    }

    upper_index <- i + ncols - 1
    section <- df[6:nrow(df), c(1, 2, i:upper_index)]
    # treating the last summary row
    section[nrow(section), 2] <- section[nrow(section) - 1, 1]
    section[nrow(section), 1] <- NA
    section <- section[-c(nrow(section) - 1), ]
    # there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    # the sections end with index comparison to previous years, we treat each section differently
    if (grepl("SKUT", df[1, i]) && ncol(section) %in% 10:11) {
      if(ncol(section == 11)) section <- section[, -11]
      section <- add_column(section, NA, .after = 8)
    } else if (grepl("SCHV", df[1, i]) && ncol(section) == 9) {
      section <- add_column(section, NA, .after = 9)
      section <- add_column(section, NA, .after = 10)
    } else if (grepl("UPRAV", df[1, i]) && ncol(section) == 8) {
      section <- add_column(section, NA, .after = 8)
      section <- add_column(section, NA, .after = 9)
      section <- add_column(section, NA, .after = 10)
    }
    # no other name or length expected
    else {
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1, i])
    }

    # additional descriptive columns
    section <- cbind(a = sheet_name, section)
    section <- cbind(a = df[1, i], section)
    # finally assign the colnames and merge with the main dataframe
    colnames(section) <- section_names
    res <- rbind(res, section)
    if(is.na(df[1, i]) && is.na(df[1, i + 1])) {
      break
    }
  }


  # cleaning the data
  res <- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names() %>%
    mutate(aux_names = iconv(res$full_name, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(name = case_when(
      aux_names == "UO" ~ "UO",
      aux_names == "OSS SS" ~ "OSS_SS",
      aux_names == "SOBCPO" ~ "SOBCPO",
      aux_names == "STATNI SPRAVA" ~ "SS",
      aux_names == "OOSS" ~ "OOSS",
      aux_names == "OSS (RO)" ~ "OSS",
      aux_names == "PO" ~ "PO",
      aux_names == "ROPO CELKEM" ~ "ROPO",
      aux_names == "ZAMCI_5011_platy" ~ "ZAMCI_5011_platy",
      aux_names == "VOJACI_5012" ~ "VOJACI_5012",
      aux_names == "ST_ZAMCI_5013" ~ "ST_ZAMCI_5013",
      aux_names == "ST_ZASTUP_5014" ~ "ST_ZASTUP_5014",
      aux_names == "UC_S_5022" ~ "UC_S_5022",
      aux_names == "SOBCPO  JEDNOTLIVY" ~ "SOBCPO_JEDNOTL",
      aux_names == "OSS SS - jednotl" ~ "OSS_SS_JEDNOTL"
    )) %>%
    select(-c("aux_names", "full_name")) %>%
    mutate(kap_name = ifelse(str_detect(iconv(kap_name, from = "UTF-8", to = "ASCII//TRANSLIT"), "UDHPSH|UPDSH|UDHPS", negate = FALSE), "UDHPS", kap_name)) %>%
    mutate(kap_num = as.numeric(kap_num)) %>%
    left_join(names_df, by = "kap_num")
  res[num_names] <- sapply(res[num_names], as.numeric)
  res["pocet_zamestnancu"] <- sapply(res["pocet_zamestnancu"], as.numeric)
  res["pocet_zamestnancu"] <- sapply(res["pocet_zamestnancu"], round,digits = 0)
  res[int_names] <- sapply(res[int_names], as.integer)
  res$typ_rozpoctu[which(grepl("SKUT", res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV", res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV", res$typ_rozpoctu))] <- "UPRAV"

  return(res)
}


# loads and clean the sections of input data containing detailed granularity
divide_jednotl <- function(df, sheet_name, section_names) {
  num_names <- c("prostredky_na_platy_a_oppp", "oppp", "prostredky_na_platy", "prumerny_plat", "schv_ke_schv", "skut_k_rozp", "skut_ke_skut")
  int_names <- c("rok", "kap_num", "poradi_prumerneho_platu", "pocet_zamestnancu")
  res <- data.frame()

  # applying the fill function
  aux <- df %>%
    select(1, 2) %>%
    fill(1, 2)
  df[1:nrow(df) - 1, c(1, 2)] <- aux[(1:nrow(aux) - 1), ]

  for (i in 4:ncol(df)) {
    # searching for the start of the next category
    if (is.na(df[1, i]) || grepl("INDEX", df[1, i])) {
      next
    }
    ncols <- 1 # count the number of columns belonging to this section
    for (j in i + 1:ncol(df)) {
      if (!is.null(df[1, j]) && (is.na(df[1, j]) || grepl("INDEX", df[1, j]))) {
        ncols <- 1 + ncols
      } else { # next section beginning
        break
      }
    }

    upper_index <- i + ncols - 1
    section <- df[6:nrow(df), c(1, 2, 3, i:upper_index)]
    # there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    # the sections end with index comparison to previous years, we treat each section differently

    if (grepl("SKUT", df[1, i]) && ncol(section) %in% 11:12) {
      if(ncol(section) == 12) section <- section[, -12] # empty column
      section <- add_column(section, NA, .after = 9)
    } else if (grepl("SCHV", df[1, i])) {
      # OSS SS - jednotl: chybí index prumerného platu za schv.rozpoctem každého roku
      if (sheet_name == "OSS SS - jednotl" && ncol(section) == 9) {
        section <- add_column(section, NA, .after = 9)
        section <- add_column(section, NA, .after = 10)
        section <- add_column(section, NA, .after = 11)
      } else if (sheet_name == "SOBCPO  JEDNOTLIVY" && ncol(section) == 10) {
        section <- add_column(section, NA, .after = 11)
        section <- add_column(section, NA, .after = 12)
      } else {
        print(dim(section))
        print(head(section))
        print(sheet_name)
        print(df[1, i])
      }
    } else if (grepl("UPRAV", df[1, i]) && ncol(section) == 9) {
      section <- add_column(section, NA, .after = 9)
      section <- add_column(section, NA, .after = 10)
      section <- add_column(section, NA, .after = 11)
    }
    # no other name or length expected
    else {
      print(dim(section))
      print(head(section))
      print(sheet_name)
      print(df[1, i])
    }

    # additional descriptive columns
    section <- cbind(a = sheet_name, section)
    section <- cbind(a = df[1, i], section)


    # finally assign the colnames and merge with the main dataframe
    colnames(section) <- section_names
    res <- rbind(res, section)
  }


  # cleaning the data
  res <- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names() %>%
    mutate(aux_names = iconv(res$full_name, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(name = case_when(
      aux_names == "UO" ~ "UO",
      aux_names == "OSS SS" ~ "OSS_SS",
      aux_names == "SOBCPO" ~ "SOBCPO",
      aux_names == "STATNI SPRAVA" ~ "SS",
      aux_names == "OOSS" ~ "OOSS",
      aux_names == "OSS (RO)" ~ "OSS",
      aux_names == "PO" ~ "PO",
      aux_names == "ROPO CELKEM" ~ "ROPO",
      aux_names == "ZAMCI_5011_platy" ~ "ZAMCI_5011_platy",
      aux_names == "VOJACI_5012" ~ "VOJACI_5012",
      aux_names == "ST_ZAMCI_5013" ~ "ST_ZAMCI_5013",
      aux_names == "ST_ZASTUP_5014" ~ "ST_ZASTUP_5014",
      aux_names == "UC_S_5022" ~ "UC_S_5022",
      aux_names == "SOBCPO  JEDNOTLIVY" ~ "SOBCPO_JEDNOTL",
      aux_names == "OSS SS - jednotl" ~ "OSS_SS_JEDNOTL",
    )) %>%
    select(-"aux_names", "full_name")
  res[num_names] <- sapply(res[num_names], as.numeric)
  res[int_names] <- sapply(res[int_names], as.integer)
  res$typ_rozpoctu[which(grepl("SKUT", res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV", res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV", res$typ_rozpoctu))] <- "UPRAV"
  return(res)
}





# slightly different procedure used for loading the SUMMARY data
divide_summary <- function(df, sheet_name, section_names) {
  num_names <- c("prostredky_na_platy_a_oppp", "oppp", "prostredky_na_platy", "prumerny_plat", "schv_ke_schv", "skut_k_rozp", "skut_ke_skut")
  int_names <- c("rok", "poradi_prumerneho_platu", "pocet_zamestnancu")

  res <- data.frame()
  for (i in 3:ncol(df)) {
    # searching for the start of the next category
    if (is.na(df[1, i]) || grepl("INDEX", df[1, i])) {
      next
    }
    ncols <- 1 # count the number of columns belonging to this section
    for (j in i + 1:ncol(df)) {
      if (!is.null(df[1, j]) && (is.na(df[1, j]) || grepl("INDEX", df[1, j]))) {
        ncols <- 1 + ncols
      } else { # next section beginning
        break
      }
    }

    upper_index <- i + ncols - 1
    section <- df[6:nrow(df), c(2, i:upper_index)]

    # there are three types of section: SKUTECNY ROZPOCET, SCHVALENY ROZPOCET, UPRAVENY ROZPOCET
    # the sections end with index comparison to previous years, we treat each section differently
    if (grepl("SKUT", df[1, i]) && ncol(section) %in% 9:10) {
      if(ncol(section) == 10) section <- section[, -10]
      section <- add_column(section, NA, .after = 7)
    } else if (grepl("SCHV", df[1, i]) && ncol(section) == 8) {
      section <- add_column(section, NA, .after = 8)
      section <- add_column(section, NA, .after = 9)
    } else if (grepl("UPRAV", df[1, i]) && ncol(section) == 7) {
      section <- add_column(section, NA, .after = 7)
      section <- add_column(section, NA, .after = 8)
      section <- add_column(section, NA, .after = 9)
    }
    # no other name or length expected
    else {
      print(ncol(section))
      print(head(section))
      print(sheet_name)
      print(df[1, i])
    }
    # additional descriptive columns
    section <- cbind(a = df[1, i], section)
    # finally assign the colnames and merge with the main dataframe
    colnames(section) <- section_names
    res <- rbind(res, section)
  }

  # cleaning the data
  res <- res %>%
    separate(Rozpocet_a_rok, into = c("typ_rozpoctu", "rok"), sep = -4, remove = TRUE) %>%
    clean_names() %>%
    mutate(aux_names = iconv(res$full_name, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
    mutate(name = case_when(
      aux_names == "Ustredni organy statni spravy (UO)" ~ "UO",
      aux_names == "OSS -  Statni sprava" ~ "OSS_SS",
      aux_names == "Sprava ve sloz. Obrany, bezpecnost. Celni a pravni ochrany (SOBCPO)" ~ "SOBCPO",
      aux_names == "Statni sprava" ~ "SS",
      aux_names == "Statni sprava bez SOBCPO" ~ "SS_bez_SOBCPO",
      aux_names == "Ostatni organizacni slozky statu (OOSS)" ~ "OOSS",
      aux_names == "Organizacni slozky statu (OSS)" ~ "OSS",
      aux_names == "Prispevkove organizace (PO)" ~ "PO",
      aux_names == "OSS A PO CELKEM" ~ "ROPO"
    )) %>%
    select(-"aux_names", "full_name")
  res[num_names] <- sapply(res[num_names], as.numeric)
  res[int_names] <- sapply(res[int_names], as.integer)
  res$typ_rozpoctu[which(grepl("SKUT", res$typ_rozpoctu))] <- "SKUT"
  res$typ_rozpoctu[which(grepl("SCHV", res$typ_rozpoctu))] <- "SCHV"
  res$typ_rozpoctu[which(grepl("UPRAV", res$typ_rozpoctu))] <- "UPRAV"

  return(res)
}
