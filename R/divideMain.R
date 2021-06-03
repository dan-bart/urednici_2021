divide_sections <- function(df, sheet_name, section_names){
  res = data.frame()

  #řádek obsahující "CELKEM" je poslední řádek, který chceme sbírat
  last_val<-which(grepl("C E L K E M",df$...3))
  df <- df[1:last_val,]

  for (i in 3:ncol(df)){
    #hledáme začátek další sekce
    if(is.na(df[1,i]) || grepl("INDEX",df[1,i])) {next}
    # upravený rozpočet 2021 ještě v datech není, takže ho nepotřebujeme zpracovat
    if(grepl("2021",df[1,i]) && grepl("UPRAV",df[1,i])) {return(res)}
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
    section[nrow(section),1]<-section[nrow(section)-1,1]
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
  return(res)
}
