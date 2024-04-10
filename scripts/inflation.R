library(czso)
library(dplyr)
library(tidyr)
library(readr)

df_infl <- czso_get_table("010022", dest_dir = "data-input/czso", force_redownload = T) %>%
  filter(is.na(ucel_txt)) %>%
  filter(casz_txt == "stejné období předchozího roku") %>%
  group_by(rok) %>%
  summarise(ceny_index = mean(hodnota)) %>%
  select(contains("obdobi"),rok, ceny_index) %>%
  mutate(inflation=ceny_index/100)%>%
  arrange(rok)%>%
  filter(rok>=2003,rok<=2023)

df_infl$ceny_deflator_2003 <- 0
df_infl$ceny_deflator_2023 <- 0
df_infl[1, "ceny_deflator_2003"] <- 1
df_infl[nrow(df_infl), "ceny_deflator_2023"] <- 1


for (i in 2:nrow(df_infl)) {
  df_infl[i, "ceny_deflator_2003"] <- df_infl[i - 1, "ceny_deflator_2003"] * df_infl[i, "inflation"]
}


for (i in (nrow(df_infl) - 1):1) {
  df_infl[i, "ceny_deflator_2023"] <- df_infl[i + 1, "ceny_deflator_2023"] * df_infl[i+1, "inflation"]
}

df_infl
write_rds(df_infl, "data-interim/inflace.rds")
