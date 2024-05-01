library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(readr)
library(ggplot2)
library(lubridate)

kap_meta <- read_csv("data-input/kapitoly.csv")
dta_fin <- dta_all |>
  filter(sheet != "sumar") |>
  select(-kap_zkr, -kap_name) |>
  left_join(kap_meta, by = "kap_kod") |>
  pivot_longer(-c(starts_with("kap_"), sheet)) |>
  mutate(name = str_replace(name, "zamestancu", "zamestnancu")) |>
  separate_wider_regex(name, patterns = c(stage = "^[a-z]{4,5}", sep_1 = "\\_",
                                          rok = "20[0-9]{2}", sep_2 = "\\_",
                                          var = "(?<=[0-9]_)[a-z\\_]*$")) |>
  select(-starts_with("sep_")) |>
  mutate(kap_name = if_else(str_detect(tolower(kap_name), "(celkem)|(c e l k e m)"), "celkem", kap_name),
         kap_zkr = if_else(kap_name == "celkem" | sheet == "sumar", "TOT", kap_zkr),
         kap_kod = if_else(kap_name == "celkem" | sheet == "sumar", "999", kap_kod),
         rok = as.integer(rok)
         ) |>
  drop_na(value) |>
  # count(kap_kod, kap_zkr, kap_name, var, sheet, rok, stage, sort = TRUE)
  pivot_wider(names_from = c(stage, var), values_from = value) |>
  drop_na(kap_zkr) |>
  mutate(datum = make_date(rok),
         grp = case_when(sheet == "uo" & kap_vladni ~ "uo - ministerstva",
                         sheet == "uo" & !kap_vladni ~ "uo - ostatní",
                         .default = sheet))

dta_fin |>
  filter(sheet == "ropo celkem", kap_vladni, skut_prum_plat > 0) |>
  ggplot(aes(rok, skut_prum_plat, group = kap_name)) +
  geom_line() +
  facet_wrap(~kap_name)

names(dta_fin)
unique(dta_fin$sheet)

gdta_old0 <- read_csv("data-input/legacy/groups_ALL.csv")

reshape_old_data <- function(data, kap_meta) {
  has_kap_num <- "KapNum" %in% names(data)
  if(!has_kap_num) data$KapNum <- NA

  dta_old <- data |>
    select(datum = Year, grp, kap_kod = KapNum, variable, value) |>
    mutate(kap_kod = as.character(kap_kod)) |>
    separate_wider_delim(variable, delim = "_", names = c("var", "stage")) |>
    mutate(
      grp = tolower(grp),
      var = case_match(var,
                       "AvgSal" ~ "prum_plat",
                       "PlatyOPPP" ~ "platy_a_oppp",
                       "OPPP" ~ "oppp",
                       "Platy" ~ "platy",
                       "Zam" ~ "pocet_zamestnancu",
      ),
      stage = case_match(stage,
                         "skutecnost" ~ "skut",
                         "schvaleny" ~ "schv",
                         "upraveny" ~ "uprav", .default = stage,
      ),
      grp = case_match(grp,
                       "oss-ro" ~ "oss (ro)",
                       "oss-ss" ~ "oss ss",
                       "st. sprava se sobcpo" ~ "statni sprava",
                       "st. sprava se sobcpo" ~ "statni sprava",
                       .default = grp
                       )
    ) |>
    filter(stage %in% c("skut", "schv", "uprav"), grp != "exekutiva") |>
    mutate(var = paste(stage, var, sep = "_"),
           rok = year(datum)) |>
    select(-stage) |>
    pivot_wider(names_from = var, values_from = value)

  if(!has_kap_num) dta_old$kap_kod <- NULL
  if(has_kap_num) {

    dta_old <- dta_old |>
      left_join(kap_meta, by = "kap_kod")
  }
  return(dta_old)
}

dta_old0 <- read_csv("data-input/legacy/chapters_ALL.csv")
dta_old_reshaped <- reshape_old_data(dta_old0, kap_meta)
gdta_old0 <- read_csv("data-input/legacy/groups_ALL.csv")
gdta_old_reshaped <- reshape_old_data(gdta_old0, kap_meta)

unique(dta_fin$grp)
count(gdta_old_reshaped, grp)
count(dta_old_reshaped, grp)
unique(dta_old_reshaped$grp)

setdiff(unique(gdta_old_reshaped$grp), unique(dta_old_reshaped$grp))
setdiff(unique(dta_old_reshaped$grp), unique(gdta_old_reshaped$grp))
setdiff(unique(dta_fin$grp), unique(gdta_old_reshaped$grp))
setdiff(unique(gdta_old_reshaped$grp), unique(dta_fin$grp))

dta_both <- bind_rows(dta_old_reshaped |> filter(rok < 2013) |> mutate(src = "old"),
                      dta_fin |> mutate(src = "new"))

dta_both |>
  filter(grp %in% c("uo", "uo - ministerstva", "uo - ostatní"), skut_prum_plat > 0, !kap_ancient) |>
  ggplot(aes(rok, skut_prum_plat, group = kap_name)) +
  geom_line() +
  facet_wrap(~kap_zkr)

dta_both |>
  filter(kap_zkr %in% c("AV", "AVČR")) |>
  count(kap_zkr, src)

dta_both |>
  filter(is.na(kap_zkr)) |>
  count(kap_kod)

dta_both |>
  count(kap_zkr, kap_ancient)
