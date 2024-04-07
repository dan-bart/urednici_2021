library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(purrr)
library(stringr)
library(ggplot2)
library(lubridate)
library(forcats)

options(scipen = 100)

szu_names <- c("kap_kod", "kap_zkr", "kap_name",
               "skut_min", "rozp_schv", "rozp_pzm", "skut",
               "plneni_proc", "plneni_rozdil", "skut_skut",
               "rok", "list", "tabulka", "file")

szu_excel_sheets_listed <- tibble(file = list.files("data-input/szu/",
                                                    pattern = "\\.xls$",
                                                    full.names = TRUE)) |>
  bind_rows(tibble(file = list.files("data-input/szu-eklep/",
                                     pattern = "\\.xls$",
                                     full.names = TRUE))) |>
  mutate(rok = str_extract(file, "20[012][0-9]"),
         sheets = map(file, readxl::excel_sheets)) |>
  unnest(cols = sheets) |>
  mutate(col_names = list(szu_names))

names(szu_excel_sheets_listed)

szu_excel_sheets_listed |>
  filter(str_detect(sheets, "([Tt]ab1[01])|(Str\\.)")) |>
  count(rok, file)

read_szu_excel <- function(path, sheet, year, col_names) {
  print(year)
  print(path)
  row1 <- read_excel(path, sheet, range = "A1:J3", col_names = FALSE) |>
    janitor::remove_empty("cols") |>
    janitor::remove_empty("rows") |>
    janitor::clean_names() |>
    gather()
  print(row1)
  tabulka_c <- case_when(any(str_detect(row1$value, "10")) ~ "10",
                         any(str_detect(row1$value, "11")) ~ "11",
                         .default = "none")
  dta <- read_excel(path, sheet, skip = ifelse(year < 2023, 11, 7)) |>
    janitor::remove_empty(which = "rows") |>
    janitor::remove_empty(which = "cols") |>
    mutate(rok = year, list = sheet,
           tabulka = tabulka_c,
           file = basename(path),)

  print(col_names)
  col_names_new <- col_names[col_names != "kap_zkr"]
  print(col_names_new)
  real_names <- if(year < 2022) col_names else col_names_new
  print(real_names)

  print(head(dta))

  dta1 <- dta |>
    rlang::set_names(real_names)

  if(!"kap_zkr" %in% names(dta1)) dta1 <- dta1 |> mutate(kap_zkr = NA_character_)

  return(dta1)

}


load_szu_sheet <- function(sheet_tibble, detection_string) {
  sheet_data <- sheet_tibble |>
    mutate(file_hash = tools::md5sum(file)) |>
    filter(str_detect(sheets, detection_string), str_detect(sheets, "Tab1[10]|Str")) |>
    distinct(file_hash, sheets, rok, .keep_all = TRUE) |>
    select(path = file, sheet = sheets, year = rok, col_names)

  dta0 <- pmap_dfr(sheet_data, read_szu_excel) |>
    janitor::clean_names() |>
    group_by(rok, list, file) |>
    mutate(promenna = case_match(tabulka,
                                 "11" ~ "objem",
                                 "10" ~ "pocet",
                                 .default = "none")) |>
    ungroup()

  print(dta0)

  dta <- dta0 |>
    drop_na(kap_kod) |>
    mutate(skut_min = if_else(rok < 2013 & promenna == "objem", skut_min * 1000, skut_min),
           rozp_schv = if_else(rok < 2013 & promenna == "objem", rozp_schv * 1000, rozp_schv),
           rozp_pzm = if_else(rok < 2013 & promenna == "objem", rozp_pzm * 1000, rozp_pzm),
           skut = if_else(rok < 2013 & promenna == "objem", skut * 1000, skut),
           rok = as.integer(rok),
           kap_name = if_else(is.na(kap_name), kap_zkr, kap_name)) |>
    rows_update(read_csv("data-input/kapitoly.csv"), by = "kap_kod") |>
    relocate(rok, .after = kap_name) |>
    mutate(kap_mini = str_detect(kap_name, "^Minis|Úřad [Vv]lád"),
           date = make_date(rok, 1, 1))

  return(dta)
}

szu_uo <- load_szu_sheet(szu_excel_sheets_listed, "ÚO")
szu_oss_sum <- load_szu_sheet(szu_excel_sheets_listed, "OSS sum")
# load_szu_sheet(szu_excel_sheets, "PO sum")
# load_szu_sheet(szu_excel_sheets, "ST.SPRÁVA")

szu_oss_sum |>
  count(promenna)

szu_uo |>
  count(tabulka, rok) |>
  arrange(desc(tabulka))

szu_oss_sum |>
  count(rok, kap_kod, promenna) |>
  count(n)

szu_uo |>
  count(rok, wt = is.na(kap_name))

szu_uo |>
  group_by(rok, kap_kod, promenna) |>
  filter(n() > 1) |>
  arrange(kap_kod)

head(szu_uo)

# source("scripts/inflation.R")
deflators <- read_rds("data-interim/inflace.rds")
# source("scripts/wages.R")
wages <- read_rds("data-interim/wages-later.rds")

szu_make_wide <- function(data, df_infl, wages_later) {
  data_2009 <- data |>
    filter(rok == 2010) |>
    mutate(rok = 2009,
           skut = skut_min,
           skut_min = NA,
           plneni_proc = NA,
           plneni_rozdil = NA,
           rozp_schv = NA, rozp_pzm = NA, skut_skut = NA)

  data |>
    bind_rows(data_2009) |>
    pivot_wider(id_cols = c(kap_kod, kap_name, rok, promenna, kap_zkr, kap_mini),
                values_from = c(matches("^skut|^rozp|^plneni")), names_from = promenna, names_glue = "{promenna}_{.value}") |>
    left_join(df_infl, by = join_by(rok)) |>
    left_join(wages_later, by = join_by(rok))
}

szu_oss_sum_wide <- szu_make_wide(szu_oss_sum, deflators, wages)
szu_uo_wide <- szu_make_wide(szu_uo, deflators, wages)

# szu_uo_wide |>
szu_uo_wide |>
  mutate(plat = objem_skut/pocet_skut/12,
         kap_zkr = as.factor(kap_zkr) |> fct_relevel("Celkem", after = 0)) |>
  drop_na(kap_zkr) |>
  filter(plat != 0, !kap_zkr %in% c("NSA", "ÚS")) |>
  ggplot(aes(rok, plat/1e3 * ceny_deflator_2023 / phasal_all * 1e3,
             # size = kap_zkr == "Celkem",
             group = kap_kod, colour = kap_mini)) +
  geom_hline(yintercept = 1, colour = "darkgrey") +
  # geom_line(aes(y = phasal_all/1e3 * base_2023), color = "grey") +
  facet_wrap(~kap_zkr) +
  geom_line(alpha = .5) +
  scale_x_continuous(breaks = seq(from = 2011, to = 2023, by = 2), limits = c(2011, 2023)) +
  ptrr::scale_y_percent_cz() +
  ptrr::theme_ptrr(multiplot = TRUE, gridlines = "both")

szu_uo_wide |>
  filter(is.na(kap_name)) |> View()
count(kap_kod)

szu_uo_wide |>
  filter(rok == max(rok)) |>
  ggplot(aes(pocet_skut, kap_zkr)) +
  geom_col()

range(szu_uo_wide$rok)

szu_uo |>
  distinct(kap_kod, kap_zkr, kap_name) |>
  format_csv() |> cat()

dt_orig <- arrow::read_parquet("data-input/szu/data_all.parquet")

dt_orig |>
  count(faze_rozpoctu)
