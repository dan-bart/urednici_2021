library(dplyr)
library(readxl)
library(janitor)
library(tidyr)
library(stringr)
library(purrr)

dta0 <- read_excel("data-input/data_2023.xlsx")

headers0 <- read_excel("data-input/data_2023.xlsx", col_names = FALSE) |>
  janitor::remove_empty("rows") |>
  select(-(1:4)) |>
  slice(1:3) |>
  mutate(row = row_number())

hh <- headers0 |>
  t() |>
  as_tibble(.name_repair = "unique") |>
  set_names(c("h1", "h2", "h3")) |>
  mutate(rok = str_extract(h1, "20[0-9]{2}"), across(everything(), tolower)) |>
  fill(rok, h1, h2, h3) |>
  mutate(h3 = if_else(str_detect(h2, "platy a oppp"), "platy a oppp", h3),
         across(everything(), \(x) str_remove_all(x, " v kč| \\:|prostředky na ")),
         var = if_else(h2 == "z toho", h3, h2),
         var = if_else(var == "ø měsíční plat", "prum_plat", var),
         var = if_else(var == "pořadí ø měsíčního platu", "poradi_prum_plat", var),
         var = if_else(str_detect(var, "skut\\. [0-9]{4} ke skut"), "skut_skut", var),
         var = if_else(str_detect(var, "skut\\. [0-9]{4} k uprav"), "skut_uprav", var),
         var = if_else(str_detect(var, "schv\\. [0-9]{4} ke schv"), "schv_schv", var),
         stage = str_extract(h1, "^index|^schv|^skut|^uprav|^poradi")) |>
  mutate(var = make_clean_names(var), .by = c(rok, stage),
         var_name = paste(stage, rok, var, sep = "_"))

var_ids <- c("kap_kod", "kap_zkr", "kap_name", "nic")
var_names <- c(var_ids, hh$var_name)[1:(length(hh$var_name) + 3)]

sheets0 <- excel_sheets("data-input/data_2023.xlsx")
sheets <- sheets0[!grepl("jednot|JEDNOT", sheets0)]

dta_all0 <- map_dfr(sheets, \(x) read_excel("data-input/data_2023.xlsx", sheet = x, col_names = var_names) |>
                     slice(-(1:5)) |>
                     mutate(sheet = tolower(x)))

tail(names(dta_all0))

unique(dta_all0$sheet)

dta_all <- dta_all0 |>
  mutate(across(c(everything(), -any_of(var_ids), -sheet), as.numeric)) |>
  remove_empty("cols") |>
  remove_empty("rows") |>
  filter(!(is.na(kap_kod) & is.na(kap_zkr))) |>
  relocate(sheet, .after = "kap_name")

dta_all |>
  filter(if_any(matches("index"), \(x) !between(x, 1, 300) & x != 0 & !is.na(x))) |>
  select(matches("index"))

dta_all |>
  filter(if_any(matches("pocet"), \(x) !between(x, 1, 1e6) & x != 0 & !is.na(x))) |>
  select(matches("pocet"))

dta_all |>
  filter(if_any(matches("[0-9]_prum_plat"), \(x) !between(x, 10000, 200000) & x != 0 & !is.na(x))) |>
  select(matches("prum_plat"))

dta_all |>
  filter(if_any(matches("[0-9]_platy"), \(x) !between(x, 5e5, 500 * 1e9) & x != 0 & !is.na(x))) |>
  # remove_empty("cols") |>
  select(sheet, starts_with("kap_"), matches("platy"))

dta_all |>
  filter(if_any(matches("poradi"), \(x) !between(x, 1, 100) & x != 0 & !is.na(x))) |>
  select(sheet, starts_with("kap_"), matches("poradi"))

dta_all |>
  distinct(kap_kod) |> pull()
