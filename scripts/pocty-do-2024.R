library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(forcats)
library(readr)
library(tibble)
library(tidyr)
library(here)
library(janitor)
options(scipen = 100, digits = 8)

dta <- readRDS("./data-interim/sections.rds")

ddd <- dta %>%
  filter(typ_rozpoctu == "SCHV",
         kategorie_2014 %in% c("Ministerstva", "Neustredni st. sprava",
                               "Ostatni ustredni")) %>%
  filter(!kap_num %in% c(314)) %>%
  group_by(kategorie_2014_cz, rok) %>%
  summarise(pocet_zamestnancu = sum(pocet_zamestnancu), .groups = "drop") %>%
  group_by(rok) %>%
  mutate(pocet_zamestnancu_agg = sum(pocet_zamestnancu), note = NA_character_) |>
  ungroup() |>
  rows_append(tribble(~kategorie_2014_cz, ~rok, ~pocet_zamestnancu, ~note,
                     "Ministerstva", 2023, 27622, NA,
                     "Neústřední st. správa", 2023, 52488,NA,
                     "Ostatní ústřední", 2023, 4532,NA,
                     "Ministerstva", 2024, 27067, NA,
                     "Neústřední st. správa", 2024, 50931,NA,
                     "Ostatní ústřední", 2024, 4680,NA,
                     "Ministerstva", 2023, -14327, "MV korekce 2022+",
                     "Ministerstva", 2024, -13750, "MV korekce 2022+"
                     )) |>
  mutate(kategorie_2014_cz = as.factor(kategorie_2014_cz) |>
           fct_relevel("Ostatní ústřední", "Ministerstva", after = Inf)) |>
  count(rok, kategorie_2014_cz, wt = pocet_zamestnancu, name = "pocet_zamestnancu")

ddd |> filter(rok == 2021)


ddd |>
  filter(rok > 2011) |>
  ggplot(aes(rok, pocet_zamestnancu, alpha = rok != 2024)) +
  geom_col(aes(fill = kategorie_2014_cz)) +
  scale_alpha_discrete(range = c(.6, 1), guide = "none") +
  scale_x_continuous(n.breaks = 8, limits = c(2011.5, 2024.5)) +
  scale_fill_manual(values = c("goldenrod", "grey40", "darkblue")) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(add = c(0, 500)), n.breaks = 7,
                     label = scales::number) +
  theme(axis.title = element_blank(), legend.title = element_blank())

ddd |>
  count(rok, wt = pocet_zamestnancu) |>
  tail(n = 10)
