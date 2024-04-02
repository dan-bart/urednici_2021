dt <- readRDS("./data-interim/sections.rds")

dt |>
  filter(rok == 2022, typ_rozpoctu == "SKUT", kategorie_2014_cz == "Ministerstva") |>
  summarise(x = weighted.mean(wage_to_general, pocet_zamestnancu)) |>
  pull(x)


dt |>
  filter(rok == 2022, typ_rozpoctu == "SKUT", kategorie_2014_cz == "Ministerstva") |>
  select(kap_name, wage_to_general, pocet_zamestnancu)
