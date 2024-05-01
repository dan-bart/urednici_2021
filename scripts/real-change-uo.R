dta_uo_sub |>
  filter(rok %in% 2021:2023, kap_zkr == "MV") |>
  arrange(rok) |>
  mutate(x = czsal_all * ceny_deflator_2023/first(czsal_all * ceny_deflator_2023)) |>
  select(rok, x)

dta_uo_all_mean |>
  filter(rok %in% 2020:2023) |>
  arrange(rok) |>
  mutate(x = prumerny_plat_c2023/first(prumerny_plat_c2023) - 1) |>
  select(rok, x)
