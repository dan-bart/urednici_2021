wages_later <- czso_get_table("110080", force_redownload = TRUE) %>%
  filter(is.na(POHLAVI_txt), is.na(SPKVANTIL_txt), uzemi_kod %in% c(19, 3018)) %>%
  select(rok, hodnota, uzemi_kod) %>%
  spread(key = uzemi_kod, value = hodnota) %>%
  rename("czsal_all" = 2, "phasal_all" = 3) |>
  rows_append(tibble(rok = 2023, czsal_all = 43341, phasal_all = 53347))

wages_early <- read_csv("data-input/legacy/czsalaries.csv") |>
  select(rok = Year, czsal_all, phasal_all) |>
  mutate(rok = year(rok)) |>
  filter(rok < 2011)

wages <- bind_rows(wages_early, wages_later)

write_rds(wages, "data-interim/wages.rds")
