library(czso)

ci <- czso_get_table("010022", dest_dir = "data-input/czso")

unique(ci$casz_txt)

ci %>% filter(is.na(ucel_txt)) %>%
  filter(casz_txt == "stejné období předchozího roku") %>%
  group_by(rok) %>%
  summarise(hodnota = mean(hodnota)) %>%
  select(contains("obdobi"), hodnota, rok) %>%
  arrange(rok)
