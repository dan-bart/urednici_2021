source("R/compare-adjusted.R")

isvs_rok <- 2023
isvs_rok2 <- str_sub(as.character(isvs_rok), 3, 4)
current_year <- year(Sys.Date())

if (isvs_rok %in% c(current_year, current_year - 1)) {
  pv_pha_urls <- pv_list_reg()
}  else {
  pv_pha_urls <- pv_list_reg(isvs_rok)
}


pv_pha_lastyr <- pv_pha_urls |>
  filter(str_detect(name, paste0("Pra_", isvs_rok2, "4"))) |>
  select(url, name)

map2(pv_pha_lastyr$url, file.path("data-input/ispv", pv_pha_lastyr$name),
     download.file)

pv_edu_pg <- pv_bind(
  pv_reg_monthlypay_education(paste0("data-input/ispv/Pra_",
                                     isvs_rok2, "4_mzs.xlsx")),
  pv_reg_monthlypay_education(paste0("data-input/ispv/Pra_",
                                     isvs_rok2, "4_pls.xlsx"))
) |> pv_fix_totals()

write_rds(pv_edu_pg, "data-interim/pv_edu_pg.rds")
