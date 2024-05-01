library(ispv)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

pvlc23 <- pv_list_cr()
pvlp23 <- pv_list_reg()
pvlc22 <- pv_list_cr(2022)
pvlp22 <- pv_list_reg(2022)
pvlc21 <- pv_list_cr(2021)
pvlp21 <- pv_list_reg(2021)

pv_links_relev <- bind_rows(pvlc23, pvlc22, pvlp22, pvlp23,
                            pvlc21, pvlp21) |>
  filter(str_detect(name, "(Pra|CR)_2[1-3]4_(pls|mzs|PLS|MZS)\\."))

ispv_files <- map2_chr(pv_links_relev$url, pv_links_relev$name,
                       \(x, y) {
                         pth <- file.path("data-input/ispv", y)
                         download.file(x, pth)
                         return(pth)
                       })

ispv_all <- bind_rows(
  pv_reg_monthlypay_education("data-input/ispv/Pra_214_mzs.xlsx"),
  pv_reg_monthlypay_education("data-input/ispv/Pra_224_mzs.xlsx"),
  pv_reg_monthlypay_education("data-input/ispv/Pra_234_mzs.xlsx"),
  pv_reg_monthlypay_education("data-input/ispv/Pra_214_pls.xlsx"),
  pv_reg_monthlypay_education("data-input/ispv/Pra_224_pls.xlsx"),
  pv_reg_monthlypay_education("data-input/ispv/Pra_234_mzs.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_214_MZS.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_224_MZS.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_234_PLS.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_214_PLS.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_224_MZS.xlsx"),
  pv_cr_monthlypay_education("data-input/ispv/CR_234_PLS.xlsx")
) |> replace_na(list(kraj_id_ispv = "CR"))

ispv_all |>
  filter(str_detect(category, "CELKEM")) |>
  group_by(kraj_id_ispv, period) |>
  summarise(pay_mean = weighted.mean(pay_mean, wt = fte_thous))
