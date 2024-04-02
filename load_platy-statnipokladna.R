# remotes::install_github("petrbouchal/statnipokladna", force = TRUE)

library(statnipokladna)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

renv::restore()

options(statnipokladna.dest_dir = "data/budget_data")
orgs <- read_rds("data-interim/orgs.rds")

orgs_uo <- orgs |> filter(poddruhuj_nazev == "OSS - správce kapitoly")

rozp <- sp_get_table("budget-central", 2015:2023, 12)
names(rozp)

rozppp <- rozp |>
    filter(str_detect(polozka, "^50")) |>
    sp_add_codelist("polozka") |>
    sp_add_codelist(orgs) |>
    sp_add_codelist("kapitola") |>
    mutate(kapitola_nazev = str_squish(kapitola_nazev))

rozppp |>
    count(kon_pol)

rozpp <- rozppp |> 
    group_by(vykaz_year, vykaz_date, polozka, polozka_nazev, kapitola, 
             kapitola_nazev, orgs_nazev,
             druhuj_nazev, poddruhuj_nazev,
             ico, seskupeni, podseskupeni) |>
    summarise(across(starts_with("budget_"), sum), .groups = "drop")

names(rozpp)

rozpp |>
    filter(podseskupeni == "Platy") |> 
    count(polozka, polozka_nazev)

names(rozpp)

count(rozpp, seskupeni) |> filter(str_detect(seskupeni, "Plat"))

rozpp |>
    count(vykaz_year, wt = budget_spending)

rozp |>
    count(kon_pol)

rozpp_uo <- rozpp |>
    filter(str_detect(polozka, "^50")) |>
    # filter(seskupeni == "Platy a podobné a související výdaje") |>
    # filter(podseskupeni %in% c("Platy", "Ostatní platby za provedenou práci")) |>
    filter(podseskupeni %in% c("Platy")) |>
    filter(polozka != "5012") |>
    count(kapitola_nazev, druhuj_nazev,
        #   orgs_nazev,
        vykaz_year,
        poddruhuj_nazev,
        wt = budget_spending / 1e6
    ) |>
    filter(
        # str_detect(orgs_nazev, "Ministerstvo"),
        druhuj_nazev == "OSS",
        poddruhuj_nazev == "OSS - správce kapitoly",
        # orgs_uo_nazev == "Ministerstvo obrany",
        vykaz_year == 2021
    ) |>
    select(kapitola_nazev, n)


    ggplot(aes(vykaz_year, n/1e9, fill = polozka_nazev, group = polozka_nazev)) +
    geom_area() +
    facet_wrap(~kapitola_nazev)



rozpp |>
    filter(seskupeni == "Platy a podobné a související výdaje") |>
    filter(podseskupeni %in% c("Platy", "Ostatní platby za provedenou práci")) |>
    filter(polozka_nazev %in% c("Odbytné", "Odchodné"))|>
    count(kapitola_nazev, orgs_uo_nazev, vykaz_year, podseskupeni, polozka_nazev, wt = budget_spending) |>
    ggplot(aes(vykaz_year, n / 1e6, fill = polozka_nazev, group = polozka_nazev)) +
    geom_col() +
    facet_wrap(~kapitola_nazev)

3e9/1e6
3000*12*50000

dta <- read_parquet("https://github.com/idea-cergeei/studie-urednici/raw/main/data-export/data_all.parquet")

names(dta)

dta |> count(kategorie)

dta_uo <- dta |>
    filter(faze_rozpoctu == "SKUT", rok == 2021, kategorie == "UO") |>
    select(kap_nazev, kap_zkr, platy) |>
    mutate(kap_nazev = str_squish(kap_nazev)) |> 
    mutate(platy = platy / 1e6)

dta_uo |>
    left_join(rozpp_uo, by = c(kap_nazev = "kapitola_nazev")) |>
    mutate(podil = platy/n) |> 
    View()
