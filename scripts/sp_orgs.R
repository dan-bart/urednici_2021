library(statnipokladna)
library(arrow)
orgs0 <- sp_get_codelist("ucjed")

orgs <- orgs0 |>
  sp_add_codelist("druhuj") |>
  sp_add_codelist("poddruhuj")

arrow::write_parquet(orgs, "data-interim/sp_orgs.parquet")
