source("standardize_input_data.R")

source("R/export_data.R")
rmarkdown::render("codebook.Rmd")

quarto::quarto_render("szu-analyza-stsl.qmd")

rmarkdown::render("results.Rmd")
source("graphs.R")
source("graphs_mod.R")

system2("http-server", args = c("graphs_mod" , "--o"))
