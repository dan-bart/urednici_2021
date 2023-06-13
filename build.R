source("standardize_input_data.R")
rmarkdown::render("results.Rmd")
source("graphs.R")
source("graphs_mod.R")
source("export_data.R")
rmarkdown::render("codebook.Rmd")

system2("http-server", args = c("graphs_mod" , "--o"))
