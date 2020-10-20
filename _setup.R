
usethis::use_package("rgdal")
# usethis::use_package("ckanr")
# usethis::use_package("ggplot2")
# usethis::use_package("dplyr")
# usethis::use_package("xml2")
# usethis::use_package("httr")
# usethis::use_package("purrr")
# usethis::use_package("stringr")
# usethis::use_package("sf")
# usethis::use_package("future.callr")
# usethis::use_package("drake")
# usethis::use_package("furrr")
# usethis::use_package("tools")
# usethis::use_package("forcats")
# usethis::use_package("tibble")
# usethis::use_package("rmarkdown")
# usethis::use_package("raster")
# usethis::use_package("magrittr")
# usethis::use_package("lme4")
# usethis::use_package("rlang")
# usethis::use_package("stars")
# usethis::use_package("extrafont")
# usethis::use_package("scales")
# usethis::use_package("raster")
# usethis::use_package("data.table")

## packages

library(drake)
# library(future.callr)
# future::plan(future.callr::callr)

# clean()
#
# devtools::install(upgrade = TRUE)
devtools::install()


drake::r_outdated(source = './_drake.R')
# redownload <- FALSE

drake::r_make()


source("R/plan.R")      # Create your drake plan.
drake::vis_drake_graph(plan, targets_only = TRUE)
drake::sankey_drake_graph(plan, targets_only = TRUE)
