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
usethis::use_package("tools")

## packages

library(drake)
library(future.callr)
future::plan(future.callr::callr)

# clean()
#
devtools::install(upgrade = FALSE)
drake::r_outdated(source = './_drake.R')
# redownload <- FALSE

drake::r_make()


source("R/plan.R")      # Create your drake plan.
drake::vis_drake_graph(config = drake_config(plan))
