# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/functions.R") # Define your custom code as a bunch of functions.
# devtools::install(upgrade = FALSE)
# drake::expose_imports("bookdown",
#                       character_only = TRUE)

# drake::expose_imports("berlin.trees",
                      # character_only = TRUE)
# redownload <- FALSE

source("R/plan.R")      # Create your drake plan.
# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
# drake::expose_imports("berlin.trees",
                      # character_only = TRUE)
# drake_config(plan, envir = getNamespace("berlin.trees"))


# future::plan(future.callr::callr)

drake_config(plan)
