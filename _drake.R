source("R/packages.R")
source("R/functions.R")
source("R/plan.R")

config <- drake_config(plan = plan, lock_envir = FALSE)
