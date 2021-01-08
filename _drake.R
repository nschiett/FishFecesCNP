# Load all functions 
devtools::load_all()
files <- paste0("R/", list.files("R"))
lapply(files, source)
  
# get plan
plan <- make_plan()
config <- drake::drake_config(plan = plan, lock_envir = FALSE)
