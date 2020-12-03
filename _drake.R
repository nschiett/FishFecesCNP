# Load all functions 
devtools::load_all()

# get plan
plan <- make_plan()
config <- drake::drake_config(plan = plan, lock_envir = FALSE)
