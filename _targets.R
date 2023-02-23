library(targets)
# load functions
source("R/functions.R")
source("R/functions_plots.R")
# set up R packages needed
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "fishualize", "rstan", 
                            "brms", "patchwork", "fishflux", "tidyr", "forcats"))

# create ouput folders
dir.create("output")
dir.create("output/data")
dir.create("output/plots")

# pipeline
list(
  # load and clean data
  tar_target(file_rawdata, "data/results_aecnp.csv", format = "file"),
  tar_target(data_raw, read_csv(file_rawdata)),
  tar_target(data_ae, prune(data_raw)),
  # get extra info per species
  tar_target(diets, get_diets()),
  tar_target(ash, get_ash()),
  tar_target(intestine, get_intestine()),
  tar_target(mass, get_mass(data_ae)),
  # Load and run model to estimate average CNP in diet and feces
  tar_target(stanmodel_cnp, rstan::stan_model("stan/cnp_student_simple.stan")),
  tar_target(result_cnp, run_cnp_models(data = data_ae, stanmodel = stanmodel_cnp, ash)),
  # add traits
  tar_target(result_ext, add_traits(result_cnp, mass, intestine, diets)), 
  # Regression models 
  tar_target(models_copro,  fit_copr_models(result_ext)),
  tar_target(models_ae_diet, fit_diet_models(result_ext)),
  # CNP fluxes per species
  tar_target(spflux, sp_fluxes(data_ae, result_ext_pred)),
  # Add AE prediction for community analysis
  tar_target(result_ext_pred, add_pred(result_ext, models_ae_diet)),
  # community P fluxes 
  tar_target(pcomflux, community_fluxes(result_ext_pred, diets, models_copro)), 
  
  # Main plots
  tar_target(fig1, make_fig1(result_ext)),
  tar_target(fig2, make_fig2(result_ext, models_copro)),
  tar_target(fig3, make_fig3(models_ae_diet, result_ext)),
  tar_target(fig4, make_fig4(spflux)),
  
  # Supplemental figs and plots
  tar_target(figs1, make_figs1(result_ext)),
  tar_target(tables1, make_table1(data_ae, result_ext)),
  tar_target(tables2, make_table2(result_ext)),
  tar_target(tables3, make_table3(result_ext)),
  
  # Save plots
  tar_target(p1, save_plot(fig1, "fig1", height = 12)),
  tar_target(ps1, save_plot(figs1, "figs1", height = 10, width = 10))

)
