

plan <- drake_plan(
  ##### set up output #####
  out_folder = dir.create("output"),
  out_folder_data = dir.create("output/data"),
  out_folder_plots = dir.create("output/plots"),
  out_folder_text = dir.create("output/text"),
  
  data_raw = read_csv("data/results_aecnp.csv"),
  data_ae = remove_outliers(data_raw),
  
  stanmodel_ae = stan_model("stan/ae_normal.stan"),
  result_ae = run_ae_models(data = data_ae, stanmodel = stanmodel_ae)
)



