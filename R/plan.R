#' Writes the drake plan of this project
#'
#' @return
#' @export
#'
#' @importFrom readr read_csv
make_plan <- function(){
  drake::drake_plan(
    ##### set up output #####
    out_folder = dir.create("output"),
    out_folder_data = dir.create("output/data"),
    out_folder_plots = dir.create("output/plots"),
    out_folder_text = dir.create("output/text"),
    
    # ae data
    data_raw = read_csv("data/results_aecnp.csv"),
    data_ae = prune(data_raw),
    
    # additional info
    sia_species = get_sia_means(),
    intestine = get_intestine_residuals(),
    
    # ae models
    stanmodel_ae = rstan::stan_model("stan/ae_student.stan"),
    result_ae = run_ae_models(data = data_ae, stanmodel = stanmodel_ae),
    
    # add traits to ae results
    result_ext = add_traits(result_ae, sia_species, intestine),
    
    # more models
    model_ae_diet = fit_diet_models(result_ext),
    models_copro = fit_copr_models(result_ext),
    
    # predict ae per group
    group_ae = predict_ae(result_ext),
    
    # community
    comflux = community_fluxes(result_ext, group_ae),
    
    #plots
    fig1 = make_fig1(result_ext),
    fig2 = make_fig2(model_ae_diet),
    figs1 = make_figs1(result_ext),
    figs2 = make_figs2(result_ext),
    
    # output
    out1 = save_plot(fig1, "fig1", height = 10),
    out2 = save_plot(fig2, "fig2", height = 8),
    outs1 = save_plot(figs1, "figs1", height = 10),
    outs2 = save_plot(figs2, "figs2", height = 8)
    
  )
}




