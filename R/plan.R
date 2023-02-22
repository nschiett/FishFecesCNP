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
    
    # get extra traits per species
    diets = get_diets(),
    ash = get_ash(diets),
    intestine = get_intestine(),
    mass = get_mass(data_ae),
    
    # cnp models
    stanmodel_cnp = rstan::stan_model("stan/cnp_student_simple.stan"),
    result_cnp = run_cnp_models(data = data_ae, stanmodel = stanmodel_cnp, ash),

    # add traits to ae results
    result_ext = add_traits(result_cnp, mass, intestine, diets),
    
    # more models
    models_ae_diet = fit_diet_models(result_ext),
    models_copro = fit_copr_models(result_ext),
    
    # predict ae per group
    #group_ae = predict_ae(result_ext),
    
    spflux = sp_fluxes(data_ae, result_ext),
    
    # community
    #comflux = community_fluxes(result_ext),
    
    #plots
    fig1 = make_fig1(result_ext),
    # fig2 = make_fig2(model_ae_diet),
    figs1 = make_figs1(result_ext),
    # figs2 = make_figs2(result_ext),
    
    # output
    out1 = save_plot(fig1, "fig1", height = 12),
    # out2 = save_plot(fig2, "fig2", height = 8),
    outs1 = save_plot(figs1, "figs1", height = 10, width = 10)
    
    ### paper ###
    # main_text_doc = rmarkdown::render(knitr_in("text/main.Rmd"), 
    #                                   output_format = "word_document", 
    #                                   output_dir = "./output/text/",
    #                                   output_file = "main.docx")
    
  )
}




