#' Writes the drake plan of this project
#'
#' @return
#' @export
#'
#' @examples
make_plan <- function(){
  drake::drake_plan(
    ##### set up output #####
    out_folder = dir.create("output"),
    out_folder_data = dir.create("output/data"),
    out_folder_plots = dir.create("output/plots"),
    out_folder_text = dir.create("output/text"),
    
    data_raw = read_csv("data/results_aecnp.csv"),
    data_ae = prune(data_raw),
    
    stanmodel_ae = stan_model("stan/ae_student.stan"),
    result_ae = run_ae_models(data = data_ae, stanmodel = stanmodel_ae),
    
    result_ext = add_traits(result_ae),
    
    #plots
    fig1 = make_fig1(result_ext),
    figs1 = make_figs1(result_ext),
    figs2 = make_figs2(result_ext)
  )
}




