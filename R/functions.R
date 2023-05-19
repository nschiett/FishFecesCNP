standard <- function(x){
  (x - mean(x))/sd(x)
}

inv_logit <- function(x){
  exp(x)/(1+exp(x))
}
#' Prune the raw data
#'
#' @param data raw dataframe with cnp contents of gut content and feces
#'
#' @return 
#' @export
#'
#' @examples
prune <- function(data){
    data <- 
      data %>%
      dplyr::group_by(species) %>%
      dplyr::mutate(outlier = 
               c > exp(mean(log(c), na.rm = TRUE) + (3 * sd(log(c), na.rm = TRUE))) |
               n > exp(mean(log(n), na.rm = TRUE) + (3 * sd(log(n), na.rm = TRUE))) |
               p > exp(mean(log(p), na.rm = TRUE) + (3 * sd(log(p), na.rm = TRUE)))) %>%
      dplyr::filter(outlier == FALSE | is.na(outlier)) %>%
      dplyr::select(-outlier) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(species, location) %>%
      dplyr::mutate(n_c1 = sum(c > 0 & key == "AE1", na.rm = TRUE),
             n_c2 = sum(c > 0 & key == "AE2", na.rm = TRUE),
             n_n1 = sum(n > 0 & key == "AE1", na.rm = TRUE),
             n_n2 = sum(n > 0 & key == "AE2", na.rm = TRUE),
             n_p1 = sum(p > 0 & key == "AE1", na.rm = TRUE),
             n_p2 = sum(p > 0 & key == "AE2", na.rm = TRUE)) %>%
      dplyr::filter(n_c1 > 3, n_c2 > 3, n_n1 > 3, n_n2 > 3, n_p1 > 3, n_p2 > 3) %>%
      dplyr::mutate(species = gsub(" ", "_", species)) %>%
      dplyr::mutate(sploc = paste(species, location, sep = "__")) %>%
      dplyr::ungroup() %>%
      dplyr::select(-family)
    data
}


#' Run model to get assimilation efficiency
#'
#' @param model The compiled stan model
#' @param data The data containing cnp%
#' @param element The element (C, N or P)
#'
#' @return 
#' @export
#'
#' @examples
ae_mod <- function(model, data, element){
  
  x1 <- 
    dplyr::filter(data, key == "AE1") %>%
    dplyr::filter(!is.na(.data[[element]])) %>%
    dplyr::select(.data[[element]]) %>%
    purrr::simplify()
  x2 <- 
    dplyr::filter(data, key == "AE2") %>%
    dplyr::filter(!is.na(.data[[element]])) %>%
    dplyr::select(.data[[element]]) %>%
    purrr::simplify()
  
  data <- list(
    N1 = length(x1),
    N2 = length(x2),
    x1 = x1, 
    x2 = x2
  )
  
  fit <- rstan::sampling(model, data, warmup = 2000, iter = 4000)
  
  sum <- rstan::summary(fit)$summary[c("mu1", "mu2", "a"), -c(2,9,10)]
  
  result <- data.frame(
    mu1_m = sum["mu1", "mean"],
    mu1_sd = sum["mu1", "sd"],
    mu1_lb = sum["mu1", "2.5%"],
    mu1_ub = sum["mu1", "97.5%"],
    mu1_25 = sum["mu1", "25%"],
    mu1_75 = sum["mu1", "75%"],
    mu2_m = sum["mu2", "mean"],
    mu2_sd = sum["mu2", "sd"],
    mu2_lb = sum["mu2", "2.5%"],
    mu2_ub = sum["mu2", "97.5%"],
    mu2_25 = sum["mu2", "25%"],
    mu2_75 = sum["mu2", "75%"],
    a_m = sum["a", "mean"],
    a_sd = sum["a", "sd"],
    a_lb = sum["a", "2.5%"],
    a_ub = sum["a", "97.5%"],
    a_25 = sum["a", "25%"],
    a_75 = sum["a", "75%"]
  )
  
  colnames(result) <- paste(element, colnames(result), sep = "_")
  
  result
  
}

#' Run model to get averages cnp diet and feces
#'
#' @param model The compiled stan model
#' @param data The data containing cnp%
#' @param element The element (C, N or P)
#'
#' @return 
#' @export
#'
#' @examples
cnp_mod <- function(model, data){
  
  x1 <- 
    dplyr::filter(data, key == "AE1") %>%
    dplyr::select(c, n, p) %>%
    dplyr::filter(c>0, n>0, p>0) %>%
    as.matrix()
  x2 <- 
    dplyr::filter(data, key == "AE2") %>%
    dplyr::select(c, n, p) %>%
    dplyr::filter(c>0, n>0, p>0) %>%
    as.matrix()
  
  ar <- unique(data$ash_ratio)
  
  data <- list(
    N1 = nrow(x1),
    N2 = nrow(x2),
    c1 = x1[,1], 
    c2 = x2[,1],
    n1 = x1[,2], 
    n2 = x2[,2],
    p1 = x1[,3], 
    p2 = x2[,3],
    ar = ar
  )
  
  fit <- rstan::sampling(model, data, warmup = 2000, iter = 4000)
  
  rstan::summary(fit)$summary[c("Dc", "Wc", 
                                       "Dn", "Wn",
                                       "Dp", "Wp",
                                       "Dcn", "Dcp", "Dnp",
                                       "Wcn", "Wcp", "Wnp",
                                       "Acn", "Acp", 
                                       "Rc", "Rn", "Rp", 
                                "ac", "an", "ap"), -c(2,9,10)] %>%
    as.data.frame() %>%
    tibble::rownames_to_column("var") %>%
    dplyr::rename(lqn = `2.5%`,
           uqn = `97.5%`,
           lqr = `25%`,
           uqr = `75%`,
           median = `50%`) %>%
    tidyr::pivot_longer(cols = 2:8) %>%
    dplyr::mutate(name = paste(var, name, sep = "_")) %>%
    dplyr::select(-var) %>%
    tidyr::pivot_wider(names_from = name, values_from = value)
  
  
  
}

#' Run ae models for all species
#'
#' @param data 
#' @param stanmodel 
#'
#' @return
#' @export
#'
#' @examples
run_ae_models <- function(data, stanmodel){
  
  sploc <- dplyr::select(data, species, location) %>%
    unique() %>%
    dplyr::filter(location == "Moorea")
  
  c <- lapply(1:nrow(sploc), function(i){
    sp <- purrr::simplify(sploc[i, "species"])
    loc <- purrr::simplify(sploc[i, "location"])
    
    d <- dplyr::filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "c")
    
  }) %>% plyr::ldply()
  
  n <- lapply(1:nrow(sploc), function(i){
    sp <- purrr::simplify(sploc[i, "species"])
    loc <- purrr::simplify(sploc[i, "location"])
    
    d <- dplyr::filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "n")
    
  }) %>% plyr::ldply()
  
  p <- lapply(1:nrow(sploc), function(i){
    sp <- purrr::simplify(sploc[i, "species"])
    loc <- purrr::simplify(sploc[i, "location"])
    
    d <- dplyr::filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "p")
    
  }) %>% plyr::ldply()
  
  result <- cbind(sploc, c, n, p)
  
  result
  
}


#' Run cnp models for all species
#'
#' @param data 
#' @param stanmodel 
#'
#' @return
#' @export
#'
#' @examples
run_cnp_models <- function(data, stanmodel, ash){
  
  sploc <- dplyr::select(data, species, location) %>%
    dplyr::filter(location == "Moorea") %>%
    #remove outlier species manually
    dplyr::filter(!species == "Chaetodon_quadrimaculatus") %>%
    unique()
  
  res <- lapply(1:nrow(sploc), function(i){
    sp <- purrr::simplify(sploc[i, "species"])
    loc <- purrr::simplify(sploc[i, "location"])
    
    d <- dplyr::filter(data, species == sp, location == loc) %>%
      dplyr::left_join(ash)
    
    print(sp)
    print(loc)
    
    res <- cnp_mod(stanmodel, data = d)

    
  }) %>% plyr::ldply()
  
  cbind(sploc, res)
  
  
}

#' Import ash content ratios 
#'
#' @return
#' @export
#'
#' @examples
get_ash <- function(diets){
  read_csv("output/data/ash_species_estimate.csv")
}

get_intestine <- function(){
  read_csv("data/intestine_dataset.csv") %>%
    filter(location == "Moorea") %>%
    mutate(int_rel = intestine_surface/weight) %>%
    group_by(species) %>%
    summarize(int_rel = median(int_rel, na.rm = T)) %>%
    mutate(species = gsub(" ", "_", species))
}

get_mass <- function(data_ae){
  
  data_ae %>%
    group_by(species) %>%
    summarise(biomass = mean(weight, na.rm = T))
}

#' load trophic guilds
#'
#' @return
#' @export
#'
#' @examples
get_diets <- function(){
  readr::read_csv("data/extrapolation_trophic_guilds.csv") %>%
    dplyr::select(family, species, diet = trophic_guild_predicted) %>%
    rbind(data.frame(
      species = "Aulostomus_chinensis", family = "Aulostomidae",
      diet = 4
    )) %>%
    dplyr::mutate(diet2 = dplyr::case_when(
      diet %in% c(1, 5, 6) ~ "3_imix",
      diet == 3 ~ "4_cor",
      diet == 2 ~ "2_herb",
      diet %in% c(7, 4) ~ "6_carn",
      diet == 8 ~ "5_plank"
    )) %>%
    dplyr::mutate(diet2 = dplyr::case_when(
      species %in% c("Acanthurus_olivaceus", 
                     "Ctenochaetus_striatus", 
                     "Chlorurus_spilurus",
                     "Acanthurus_pyroferus") ~ "1_detr",
      species == "Cheilinus_chlorurus" ~ "3_imix",
      TRUE ~ diet2
    ))
}

#' Adding diet category and stable isotopes
#'
#' @param result_ae 
#' @param sia_species sia means on species level
#'
#' @return
#' @export
#'
add_traits <- function(result, mass, intestine, diets){
  
  # keep only Moorea
  result <- result %>%
     dplyr::filter(location == "Moorea") %>%
     #remove outlier species manually
     dplyr::filter(!species == "Chaetodon_quadrimaculatus")
  
  # combine
  result %>% 
    dplyr::left_join(diets) %>%
    dplyr::left_join(mass) %>%
    dplyr::left_join(intestine)
  }

#' Title
#'
#' @param result_ext 
#'
#' @return
#' @export
#'
fit_diet_models <- function(data){
  
  datac <- data %>%
    filter(ac_mean>0) 
  datan <- data %>%
    filter(an_mean>0) 
  datap <- data %>%
    filter(ap_mean>0) 
  
  fitc_se <- brm(ac_mean | resp_se(`ac_sd`, sigma = FALSE) ~ me(Dc_mean, Dc_sd)  + log(int_rel),
                 backend = "cmdstanr", family = "student", data = datac, iter = 4000, cores = 4, threads = 2)
  
  fitn_se <- brm(an_mean | resp_se(`an_sd`, sigma = FALSE) ~ me(Dn_mean, Dn_sd) +  log(int_rel),
                 backend = "cmdstanr", family = "student", data = datan, iter = 4000, cores = 4, threads = 2)
  
  fitp_se <- brm(ap_mean | resp_se(`ap_sd`, sigma = FALSE) ~ me(Dp_mean, Dp_sd) + log(int_rel) ,
                 backend = "cmdstanr", family = "student", data = datap, iter = 4000, cores = 4, threads = 2)
  
  summary(fitc_se)
  summary(fitn_se)
  summary(fitp_se)
  
  conditional_effects(fitc_se)
  conditional_effects(fitn_se)
  conditional_effects(fitp_se)
  
  return(list(fitc_se, fitn_se, fitp_se))
  
}


#' Title
#'
#' @param result_ext 
#'
#' @return
#' @export
#'
#' @import brms
fit_copr_models <- function(result_ext){
  
  
  cop <- readr::read_csv("data/coprophagy_robertson1982.csv") %>%
    dplyr::mutate(species = gsub(" ", "_", species)) %>%
    dplyr::inner_join(result_ext)
  
  # coprophage models
  
  fit_copr_p <- brms::brm(coprophage ~  Dp_median,
                   data = cop, family = "bernoulli")
  fit_copr_n <- brms::brm(coprophage ~  Dn_median,
                          data = cop, family = "bernoulli")
  
  cop2 <- tidyr::drop_na(cop, Dp_median) %>%
    dplyr::mutate(one = round(prop_eaten * n_feces)) %>%
    dplyr::mutate(zero = n_feces - one) %>%
    tidyr::pivot_longer(names_to = "key", values_to = "n", c(one, zero)) %>%
    tidyr::uncount(n) %>%
    dplyr::mutate(key2 = dplyr::case_when(key == "one" ~ 1,
                                          key == "zero" ~ 0))
  
  fit_cop_p <- brms::brm(key2 ~ Wp_median, 
                 data = cop2, family = "bernoulli", iter = 2000)
  fit_cop_n <- brms::brm(key2 ~ Wn_median, 
                       data = cop2, family = "bernoulli", iter = 2000)
  
  return(list(fit_copr_n, fit_cop_n, fit_copr_p, fit_cop_p))
  
}

add_pred <- function(result_ext, models_ae_diet){
  
  predc <- fitted(models_ae_diet[[1]], newdata = result_ext) 
  predn <- fitted(models_ae_diet[[2]], newdata = result_ext) 
  predp <- fitted(models_ae_diet[[3]], newdata = result_ext) 
  
  result_ext %>%
    mutate(ac_mean_pred = predc[,1],
           ac_sd_pred = predc[,2],
           an_mean_pred = predn[,1],
           an_sd_pred = predn[,2],
           ap_mean_pred = predp[,1],
           ap_sd_pred = predp[,2])
}

#' get sp level fluxes
#'
#' @param result_ext 
#' @param group_ae 
#'
#' @return
#' @export
#'
#' @examples
sp_fluxes <- function(data_ae, result_ext_pred) {
  
  # median size per species 
  spsize <- data_ae %>%
    dplyr::filter(location == "Moorea", species %in% result_ext_pred$species) %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(size_median = round(median(tl)),
                     size_max = round(max(tl)))
  
  # add ae values 
  ae_sp <- result_ext_pred %>%
    dplyr::mutate(ac_mean = dplyr::case_when(ac_mean < 0.1 ~ ac_mean_pred, TRUE ~ ac_mean), 
                  an_mean = dplyr::case_when(an_mean < 0.1 ~ an_mean_pred, TRUE ~ an_mean), 
                  ap_mean = dplyr::case_when(ap_mean < 0.1 ~ ap_mean_pred, TRUE ~ ap_mean),
                  ap_mean = dplyr::case_when(ap_mean < 0.1 ~ ap_mean_pred, TRUE ~ ap_mean),
                  ac_sd = dplyr::case_when(ap_mean < 0.1 ~ ac_sd_pred, TRUE ~ ac_sd), 
                  an_sd = dplyr::case_when(ap_mean < 0.1 ~ an_sd_pred, TRUE ~ an_sd), 
                  ap_sd = dplyr::case_when(ap_mean < 0.1 ~ ap_sd_pred, TRUE ~ ap_sd)) %>%
    dplyr::select(species, diet2, ac_m = ac_mean, ac_sd, 
                  an_m = an_mean, an_sd, 
                  ap_m = ap_mean, ap_sd,
                  Dc_m = Dc_mean, Dc_sd,
                  Dn_m = Dn_mean, Dn_sd,
                  Dp_m = Dp_mean, Dp_sd)
  
  # load parameters
  params <- readr::read_csv("data/params_sst_glob.csv") %>%
    dplyr::filter(v_m %in% c(26, 27, 28, 29)) %>%
    dplyr::group_by(Family, species, Species) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::select(-ac_m, -an_m, -ap_m, -Dc_m, -Dc_sd, 
                  -Dn_m, -Dn_sd, - Dp_m, -Dp_sd)

  spsize <- dplyr::inner_join(spsize, ae_sp) %>%
    dplyr::left_join(params) %>%
    tidyr::drop_na(Qc_m)
  
  cnp_out <- function(x){
    
    d <- data[x,]
    size <- c(5:purrr::simplify(d$size_max))
    p <- d %>%
      dplyr::select(k_m, Qc_m ,Qn_m, Qp_m, Dc_m, Dn_m, Dp_m, Dc_sd, Dn_sd, Dp_sd, 
                    alpha_m, f0_m,          
             theta_m, lwa_m,  lwb_m, r_m, h_m,  v_m, linf_m,        
             F0nz_m, F0pz_m, ac_m, an_m, ap_m, ac_sd, an_sd, ap_sd) %>% 
      purrr::simplify() %>% as.list()
    
    fit <- fishflux::cnp_model_mcmc(TL = size, param = p)
    
    out <- fishflux::extract(fit, c("Ic", "In", "Ip", "Sc","Sn", "Sp", 
                                    "Fn", "Fp", "Wc", "Wn", "Wp", 
                                    "Gc", "Gn", "Gp", "w1")) %>%
      dplyr::mutate(species = purrr::simplify(d$species),
                    size_median = purrr::simplify(d$size_median)) %>%
      dplyr::select(
        species,
        size = TL,
        size_median,
        biomass = w1_median,
        Ic = Ic_median,
        In = In_median,
        Ip = Ip_median,
        Sc = Sc_median,
        Sn = Sn_median,
        Sp = Sp_median,
        Fn = Fn_median,
        Fp = Fp_median,
        Wc = Wc_median,
        Wn = Wn_median,
        Wp = Wp_median,
        Gc = Gc_median,
        Gn = Gn_median,
        Gp = Gp_median
      )
    
    limit <- fishflux::limitation(fit)
    lim_ <- which.max(limit$prop_lim)
    
    if(lim_ == 1){
      lim_ <- "c"
    } else if (lim_ == 2){
      lim_ <- "n"
    } else {
      lim_ <- "p"
    }
    
    out %>%
      dplyr::mutate(lim = lim_, 
                    plimc = limit[1,3],
                    plimn = limit[2,3],
                    plimp = limit[3,3])
  }

  
  data <- spsize
  
  # in pieces to avoid crash
  results1 <- lapply(1:nrow(data), cnp_out) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(GGEc = Gc/Ic,
                  GGEn = Gn/In,
                  GGEp = Gp/Ip)

  results2 <- results1 %>% 
    dplyr::filter(round(size) == round(size_median)) %>%
    dplyr::left_join(select(result_ext_pred, -biomass))

  
  return(list(results1, results2))
}




#' get community level fluxes per broad diet group
#'
#' @param result_ext 
#' @param group_ae 
#'
#' @return
#' @export
#'
#' @examples
community_fluxes <- function(result_ext, diets, models_copro) {
  
  # load parameters
  params <- readr::read_csv("data/params_sst_glob.csv") %>%
    dplyr::filter(v_m %in% c(26, 27, 28, 29)) %>%
    dplyr::group_by(Family, species, Species) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::select(-ac_m, -an_m, -ap_m, -Dc_m, -Dc_sd, 
                  -Dn_m, -Dn_sd, - Dp_m, -Dp_sd)
  
  # uvc data
  moorea <- readr::read_csv("data/moorea_uvc.csv") %>%
    # correct a name 
    dplyr::mutate(Taxon = 
                    dplyr::case_when(Taxon == "Chlorurus sordidus" ~ 
                                       "Chlorurus spilurus",
                                     TRUE ~ Taxon)) %>%
    dplyr::filter(Island == "Moorea") %>%
    dplyr::filter(Size > 0) %>%
    dplyr::select(SurveyID, RepID, TransID, Month, Day, Year, Site_name, Lat, Long, Depth, TransectLength, TransectWidth, ReefZone,
                  Taxon, Abundance, Size) %>%
    dplyr::group_by(SurveyID, RepID, TransID, Month, Day, Year, Site_name, Lat, Long, Depth, TransectLength, TransectWidth, ReefZone,
                    Taxon, Size) %>%
    dplyr::summarize(Abundance = sum(Abundance)) %>%
    janitor::clean_names() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(species = gsub(" ", "_", taxon)) %>%
    dplyr::left_join(params) %>%
    dplyr::filter(!is.na(lwa_m)) %>%
    dplyr::mutate(biomass = lwa_m * size ^ lwb_m) %>%
    dplyr::mutate(biomass = (abundance * biomass)) %>%
    dplyr::filter(biomass > 0) %>%
    
    dplyr::mutate(check_species = species %in% result_ext$species)
  
  sum(moorea$biomass * moorea$check_species)/sum(moorea$biomass)
  #69.9% of biomass species
  
  # add ae values 
  ae_sp <- result_ext %>%
    dplyr::mutate(ac_mean = dplyr::case_when(ac_mean < 0.1 ~ ac_mean_pred, TRUE ~ ac_mean), 
                  an_mean = dplyr::case_when(an_mean < 0.1 ~ an_mean_pred, TRUE ~ an_mean), 
                  ap_mean = dplyr::case_when(ap_mean < 0.1 ~ ap_mean_pred, TRUE ~ ap_mean),
                  ap_mean = dplyr::case_when(ap_mean < 0.1 ~ ap_mean_pred, TRUE ~ ap_mean),
                  ac_sd = dplyr::case_when(ap_mean < 0.1 ~ ac_sd_pred, TRUE ~ ac_sd), 
                  an_sd = dplyr::case_when(ap_mean < 0.1 ~ an_sd_pred, TRUE ~ an_sd), 
                  ap_sd = dplyr::case_when(ap_mean < 0.1 ~ ap_sd_pred, TRUE ~ ap_sd)) %>%
    dplyr::select(species, diet2, ac_m = ac_mean, ac_sd, 
                  an_m = an_mean, an_sd, 
                  ap_m = ap_mean, ap_sd,
                  Dc_m = Dc_mean, Dc_sd,
                  Dn_m = Dn_mean, Dn_sd,
                  Dp_m = Dp_mean, Dp_sd)
  
  ae_diet <- ae_sp %>%
    group_by(diet2) %>%
    summarize(an_diet = median(an_m),
              ap_diet = median(ap_m),
              ac_diet = median(ap_m),
              Dc_diet = median(Dc_m),
              Dn_diet = median(Dn_m),
              Dp_diet = median(Dp_m))
  
  
  moorea <- moorea %>%
    dplyr::inner_join(diets) %>%
    dplyr::left_join(ae_sp) %>%
    dplyr::left_join(ae_diet) %>%
    dplyr::mutate(ac_m = dplyr::coalesce(ac_m, ac_diet),
                  an_m = dplyr::coalesce(an_m, an_diet),
                  ap_m = dplyr::coalesce(ap_m, ap_diet),
                  Dc_m = dplyr::coalesce(Dc_m, Dc_diet),
                  Dn_m = dplyr::coalesce(Dn_m, Dn_diet),
                  Dp_m = dplyr::coalesce(Dp_m, Dp_diet))
  
  spsize <- moorea %>% 
    dplyr::select(c(colnames(params), size, diet2,
                    ac_m, an_m, ap_m, ac_sd, an_sd, ap_sd,
                    Dc_m, Dn_m, Dp_m, Dc_sd, Dn_sd, Dp_sd)) %>%
    unique()
  
  cnp_out <- function(x){
    
    d <- data[x,]
    size <- purrr::simplify(d$size)
    p <- d %>%
      dplyr::select(k_m, Qc_m ,Qn_m, Qp_m, Dc_m, Dn_m, Dp_m, alpha_m, f0_m,          
                    theta_m, lwa_m,  lwb_m, r_m, h_m,  v_m, linf_m,        
                    F0nz_m, F0pz_m, ac_m, an_m, ap_m) %>% 
      purrr::simplify() %>% as.list()
    
    fit <- fishflux::cnp_model_mcmc(TL = size, param = p)
    
    out <- fishflux::extract(fit, c("Ic", "In", "Ip", "Sc","Sn", "Sp", 
                                    "Fn", "Fp", "Wc", "Wn", "Wp")) %>%
      dplyr::select(
        Ic = Ic_median,
        In = In_median,
        Ip = Ip_median,
        Sc = Sc_median,
        Sn = Sn_median,
        Sp = Sp_median,
        Fn = Fn_median,
        Fp = Fp_median,
        Wc = Wc_median,
        Wn = Wn_median,
        Wp = Wp_median
      )
    
    limit <- fishflux::limitation(fit)
    lim_ <- which.max(limit$prop_lim)
    
    if(lim_ == 1){
      lim_ <- "c"
    } else if (lim_ == 2){
      lim_ <- "n"
    } else {
      lim_ <- "p"
    }
    
    out %>%
      dplyr::mutate(lim = lim_, 
                    plimc = limit[1,3],
                    plimn = limit[2,3],
                    plimp = limit[3,3])
  }
  
  
  data <- spsize
  
  # in pieces to avoid crash
  results1 <- parallel::mclapply(1:nrow(data), cnp_out, mc.cores = 4) %>%
    dplyr::bind_rows() 
  
  # coprophagy
  summary(models_copro[[4]])
  prob_cop <- cbind(select(result_ext, species, diet2), 
                    fitted(models_copro[[4]], newdata = result_ext)) %>%
    select(species, diet2, prob_cop = Estimate)
  
  prob_cop <- left_join(select(data, species, diet2), prob_cop) %>%
    dplyr::group_by(diet2) %>%
    mutate(prob_cop_diet = median(prob_cop, na.rm = TRUE)) %>%
    mutate(prob_cop = dplyr::coalesce(prob_cop, prob_cop_diet)) %>%
    select(species, prob_cop)
  
  result <- cbind(dplyr::select(data, species, diet2, size), results1) 
  
  moo <- moorea %>%
    dplyr::select(
      survey_id, rep_id, trans_id, month, day, year, site_name, lat, long, diet2,
      depth, transect_length, transect_width, reef_zone, species, size, abundance, biomass
    ) %>%
    dplyr::left_join(result)
  
  summ <- moo %>%
    dplyr::mutate(Fn = (Fn * abundance), Fp = (Fp * abundance), 
                  Wc = (Wc * abundance), Wn = (Wn * abundance), Wp = (Wp * abundance),
                  Ic = (Ic * abundance), In = (In * abundance), Ip = (Ip * abundance),
                  Sc = (Sc * abundance), Sn = (Sn * abundance), Sp = (Sp * abundance))%>%
    dplyr::group_by(survey_id, rep_id, trans_id, month, day, year, site_name, lat, long,
                    depth, transect_length, transect_width, reef_zone, species, diet2) %>%
    dplyr::summarize(Fn = sum(Fn ), Fp = sum(Fp ), 
                     Wc = sum(Wc ), Wn = sum(Wn ), Wp = sum(Wp ),
                     Ic = sum(Ic ), In = sum(In ), Ip = sum(Ip ),
                     Sc = sum(Sc ), Sn = sum(Sn ), Sp = sum(Sp ),
                     biomass = sum(biomass) ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Fn = Fn/(transect_length*transect_width),
      Fp = Fp/(transect_length*transect_width),
      Wc = Wc/(transect_length*transect_width),
      Wn = Wn/(transect_length*transect_width),
      Wp = Wp/(transect_length*transect_width),
      Ic = Ic/(transect_length*transect_width),
      In = In/(transect_length*transect_width),
      Ip = Ip/(transect_length*transect_width),
      Sc = Sc/(transect_length*transect_width),
      Sn = Sn/(transect_length*transect_width),
      Sp = Sp/(transect_length*transect_width),
      biomass = biomass/(transect_length*transect_width)) %>%
    dplyr::group_by(year, site_name, reef_zone, diet2, species) %>%
    dplyr::summarize(Fn = median(Fn), Fp = median(Fp), 
                     Wc = median(Wc), Wn = median(Wn), Wp = median(Wp),
                     Ic = median(Ic), In = median(In), Ip = median(Ip),
                     Sc = median(Sc), Sn = median(Sn), Sp = median(Sp),
                     biomass = median(biomass)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(prob_cop) %>%
    dplyr::mutate(prob_cop = case_when(
      prob_cop < 0.5 ~ 0, TRUE ~ prob_cop)) %>%
    dplyr::mutate(pflux_cop = Wp * prob_cop/2) %>% # only consider half a day
    dplyr::group_by(year, site_name, reef_zone, diet2) %>%
    dplyr::summarize(Fn = sum(Fn), Fp = sum(Fp), 
                     Wc = sum(Wc), Wn = sum(Wn), Wp = sum(Wp),
                     Ic = sum(Ic), In = sum(In), Ip = sum(Ip),
                     Sc = sum(Sc), Sn = sum(Sn), Sp = sum(Sp),
                     biomass = sum(biomass),
                     pflux_cop = sum(pflux_cop)) %>%
    dplyr::group_by(reef_zone, site_name, year, diet2) %>%
    dplyr::summarize(Fn = median(Fn), Fp = median(Fp), 
                     Wc = median(Wc), Wn = median(Wn), Wp = median(Wp),
                     Ic = median(Ic), In = median(In), Ip = median(Ip),
                     Sc = median(Sc), Sn = median(Sn), Sp = median(Sp),
                     biomass = median(biomass),
                     pflux_cop = median(pflux_cop)) %>%
    ungroup()
  
  write_csv(summ, "output/data/comfluxes.csv")
  
  
  pflux <- summ %>%
    select(reef_zone, site_name, year, diet2, Ip, Fp, Sp, Wp, Wn, Fn, pflux_cop) %>%
    group_by(reef_zone, site_name, year) %>%
    mutate(Ip_tot = sum(Ip)) %>%
    mutate(Ip_r = Ip/Ip_tot, Wp_r = Wp/Ip_tot,
           Fp_r = Fp/Ip_tot, pcop_r = pflux_cop/Ip_tot) %>%
    mutate(Wp_r = Wp_r - pcop_r) %>%
    mutate(P_ratio = sum(Wp)/sum(Fp))%>%
    mutate(N_ratio = sum(Wn)/sum(Fn)) %>%
    group_by(reef_zone, diet2) %>%
    summarize_all(mean, na.rm = TRUE) %>% 
    filter(reef_zone == "forereef")
  
  sum(pflux$Wp_r + pflux$pcop_r)
  #0.6528274
  sum(pflux$Fp_r)
  #  0.1349268
  test <- data.frame(
    prop = c(sum(pflux$Wp_r + pflux$pcop_r), sum(pflux$Fp_r)))
  
  ggplot(test, aes(x="", y = c(0.6528274, 0.1349268), fill = c("Wp", "Fp"))) +
    geom_bar(stat="identity", width=1) +
    scale_y_continuous(limits = c(0,1)) +
    coord_polar("y", start=0) +
    geom_text(aes(label = paste0(c(82.4, 8.7), "%")), position = position_stack(vjust=0.5)) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_void()
  ggsave("output/plots/fig5_pie.pdf")
  
  write_csv(pflux, "output/data/pcomflux.csv")
  
  return(pflux)
}



