standard <- function(x){
  (x - mean(x))/sd(x)
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
      dplyr::ungroup()
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
    unique()
  
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

#' Adding diet category and stable isotopes
#'
#' @param result_ae 
#' @param sia_species sia means on species level
#'
#' @return
#' @export
#'
add_traits <- function(result_ae, sia_species, intestine){
  
  # keep only Moorea
  result_ae <- result_ae %>%
    dplyr::filter(location == "Moorea") %>%
    #remove outlier species manually
    dplyr::filter(!species == "Chaetodon_quadrimaculatus")
  
  # diet category
  diets <- readr::read_csv("data/extrapolation_trophic_guilds.csv") %>%
    dplyr::select(family, species, diet = trophic_guild_predicted) %>%
    rbind(data.frame(
      species = "Aulostomus_chinensis", family = "Aulostomidae",
      diet = 4
    ))
  
  
  # combine
  result <- result_ae %>% 
    dplyr::left_join(diets) %>%
    dplyr::left_join(sia_species) %>%
    dplyr::left_join(intestine)
  
  result

  }


##### sia #####
#' import sia data and model species means
#'
#' @return
#' @export
#'

get_sia_means <- function(){
  sia <- read.csv("data/moorea.sia.2019.csv", sep = ";") %>%
    dplyr::select(-1) %>%
    dplyr::select(species = Names, N = X.N, C = X.C, DN, DC, weight = Weight) %>%
    dplyr::mutate(species = gsub("  ", " ", species)) %>%
    dplyr::mutate(species = gsub(" ", "_", species)) %>%
    tidyr::drop_na(DN) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(n_sia = dplyr::n()) %>%
    dplyr::filter(n_sia > 5) 
  
  fit <- brms::brm(DN ~ 0 + species, data = sia)
  
  newdata <- data.frame(unique(dplyr::select(sia, species, n_sia)))

  pred <- fitted(fit, newdata = newdata)
  
  newdata %>% 
    dplyr::mutate(dn = pred[,1],
           dn_sd = pred[,2])
}

get_intestine_residuals <- function(){
  int <- readr::read_csv("data/intestine_dataset.csv")
  int %>%
    dplyr::filter(location == "Moorea") %>%
    dplyr::group_by(family, species) %>%
    dplyr::summarize(int_surface = mean(intestine_surface),
              sl = mean(sl)) %>% 
    dplyr::mutate(species = gsub(" ", "_", species),
                  int_surface_res = log(int_surface) - (1.68*log(sl))) # Ghilardi et al.
}


#' Title
#'
#' @param result_ext 
#'
#' @return
#' @export
#'
fit_diet_models <- function(result_ext){
  
    long1 <- result_ext %>%
      dplyr::mutate(c = standard(c_mu1_m),
                    n = standard(n_mu1_m),
                    p = standard(p_mu1_m)
      ) %>%
      tidyr::pivot_longer(cols = c(c, n, p), 
                          names_to = "element", values_to = "mu1_st") %>%
      dplyr::select(species, element, mu1_st, dn, family, diet, int_surface_res)
    
    long2 <- result_ext %>%
      dplyr::mutate(c = c_a_m,
             n = n_a_m,
             p = p_a_m
      ) %>%
      tidyr::pivot_longer(cols = c(c, n, p), 
                          names_to = "element", values_to = "ae") %>%
      dplyr::select(species, element, ae)
    
    long <- dplyr::left_join(long1, long2)
    
    eqn <- brms::bf(ae ~ 0 + element + element:mu1_st, family = "student") +
      brms::bf(mu1_st ~  0 + element*dn, 
               family = "student") +
      brms::set_rescor(FALSE)
    
    fit <- brms::brm(eqn, data = long)
    
    newdata <- tidyr::expand_grid(mu1_st = seq(min(long$mu1_st), max(long$mu1_st), 0.2), 
                           element = c("c", "n", "p"),
                           dn = seq(min(long$dn, na.rm = TRUE), max(long$dn, na.rm = TRUE), 0.2))
    
    pred <- fitted(fit, newdata)
    
    pred_data <- newdata %>%
      dplyr::mutate(y_a_m = pred[,1,1],
             y_a_sd = pred[,2,1],
             y_a_lb = pred[,3,1],
             y_a_ub = pred[,4,1],
             y_mu1_m = pred[,1,2],
             y_mu1_sd = pred[,2,2],
             y_mu1_lb = pred[,3,2],
             y_mu1_ub = pred[,4,2])
    
    
    return(list(fit = fit, pred = pred_data))
    
  
}

#' Title
#'
#' @param result_ext 
#'
#' @return
#' @export
#'
#' @import  brms
test_copr_models <- function(result_ext){
  
  cop <- read_csv("data/coprophagy_robertson1982.csv") %>%
    dplyr::mutate(species = gsub(" ", "_", species)) %>%
    dplyr::inner_join(result_ext)
  
  # coprophage models

  fit_copr1 <- brm(coprophage ~ c_mu1_m + n_mu1_m + p_mu1_m,
                  data = cop, family = "bernoulli")
  fit_copr2 <- update(fit_copr1, formula = coprophage ~ c_mu1_m)
  fit_copr3 <- update(fit_copr1, formula = coprophage ~ n_mu1_m)
  fit_copr4 <- update(fit_copr1, formula = coprophage ~ p_mu1_m)
  
  loo_values <- loo(fit_copr1, fit_copr2, fit_copr3, fit_copr4)
  
  ##### being coprophage
  
  cop2 <- drop_na(cop, c_mu1_m) %>%
    mutate(one = round(prop_eaten * n_feces)) %>%
    mutate(zero = n_feces - one) %>%
    pivot_longer(names_to = "key", values_to = "n", c(one, zero)) %>%
    uncount(n)

  
  fit_cop1 <- brm(key ~ p_mu1_m + (1|species), 
                 data = cop2, family = "bernoulli")
  fit_cop2 <- update(fit_cop, formula = key ~ n_mu1_m + (1|species))
  fit_cop3 <- update(fit_cop, formula = key ~ c_mu1_m + (1|species))

  loo(fit_cop1, fit_cop2, fit_cop3)
  
  summary(fit_cop3)
  
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
  
  fit_copr <- brms::brm(coprophage ~  p_mu1_m,
                   data = cop, family = "bernoulli")
  
  cop2 <- tidyr::drop_na(cop, c_mu1_m) %>%
    dplyr::mutate(one = round(prop_eaten * n_feces)) %>%
    dplyr::mutate(zero = n_feces - one) %>%
    tidyr::pivot_longer(names_to = "key", values_to = "n", c(one, zero)) %>%
    tidyr::uncount(n) %>%
    dplyr::mutate(key2 = dplyr::case_when(key == "one" ~ 1,
                                          key == "zero" ~ 0))
  
  fit_cop <- brms::brm(key2 ~ p_mu2_m, 
                 data = cop2, family = "bernoulli", iter = 2000)
  
  return(list(fit_copr, fit_cop))
  
}

#' Predict ae for extrapolation for community estimates
#'
#' @param result_ext 
#'
#' @return
#' @export
#'
#' @examples
predict_ae <- function(result_ext){
  
  result_ext <- result_ext %>%
    dplyr::mutate(diet2 = dplyr::case_when(
      diet %in% c(1, 3, 5, 6) ~ "2_imix",
      diet == 2 ~ "1_hmd",
      diet %in% c(7, 4) ~ "4_carn",
      diet == 8 ~ "3_plank"
    )) %>%
    dplyr::mutate(group = paste(family, diet2, sep = "_"))
  
  # set prior for ae estimates per diet group
  priors <- brms::prior("normal(0.5, 0.5)", lb = 0, ub = 1, class = "b")
  
  fit1 <- brms::brm(c_a_m|se(c_a_sd) ~ 0 + group + (1|species), 
              prior = priors,
              data = result_ext, 
              cores = 4,
              iter = 4000)
  
  fit2 <- update(fit1, formula = n_a_m|se(n_a_sd) ~ 0 + group + (1|species), 
                 result_ext)
  fit3 <- update(fit1, formula = p_a_m|se(p_a_sd) ~ 0 + group + (1|species), 
                 result_ext)
  
  # summary(fit1)
  # bayes_R2(fit1)
  # bayes_R2(fit2)
  # bayes_R2(fit3)
  # 
  # fixef(fit3)

  pred <- result_ext %>%
    dplyr::select(diet2, family, group) %>%
    unique() %>%
    dplyr::arrange(group) %>%
    dplyr::mutate(
      group_c_a_m = as.vector(brms::fixef(fit1)[,1]),
      group_c_a_sd = as.vector(brms::fixef(fit1)[,2]),
      group_n_a_m = as.vector(brms::fixef(fit2)[,1]),
      group_n_a_sd = as.vector(brms::fixef(fit2)[,2]),
      group_p_a_m = as.vector(brms::fixef(fit3)[,1]),
      group_p_a_sd = as.vector(brms::fixef(fit3)[,2])
    ) 
  pred
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
community_fluxes <- function(result_ext, group_ae) {
  
  # load parameters
  params <- readr::read_csv("data/params_sst_glob.csv") %>%
    dplyr::filter(v_m %in% c(26, 27, 28, 29)) %>%
    dplyr::group_by(Family, species, Species) %>%
    dplyr::summarize_all(mean) %>%
    dplyr::select(-ac_m, -an_m, -ap_m)
  
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
    dplyr::mutate(diet2 = dplyr::case_when(
      diet_cat %in% c(1, 3, 5, 6) ~ "2_imix",
      diet_cat == 2 ~ "1_hmd",
      diet_cat %in% c(7, 4) ~ "4_carn",
      diet_cat == 8 ~ "3_plank"
    )) %>%
    dplyr::mutate(group = paste(Family, diet2, sep = "_")) %>%
    dplyr::mutate(check_species = species %in% result_ext$species,
                  check_group = group %in% group_ae$group) 
    
  sum(moorea$biomass * moorea$check_species)/sum(moorea$biomass)
  #69.9% of biomass species
  sum(moorea$biomass * moorea$check_group)/sum(moorea$biomass)
  #94.0% of biomass groups
  
  # add ae values 
  ae_sp <- result_ext %>%
    dplyr::select(species, c_a_m, c_a_sd, n_a_m, n_a_sd, p_a_m, p_a_sd)
  
  moorea <- moorea %>%
    dplyr::left_join(ae_sp) %>%
    dplyr::left_join(group_ae) %>%
    dplyr::mutate(
      ac_m = dplyr::case_when(
        check_species == TRUE ~ c_a_m,
        check_group == TRUE & check_species == FALSE ~ group_c_a_m,
        TRUE ~ NA_real_),
      an_m = dplyr::case_when(
        check_species == TRUE ~ n_a_m,
        check_group == TRUE & check_species == FALSE ~ group_n_a_m,
        TRUE ~ NA_real_),
      ap_m = dplyr::case_when(
        check_species == TRUE ~ p_a_m,
        check_group == TRUE & check_species == FALSE ~ group_p_a_m,
        TRUE ~ NA_real_),
      ac_sd = dplyr::case_when(
        check_species == TRUE ~ c_a_sd,
        check_group == TRUE & check_species == FALSE ~ group_c_a_sd,
        TRUE ~ NA_real_),
      an_sd = dplyr::case_when(
        check_species == TRUE ~ n_a_sd,
        check_group == TRUE & check_species == FALSE ~ group_n_a_sd,
        TRUE ~ NA_real_),
      ap_sd = dplyr::case_when(
        check_species == TRUE ~ p_a_sd,
        check_group == TRUE & check_species == FALSE ~ group_p_a_sd,
        TRUE ~ NA_real_)
    ) %>%
    dplyr::filter(an_m > 0)
  
  spsize <- moorea %>% 
    dplyr::select(c(colnames(params), size, 
                    ac_m, an_m, ap_m, ac_sd, an_sd, ap_sd)) %>%
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
    
    out <- fishflux::extract(fit, c("Ic", "In", "Ip", "Sc","Sn", "Sp", "Fn", "Fp", "Wc", "Wn", "Wp")) %>%
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
    
    out
  }

  
  data <- spsize
  
  # in pieces to avoid crash
  results1 <- lapply(1:1000, cnp_out) %>%
    dplyr::bind_rows() 
  results2 <- lapply(1001:2000, cnp_out) %>%
    dplyr::bind_rows() 
  results3 <- lapply(2001:nrow(data), cnp_out) %>%
    dplyr::bind_rows() 
  
  results <- dplyr::bind_rows(results1, results2, results3)
  
  result <- cbind(dplyr::select(data, species, size), results)
  
  moo <- moorea %>%
    dplyr::select(
    survey_id, rep_id, trans_id, month, day, year, site_name, lat, long,
    depth, transect_length, transect_width, reef_zone, diet2, species, size, abundance, biomass
  ) %>%
    dplyr::left_join(result)
  
  summ <- moo %>%
    dplyr::mutate(Fn = (Fn * abundance) , Fp = (Fp * abundance), 
                  Wc = (Wc * abundance), Wn = (Wn * abundance), Wp = (Wp * abundance),
                  Ic = (Ic * abundance), In = (In * abundance), Ip = (Ip * abundance),
                  Sc = (Sc * abundance), Sn = (Sn * abundance), Sp = (Sp * abundance))%>%
    dplyr::group_by(survey_id, rep_id, trans_id, month, day, year, site_name, lat, long,
             depth, transect_length, transect_width, reef_zone, diet2) %>%
    dplyr::summarize(Fn = sum(Fn ) , Fp = sum(Fp ), 
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
    dplyr::group_by(year, site_name, reef_zone, diet2) %>%
    dplyr::summarize(Fn = median(Fn), Fp = median(Fp), 
              Wc = median(Wc), Wn = median(Wn), Wp = median(Wp),
              Ic = median(Ic), In = median(In), Ip = median(Ip),
              Sc = median(Sc), Sn = median(Sn), Sp = median(Sp),
              biomass = median(biomass)) %>%
    dplyr::ungroup() 
  
  summ
}




