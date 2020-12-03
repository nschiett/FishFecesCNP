
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
      dplyr::filter(n_c1 > 4, n_c2 > 4, n_n1 > 4, n_n2 > 4, n_p1 > 4, n_p2 > 4) %>%
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
#'
#' @return
#' @export
#'
#' @examples
add_traits <- function(result_ae){
  
  # keep only Moorea
  result_ae <- result_ae %>%
    dplyr::filter(location == "Moorea")
  
  # diet category
  diets <- readr::read_csv("data/extrapolation_trophic_guilds.csv") %>%
    dplyr::select(family, species, diet = trophic_guild_predicted) %>%
    rbind(data.frame(
      species = "Aulostomus_chinensis", family = "Aulostomidae",
      diet = 4
    ))
  
  # sia
  sia <- read.csv("data/moorea.sia.2019.csv", sep = ";") %>%
    dplyr::select(-1) %>%
    dplyr::select(species = Names, dn = DN, dc = DC) %>%
    dplyr::mutate(species = gsub("  ", " ", species)) %>%
    dplyr::mutate(species = gsub(" ", "_", species)) %>%
    dplyr::group_by(species) %>%
    dplyr::filter(species %in% result_ae$species) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(n_sia = dplyr::n()) %>%
    dplyr::group_by(species, n_sia) %>%
    dplyr::summarize_all(median, na.rm = TRUE) 
  
  # combine
  result <- result_ae %>% 
    dplyr::left_join(diets) %>%
    dplyr::left_join(sia)
  
  result

  }



