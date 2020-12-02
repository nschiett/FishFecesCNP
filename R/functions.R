
remove_outliers <- function(data){
    data <- 
      data %>%
      group_by(species) %>%
      mutate(outlier = 
               c > exp(mean(log(c), na.rm = TRUE) + (3 * sd(log(c), na.rm = TRUE))) |
               n > exp(mean(log(n), na.rm = TRUE) + (3 * sd(log(n), na.rm = TRUE))) |
               p > exp(mean(log(p), na.rm = TRUE) + (3 * sd(log(p), na.rm = TRUE)))) %>%
      filter(outlier == FALSE | is.na(outlier)) %>%
      select(-outlier) %>%
      ungroup() %>%
      group_by(species, location) %>%
      mutate(n_c1 = sum(c > 0 & key == "AE1", na.rm = TRUE),
             n_c2 = sum(c > 0 & key == "AE2", na.rm = TRUE),
             n_n1 = sum(n > 0 & key == "AE1", na.rm = TRUE),
             n_n2 = sum(n > 0 & key == "AE2", na.rm = TRUE),
             n_p1 = sum(p > 0 & key == "AE1", na.rm = TRUE),
             n_p2 = sum(p > 0 & key == "AE2", na.rm = TRUE)) %>%
      filter(n_c1 > 4, n_c2 > 4, n_n1 > 4, n_n2 > 4, n_p1 > 4, n_p2 > 4) %>%
      mutate(species = gsub(" ", "_", species)) %>%
      mutate(sploc = paste(species, location, sep = "__")) %>%
      ungroup()
    data
}


ae_mod <- function(model, data, element){
  
  x1 <- filter(data, key == "AE1") %>%
    filter(!is.na(.data[[element]])) %>%
    select(.data[[element]]) %>%
    simplify()
  x2 <- filter(data, key == "AE2") %>%
    filter(!is.na(.data[[element]])) %>%
    select(.data[[element]]) %>%
    simplify()
  
  data <- list(
    N1 = length(x1),
    N2 = length(x2),
    x1 = x1, 
    x2 = x2
  )
  
  fit <- sampling(model, data)
  
  sum <- summary(fit)$summary[c("mu1", "mu2", "a"), -c(2,9,10)]
  
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

run_ae_models <- function(data, stanmodel){
  
  sploc <- select(data, species, location) %>%
    unique()
  
  c <- lapply(1:nrow(sploc), function(i){
    sp <- simplify(sploc[i, "species"])
    loc <- simplify(sploc[i, "location"])
    
    d <- filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "c")
    
  }) %>% plyr::ldply()
  
  n <- lapply(1:nrow(sploc), function(i){
    sp <- simplify(sploc[i, "species"])
    loc <- simplify(sploc[i, "location"])
    
    d <- filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "n")
    
  }) %>% plyr::ldply()
  
  p <- lapply(1:nrow(sploc), function(i){
    sp <- simplify(sploc[i, "species"])
    loc <- simplify(sploc[i, "location"])
    
    d <- filter(data, species == sp, location == loc)
    
    res <- ae_mod(stanmodel, data = d, element = "p")
    
  }) %>% plyr::ldply()
  
  result <- cbind(sploc, c, n, p)
  
  result
  
}

# 
# run_sploc_models <- function(data){
#   
#   ##### C #####
# 
#   fit_c <- brm(c/100 ~ 0 + sploc + sploc:key, data = data, 
#                family = "beta",
#                chains = 4, cores = 2)
#   
#   ##### N #####
# 
#   fit_n <- brm(n/100 ~ 0 + sploc + sploc:key, data = data, 
#                chains = 4, cores = 2, family = "beta")
#   
#   ##### P #####
#  
#   fit_p <- brm(p/100 ~ 0 + sploc + sploc:key, data = data,
#                chains = 4, cores = 2, family = "beta")
#   
#   return(list(fit_c, fit_n, fit_p))
# 
# }
# 
# 
# extract_cnp <- function(list, data){
#   
#   newdata <- unique(select(data, species, location, sploc, key))
#   
#   ##### C #####
#   pred <- cbind(newdata, fitted(list[[1]], newdata = newdata) ) 
#   
#   pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
#                                  values_from = Estimate, names_prefix = "m_")
#   pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
#                                   values_from = Est.Error, names_prefix = "sd_")
#   
#   pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
#                                   values_from = Q2.5, names_prefix = "lb_")
#   pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
#                                   values_from = Q97.5, names_prefix = "ub_")
#   
#   pred_c <- left_join(pred_m, pred_sd) %>%
#     left_join(pred_lb) %>%
#     left_join(pred_ub) 
#   
#   colnames(pred_c) <-
#     c("species", "location", "sploc", "c1_m", "c2_m", "c1_sd", "c2_sd", "c1_lb", "c2_lb", "c1_ub", "c2_ub")
#   
#   ##### N #####
#   pred <- cbind(newdata, fitted(list[[2]], newdata = newdata) ) 
#   
#   pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
#                                  values_from = Estimate, names_prefix = "m_")
#   pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
#                                   values_from = Est.Error, names_prefix = "sd_")
#   
#   pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
#                                   values_from = Q2.5, names_prefix = "lb_")
#   pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
#                                   values_from = Q97.5, names_prefix = "ub_")
#   
#   pred_n <- left_join(pred_m, pred_sd) %>%
#     left_join(pred_lb) %>%
#     left_join(pred_ub) 
#   
#   colnames(pred_n) <-
#     c("species", "location", "sploc", "n1_m", "n2_m", "n1_sd", "n2_sd", "n1_lb", "n2_lb", "n1_ub", "n2_ub")
#   
#   ##### P #####
#   pred <- cbind(newdata, fitted(list[[3]], newdata = newdata) ) 
#   
#   pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
#                                  values_from = Estimate, names_prefix = "m_")
#   pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
#                                   values_from = Est.Error, names_prefix = "sd_")
#   
#   pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
#                                   values_from = Q2.5, names_prefix = "lb_")
#   pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
#                                   values_from = Q97.5, names_prefix = "ub_")
#   
#   pred_p <- left_join(pred_m, pred_sd) %>%
#     left_join(pred_lb) %>%
#     left_join(pred_ub) 
#   
#   colnames(pred_p) <-
#     c("species", "location", "sploc", "p1_m", "p2_m", "p1_sd", "p2_sd", "p1_lb", "p2_lb", "p1_ub", "p2_ub")
#   
#   left_join(pred_c, pred_n) %>%
#     left_join(pred_p)
# }
# 
# run_ae_models <- function(data){
#   
#   fit_c <- brm(data = data,
#                 formula = c2_m|se(c2_sd) ~ 0 + sploc:me(c1_m, c1_sd), chains = 4, cores = 2,
#                 prior = prior_string("beta(2, 2)", class = "b"))
#  
#   fit_n <- brm(data = data,
#                formula = n2_m|se(n2_sd) ~ 0 + sploc:me(n1_m, n1_sd), chains = 4, cores = 2,
#                prior = prior_string("beta(2, 2)", class = "b"))
# 
#   fit_p <- brm(data = data,
#                formula = p2_m|se(p2_sd) ~ 0 + sploc:me(p1_m, p1_sd), chains = 4, cores = 2,
#                prior = prior_string("beta(2, 2)", class = "b"))
#   
#   return(list(fit_c, fit_n, fit_p))
#   
# }
# 
# extract_ae <- function(list){
#   
#   sam <- 1 - fixef(list[[1]], summary = FALSE) 
#   
#   ac <- data.frame(
#     sploc = sort(unique(list[[1]]$data$sploc)),
#     ac_m = apply(sam, 2, median),
#     ac_sd = apply(sam, 2, sd),
#     ac_lb = apply(sam, 2, quantile, 0.025),
#     ac_ub = apply(sam, 2, quantile, 0.975)
#   )
#   
#   sam <- 1 - fixef(list[[2]], summary = FALSE) 
#   
#   an <- data.frame(
#     sploc = sort(unique(list[[2]]$data$sploc)),
#     an_m = apply(sam, 2, median),
#     an_sd = apply(sam, 2, sd),
#     an_lb = apply(sam, 2, quantile, 0.025),
#     an_ub = apply(sam, 2, quantile, 0.975)
#   )
#   
#   sam <- 1 - fixef(list[[3]], summary = FALSE) 
#   
#   ap <- data.frame(
#     sploc = sort(unique(list[[3]]$data$sploc)),
#     ap_m = apply(sam, 2, median),
#     ap_sd = apply(sam, 2, sd),
#     ap_lb = apply(sam, 2, quantile, 0.025),
#     ap_ub = apply(sam, 2, quantile, 0.975)
#   )
#   
#   left_join(ac, an) %>%
#     left_join(ap)
#     
# }
# 
# 
# 
