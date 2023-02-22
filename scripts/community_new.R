library(fishflux)
library(drake)
library(tidyverse)
loadd(models_copro)


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

result_ext <- read_csv("output/data/result_ae_predict.csv")

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


loadd(diets)
moorea <- moorea %>%
  dplyr::inner_join(diets) %>%
  dplyr::left_join(ae_sp) %>%
  dplyr::left_join(ae_diet) %>%
  #dplyr::filter(species %in% result_ext$species)
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

prob_cop <- unique(select(data, diet2, species)) %>%
  left_join(prob_cop) %>%
  group_by(diet2) %>%
  mutate(prob_cop_diet = median(prob_cop, na.rm = TRUE)) %>%
  mutate(prob_cop = dplyr::coalesce(prob_cop, prob_cop_diet)) %>%
  filter(species %in% results1$species)

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
  #dplyr::filter(year == 2016) %>%
  dplyr::group_by(reef_zone, site_name, year, diet2) %>%
  dplyr::summarize(Fn = median(Fn), Fp = median(Fp), 
                   Wc = median(Wc), Wn = median(Wn), Wp = median(Wp),
                   Ic = median(Ic), In = median(In), Ip = median(Ip),
                   Sc = median(Sc), Sn = median(Sn), Sp = median(Sp),
                   biomass = median(biomass),
                   pflux_cop = median(pflux_cop)) %>%
  ungroup()

write_csv(summ, "output/data/comfluxes2.csv")


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
  summarize_all(mean, na.rm = TRUE)

data <- pflux %>% filter(reef_zone == "forereef")
sum(data$Wp_r + data$pcop_r)
#0.6528274
sum(data$Fp_r)
#  0.1349268
data <- data.frame(
  prop = c(sum(data$Wp_r + data$pcop_r), sum(data$Fp_r)))

ggplot(data, aes(x="", y = c(0.6528274, 0.1349268), fill = c("Wp", "Fp"))) +
  geom_bar(stat="identity", width=1) +
  scale_y_continuous(limits = c(0,1)) +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(c(82.4, 8.7), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_void()
ggsave("pie.pdf")

ggplot(pflux) +
  geom_boxplot(aes(x = as.character(year), y = log(N_ratio))) +
  facet_grid(reef_zone~site_name)

write_csv(pflux, "output/data/pcomflux.csv")

test <- pflux %>%
  filter(reef_zone == "forereef")





