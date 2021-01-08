
library(readr)
library(tidyverse)
diets <- read_csv("data/extrapolation_trophic_guilds.csv") %>%
  select(species, diet = trophic_guild_predicted) 


diets <- read_csv("data/extrapolation_trophic_guilds.csv") %>%
  select(species, diet = trophic_guild_predicted) 


params <- read_csv("data/params_sst_glob.csv") %>%
  filter(v_m %in% c(26, 27, 28, 29)) %>%
           group_by(Family, species, Species) %>%
           summarize_all(mean) %>%
  select(-ac_m, -an_m, -ap_m)

moorea <- read_csv("data/moorea_uvc.csv") %>%
  filter(Island == "Moorea") %>%
  filter(Size > 0) %>%
  select(SurveyID, RepID, TransID, Month, Day, Year, Site_name, Lat, Long, Depth, TransectLength, TransectWidth, ReefZone,
         Taxon, Abundance, Size) %>%
  group_by(SurveyID, RepID, TransID, Month, Day, Year, Site_name, Lat, Long, Depth, TransectLength, TransectWidth, ReefZone,
           Taxon, Size) %>%
  summarize(Abundance = sum(Abundance)) %>%
  ungroup() %>%
  mutate(species = gsub(" ", "_", Taxon)) %>%
  left_join(params) %>%
  filter(!is.na(lwa_m)) 

tia <- moorea %>%
  filter(Site_name == "Tiahura") %>%
  mutate(biomass = lwa_m * Size ^ lwb_m) %>%
  filter(biomass > 0) %>%
  group_by(TransID, Year, ReefZone, diet_cat) %>%
  summarize(biomass = sum(Abundance * biomass))


moo <- moorea %>%
  #filter(Site_name == "Tiahura") %>%
  mutate(biomass = lwa_m * Size ^ lwb_m) %>%
  filter(biomass > 0) %>%
  group_by(TransID, Year, ReefZone, diet_cat, Site_name) %>%
  summarize(biomass = sum(Abundance * biomass))

ggplot(moo[moo$ReefZone == "forereef",]) +
  #geom_point(aes(x = Year, y = (biomass), color = as.character(Site_name))) +
  geom_smooth(aes(x = Year, y = (biomass), color = as.character(Site_name)), se = FALSE) +
  theme_bw() +
  facet_wrap(~as.character(diet_cat), scales = "free")
  

loadd(result_ext)

data <- result_ae %>% 
  left_join(params) %>%
  group_by(diet_cat, Family) %>%
  summarise_all(mean) %>%
  select(Family, diet_cat, 
         ac_m = c_a_m, an_m = n_a_m, ap_m = p_a_m) %>%
  right_join(moorea) %>%
  group_by(diet_cat) %>%
  mutate(ac_m = ifelse(is.na(ac_m), mean(ac_m, na.rm = TRUE), ac_m),
         an_m = ifelse(is.na(an_m), mean(an_m, na.rm = TRUE), an_m),
         ap_m = ifelse(is.na(ap_m), mean(ap_m, na.rm = TRUE), ap_m)) %>%
  ungroup()

library(fishflux)


cnp_out <- function(x){
  
  d <- data[x,]
  size <- simplify(d$Size)
  p <- d %>%
    select(k_m, Qc_m ,Qn_m, Qp_m, Dc_m, Dn_m, Dp_m, alpha_m, f0_m,          
           theta_m, lwa_m,  lwb_m, r_m, h_m,  v_m, linf_m,        
           F0nz_m, F0pz_m, ac_m, an_m, ap_m) %>% simplify() %>% as.list()
  
  fit <- cnp_model_mcmc(TL = size, param = p)
  
  out <- fishflux::extract(fit, c("Fn", "Fp", "Wc", "Wn", "Wp"))
  
  out
}

standard <- function(x){
  (x - mean(x))/sd(x)
}

data_sp_size <- data %>% 
  select(Family, diet_cat, species, Size, k_m, Qc_m ,Qn_m, Qp_m, Dc_m, Dn_m, Dp_m, alpha_m, f0_m,          
                        theta_m, lwa_m,  lwb_m, r_m, h_m,  v_m, linf_m,        
                        F0nz_m, F0pz_m, ac_m, an_m, ap_m) %>%
  unique()


results <- lapply(1:nrow(data_sp_size), cnp_out) %>%
  plyr::ldply() 
results <- cbind(data_sp_size, results)

summ <- left_join(data, results) %>%
  mutate(biomass = Abundance * (lwa_m * Size ^ lwb_m)) %>%
  group_by(SurveyID, RepID, TransID, Month, Day, Year, Site_name, Lat, Long, 
           Depth, TransectLength, TransectWidth, ReefZone) %>%
  summarize(Fn = sum(Fn_mean), Fp = sum(Fp_mean), Wc = sum(Wc_mean), Wn = sum(Wn_mean), Wp = sum(Wp_mean),
            biomass = sum(biomass), Wnp = mean(Wn_mean/Wp_mean), Wcn = mean(Wc_mean/Wn_mean)) %>%
  ungroup() %>%
  mutate(Fn = Fn/(TransectLength*TransectWidth),
         Fp = Fp/(TransectLength*TransectWidth),
         Wc = Wc/(TransectLength*TransectWidth),
         Wn = Wn/(TransectLength*TransectWidth),
         Wp = Wp/(TransectLength*TransectWidth),
         biomass = biomass/(TransectLength*TransectWidth)) %>%
  filter(ReefZone == "forereef") %>%
  group_by(Year, Site_name, TransID) %>%
  summarize(Fn = mean(Fn), Fp = mean(Fp), 
            Wc = mean(Wc), Wn = mean(Wn), Wp = mean(Wp),
            biomass = mean(biomass), Wnp = mean(Wnp), Wcn = mean(Wcn)) %>%
  ungroup() %>% 
  group_by(Site_name) %>%
  mutate(Fn_st = standard(Fn), Fp_st = standard(Fp), 
         Wc_st = standard(Wc), Wn_st = standard(Wn), Wp_st = standard(Wp),
         biomass_st = standard(biomass))

ggplot(summ, aes(x = Year)) +
  geom_smooth(aes(y = biomass), color = "black", span = 0.6, linetype = 2) + 
  theme_bw()  +
  facet_wrap(~Site_name, scale = "free") +
  labs(y = "Biomass (g/m2)")
ggsave("output/plots/moorea_biomass.png")


ggplot(summ, aes(x = Year)) +
  geom_point(aes(y = Wc, group = Site_name), 
             color = "black", size = 1, alpha = 0.5) +
  geom_smooth(aes(y = Wc, group = Site_name), color = "black", span = 0.6) + 
  theme_bw() +
  facet_wrap(~Site_name, scale = "free") +
  labs(y = "Egestion rate (gC/day/m2)")
ggsave("output/plots/moorea_egestionrate.png")


ggplot(summ, aes(x = Year)) +
  geom_point(aes(y = Wc/biomass, group = Site_name), 
             color = "black", size = 1, alpha = 0.5) +
  geom_smooth(aes(y = Wc/biomass, group = Site_name), color = "black", span = 0.6) + 
  theme_bw() +
  facet_wrap(~Site_name, scale = "free") +
  labs(y = "Egestion per unit biomass")
ggsave("output/plots/moorea_egestionratebiomass.png")


ggplot(summ, aes(x = Year)) +
  geom_point(aes(y = Wn/Wp, group = Site_name), 
             color = "black", size = 1, alpha = 0.5) +
  geom_smooth(aes(y = Wn/Wp, group = Site_name), color = "black", span = 0.6) + 
  theme_bw() +
  facet_wrap(~Site_name) +
  labs(y = "Egestion ratio N:P")

ggplot(summ, aes(x = Year)) +
  geom_point(aes(y = Wc/Wn, group = Site_name), 
             color = "black", size = 1, alpha = 0.5) +
  geom_smooth(aes(y = Wc/Wn, group = Site_name), color = "black", span = 0.6) + 
  theme_bw() +
  facet_wrap(~Site_name) +
  labs(y = "Egestion ratio C:N")



########### rls #############"
rls <- read_csv("data/rls_french_polynesia.csv")


rls <- rls %>%
  filter(size_class > 0) %>%
  group_by(survey_id, area, ecoregion, realm, site_code, site_name, latitude, longitude, survey_date, block, depth, location, diver, family, taxon, size_class) %>%
  summarize(abundance = sum(total)) %>%
  ungroup() %>%
  mutate(species = gsub(" ", "_", taxon)) %>%
  left_join(params) %>%
  filter(!is.na(lwa_m)) %>%
  mutate(biomass = lwa_m * size_class ^ lwb_m) %>%
  filter(biomass > 0) %>%
  group_by(survey_id, area, ecoregion, realm, site_code, site_name, latitude, longitude, survey_date, block, depth, location, diver, diet_cat) %>%
  summarize(biomass = sum(abundance * biomass)) 

rls <- rls %>%
  mutate(survey_date = lubridate::dmy(survey_date))
summary(rlstest$survey_date)

ggplot(rls[rls$area == "Society Islands",]) +
  geom_point(aes(x = site_name, y = biomass, color = as.character(diet_cat))) +
  geom_boxplot(aes(x = site_name, y = biomass, fill = as.character(diet_cat)), alpha = 0.5) +
  coord_flip()
  

######## prediction ae ###########
loadd(result_ext)
devtools::load_all()
trl <- lapply(gsub("_", " ", unique(result_ext$species)),
              function(x){fishflux::trophic_level(x)}) %>%
  plyr::ldply()

trl <- trl %>%
  dplyr::mutate(species = gsub(" ", "_", species)) %>%
  dplyr::left_join(result_ext)

library(ggplot2)

ggplot(trl) +
  geom_point(aes((trophic_level), c_mu1_m, color = family))
ggplot(trl) +
  geom_point(aes((trophic_level), n_mu1_m, color = family))
ggplot(trl) +
  geom_point(aes((trophic_level), p_mu1_m, color = family))
ggplot(trl) +
  geom_point(aes((p_mu1_m), p_a_m, color = family))


library(brms)

fit1 <- brm(
  bf(c_a_m ~ c_mu1_m + (1|family)) +
  bf(c_mu1_m ~ trophic_level + (1|family)), 
  data = trl)
fit2 <- brm(
  bf(n_a_m ~ n_mu1_m + (1|family)) +
    bf(n_mu1_m ~ trophic_level + (1|family)), 
  data = trl)
fit3 <- brm(
  bf(p_a_m ~ p_mu1_m + (1|family)) +
    bf(p_mu1_m ~ trophic_level + (1|family)), 
  data = trl)


fit2 <- brm(n_mu1_m ~ trophic_level, data = trl)
fit3 <- brm(p_mu1_m ~ trophic_level, data = trl)

fixef(fit1)
ranef(fit1)
fixef(fit2)
fixef(fit3)
marginal_effects(fit1)
marginal_effects(fit2)
marginal_effects(fit3)

bayes_R2(fit1)
bayes_R2(fit2)
bayes_R2(fit3)


plot(fitted(fit1)[,1,1], fit1$data$c_a_m)
plot(fitted(fit2)[,1,1], fit2$data$n_a_m)
plot(fitted(fit3)[,1,1], fit3$data$p_a_m)

# predict
moosp <- dplyr::select(moorea, species, Family) %>%
  unique() %>%
  dplyr::filter(Family %in% trl$family)

# idea: groups of diet category
# 1) HMD (2)
# 2) Carnivores (4 + 7)
# 3) Mixed Invertivores (1, 3, 5, 6)
# 4) Planktivores (8)

ggplot(result_ext) +
  geom_boxplot(aes(x = as.character(diet2), y = p_a_m, color = family), 
width = 0.5)
ggplot(result_ext) +
  geom_jitter(aes(x = as.character(diet), y = n_mu1_m, color = as.character(diet)))
ggplot(result_ext) +
  geom_jitter(aes(y = as.character(family), x = n_a_m, color = as.character(diet)))

sum <- result_ext %>%
  mutate(diet2 = case_when(
    diet %in% c(1, 3, 5, 6) ~ "2_imix",
    diet == 2 ~ "1_hmd",
    diet %in% c(7, 4) ~ "4_carn",
    diet == 8 ~ "3_plank"
  )) %>%
  group_by(family, diet2) %>%
  summarize(ac = median(c_a_m), an = median(n_a_m), ap = median(p_a_m))

priors <- prior("normal(0.5, 0.5)", lb = 0, ub = 1, class = "b")

result_ext <- result_ext %>%
  dplyr::mutate(group = paste(family, diet2, sep = "_"))

fit1 <- brm(c_a_m|se(c_a_sd) ~ 0 + group + (1|species), 
            prior = priors,
            data = result_ext, cores = 1 )

summary(fit1)
bayes_R2(fit1)


fit2 <- update(fit1, formula = n_a_m ~ 0 + diet2 + (1|family), result_ext, cores = 4)
fit3 <- update(fit1, formula = p_a_m ~ 0 + diet2 + (1|family), result_ext, cores = 4)

fit1e <- brm(c_a_m|se(c_a_sd) ~ 0 + diet2 + (1|family), 
            prior = priors,
            data = result_ext, cores = 1 )
fit2e <- update(fit1e, formula = n_a_m|se(n_a_sd) ~ 0 + diet2 + (1|family), result_ext, cores = 4)
fit3e <- update(fit1e, formula = p_a_m|se(p_a_sd) ~ 0 + diet2 + (1|family), result_ext, cores = 4)


summary(fit1e)
bayes_R2(fit3)

fixef(fit3)
fixef(fit3e)

newdata <- result_ext %>%
  dplyr::select(diet2, family) %>%
  unique() %>%
  dplyr::mutate(c_a_sd = 0.01)
pred <- cbind(newdata, fitted(fit1e, newdata))
