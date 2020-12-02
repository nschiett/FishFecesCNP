

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
  filter(!is.na(lwa_m)) %>%
  select(-ac_m, -an_m, -ap_m)

tia <- moorea %>%
  filter(Site_name == "Tiahura") %>%
  mutate(biomass = lwa_m * Size ^ lwb_m) %>%
  filter(biomass > 0) %>%
  group_by(TransID, Year, ReefZone, diet_cat) %>%
  summarize(biomass = sum(Abundance * biomass))

ggplot(tia[tia$ReefZone == "forereef",]) +
  geom_point(aes(x = Year, y = log(biomass), color = as.character(diet_cat))) +
  geom_smooth(aes(x = Year, y = log(biomass), color = as.character(diet_cat))) +
  theme_bw()
  

loadd(result_ae)

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














