fit_cop_p <- brms::brm(key2 ~ Wp_median, 
                       data = cop2, family = "bernoulli", iter = 2000, backend = "cmdstanr")


fit_cop_p2 <- brms::brm(key2 ~ log(Wp_median) , 
                       data = cop2, family = "bernoulli", iter = 2000, backend = "cmdstanr")

brms::conditional_effects(fit_cop_p2)
brms::conditional_effects(fit_cop_p)

brms::ranef(fit_cop_p2)


fit_cop_p2 <- brms::brm(prop_eaten ~ (Wp_median) , 
                        data = cop, family = "zero_one_inflated_beta", iter = 2000, backend = "cmdstanr")

summary(fit_cop_p2)

test <- predict(fit_cop_p2)


1/(1+exp(-0))

ggplot() +
  geom_point(aes(x = log(Wp_median), y = prop_eaten), data = cop) +
  geom_point(aes(x = rep(log(cop$Wp_median), cop$n_feces), y = test[,1])) +
  geom_smooth(aes(x = rep(log(cop$Wp_median), cop$n_feces), y = test[,1])) 
  


int <- readr::read_csv("data/intestine_dataset.csv")
int <- int %>%
  dplyr::filter(location == "Moorea") %>%
  mutate(int_rel = intestine_surface/weight) %>%
  dplyr::group_by(family, species) %>%
  dplyr::summarize(int_rel = median(int_rel)) # Ghilardi et al.

data <- result_ext %>%
  left_join(diets) %>%
  left_join(spmass) %>%
  mutate(int_rel2 = log(int_surface/biomass))  %>%
  mutate(negp = ap_mean<0)



ggplot(int) +
  geom_boxplot(aes(x = species, y = int_rel))

test <- int %>% 
  mutate(species = gsub(" ", "_", species)) %>%
  left_join(data)

ggplot(test, aes(x = log(int_rel), y = int_rel2)) +
  geom_point() +
  geom_abline(slope = 1)


data <- test
ggplot(data) +
  geom_point(aes(x = (biomass), y = int_surface)) 

ggplot(data) +
  geom_point(aes(color = log(int_rel2), y = ap_mean, x = Dp_mean), size = 3) +
  # geom_smooth(aes(x = log(int_surface/biomass), y = ap_mean), method = "lm") +
  ylim(c(0,1))

lm(log(int_surface) ~ log(biomass), data = data)

datac <- data %>%
  filter(ac_mean>0) %>%
  left_join(diets)
datan <- data %>%
  filter(an_mean>0) %>%
  left_join(diets)
datap <- data %>%
  filter(ap_mean>0) %>%
  left_join(diets) %>%
  mutate(dietfam = paste0(diet2, family))

ggplot(aes(x = Dc_median, y = ac_median, color = diet2), data = data) +
  geom_point() +
  geom_smooth(method = "lm")


fitc_se <- brm(ac_mean | resp_se(`ac_sd`, sigma = FALSE) ~ me(Dc_mean, Dc_sd)  + log(int_rel),
               backend = "cmdstanr", family = "student", data = datac, iter = 4000, cores = 4, threads = 2)

fitn_se <- brm(an_mean | resp_se(`an_sd`, sigma = FALSE) ~ me(Dn_mean, Dn_sd) +  log(int_rel),
               backend = "cmdstanr", family = "student", data = datan, iter = 4000, cores = 4, threads = 2)

fitp_se <- brm(ap_mean | resp_se(`ap_sd`, sigma = FALSE) ~ me(Dp_mean, Dp_sd) + log(int_rel) ,
               backend = "cmdstanr", family = "student", data = datap, iter = 4000, cores = 4, threads = 2)



