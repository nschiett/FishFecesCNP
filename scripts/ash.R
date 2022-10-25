
########## ash ############

library(tidyverse)
library(stringr)

ash_gut <- read_csv("data/ash_gut_content.csv") %>%
  mutate(ash_diet = as.numeric(ash_diet)) %>%
  filter(!ash_diet == 1) %>%
  drop_na(ash_diet) %>%
  group_by(species) %>%
  mutate(n_diet = n()) %>%
  dplyr::filter(n_diet>1) %>%
  drop_na(species) %>%
  ungroup()

ash_gut_sum <- ash_gut %>%
  group_by(species) %>%
  summarize(ash_diet = median(ash_diet, na.rm = TRUE), n_diet = n()) %>%
  drop_na(ash_diet)

ash_feces1 <- read_csv("data/ash_summary_moorea.csv") %>%
  drop_na(genus_species) %>%
  mutate(species = str_to_sentence(str_to_lower(genus_species))) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  select(-1) %>% 
  mutate(ash_poo = ash_percent/100) %>%
  select(fish_id, species, ash_poo) 

ash_feces2 <- read_csv("data/ash_feces2.csv") %>%
  mutate(ash_poo = mean_ash_percent/100) %>%
  select(fish_id, species, ash_poo) 

ash_feces <- rbind(ash_feces1, ash_feces2) %>%
  drop_na(ash_poo)  %>%
  group_by(species) %>%
  mutate(n_poo = n()) %>%
  dplyr::filter(n_poo>1) %>%
  ungroup()

ash_feces_sum <- ash_feces %>%
  group_by(species) %>%
  summarize(ash_poo = median(ash_poo, na.rm = TRUE), n_poo = n()) %>%
  drop_na(ash_poo) 
  
ashh <- dplyr::left_join(ash_gut_sum, ash_feces_sum) %>%
  mutate(ash_ratio = ash_diet/ash_poo)

test <- left_join(result_ext, ashh) %>%
  mutate(rest1 = 1 - (Dc_mean + Dn_mean + Dp_mean)/100)

hist(test$rest1)

test <- left_join(data_ae, full_join(ash_gut, ash_feces2)) 
diet <- filter(test, key == "AE1") %>%
  drop_na(c, n, p, ash_diet) %>%
  select(id, species, diet2, c, n, p, ash_diet) %>%
  mutate(sum = (c + n + p)/100 + ash_diet) %>%
  filter(sum<1) %>%
  mutate(ash_scaled = ash_diet/(1-(c + n + p)/100))

poo <-  left_join(data_ae, ash_feces2) %>%
  filter(key == "AE2") %>%
  drop_na(c, n, p, ash_poo) %>%
  left_join(select(result_ext, species, diet2)) %>%
  select(id, species, diet2, c, n, p, ash_poo) %>%
  mutate(sum = (c + n + p)/100 + ash_poo) %>%
  filter(sum<1) %>%
  mutate(ash_scaled = ash_poo/(1-(c + n + p)/100))

ggplot(poo) +
  geom_point(aes(x = c, y = ash_scaled, color = diet2))


fitb <- brm(ash_scaled ~ log(c) + diet2 + (1|species), 
            data = diet, family = "beta")

fitb2 <- update(fitb, ash_scaled ~ log(c) + diet2 + (1|species), newdata = poo)

newdata <- select(result_ext, diet2, species, c = Dc_mean, n = Dn_mean, p = Dp_mean)  %>%
  mutate(sp = species, species = NA)
pred1 <- cbind(newdata,
                 fitted(fitb, newdata, allow_new_levels = TRUE)) %>%
  mutate(ash_diet = Estimate * (1-(c+n+p)/100)) %>%
  select(species = sp, ash_diet)

newdata[newdata$diet2 == "1_detr", "diet2"] <- NA
pred2 <- cbind(newdata,
               fitted(fitb2, newdata, allow_new_levels = TRUE)) %>%
  mutate(ash_poo = Estimate * (1-(c+n+p)/100)) %>%
  select(species = sp, ash_poo)

pred <- left_join(pred1, pred2) %>%
  mutate(ash_ratio = ash_diet/ash_poo)

bayes_R2(fitb)
bayes_R2(fitb2)

summary(fitb)
summary(fitb2)
pp_check(fitb)
conditional_effects(fitb)
ggplot(diet) +
  geom_point(aes(x = log(c), y = ash_scaled, color = diet2))

tl <- plyr::ldply(lapply(gsub("_", " ", unique(test$species)), fishflux::trophic_level))
tl <- rfishbase:::ecology(gsub("_", " ", unique(test$species)), "FoodTroph") %>%
  mutate(species =  unique(test$species))

fishflux::name_errors(unique(test$species))


test <- left_join(result_ext, ashh) %>%
  mutate(rest1 = 1 - (Dc_mean + Dn_mean + Dp_mean)/100)
test <- left_join(test, tl) %>%
  mutate(ash_scaled1 = ash_diet/rest1,
         ash_scaled2 = ash_poo/(1 - (Wc_mean + Wn_mean + Wp_mean)/100))

ggplot(test) +
  geom_point(aes(y = ash_scaled1, x = Dc_mean, color = diet2)) +
  geom_abline(slope = 1)

ggplot(test) +
  geom_point(aes(y = ash_poo, x = Wc_mean))
ggplot(test) +
  geom_point(aes(y = ash_scaled2, x = log(Wc_mean), color = diet2))
ggplot(test) +
  geom_point(aes(y = ash_poo, x = Wc_mean, color = diet2))+
  geom_text(aes(y = ash_poo, x = Wc_mean, label = species, color = diet2))

fit <- brm(ash_scaled1 ~ log(Dc_mean) + (1|diet2), 
            data = test[test$ash_scaled1<1,], family = "beta")

fit2 <- update(fitb, ash_scaled2  ~ log(Wc_mean) + (1|diet2), 
               newdata = test[test$ash_scaled2<1,])

bayes_R2(fit)
bayes_R2(fit2)

pp_check(fit2)
conditional_effects(fit)
conditional_effects(fit2)
ranef(fitb)

ggplot(test) +
  geom_boxplot(aes(y = ash_scaled1, x = diet2))
ggplot(test) +
  geom_boxplot(aes(y = ash_scaled2, x = diet2))

ggplot(ash_gut) +
  geom_boxplot(aes(x = species, y = ash_diet), outlier.alpha = 0) +
  geom_jitter(aes(x = species, y = ash_diet)) +
  coord_flip() 

ggplot(ash_feces) +
  geom_boxplot(aes(x = species, y = ash_poo), outlier.alpha = 0) +
  geom_jitter(aes(x = species, y = ash_poo)) +
  coord_flip()

library(brms)

diets <- select(result_ext, species, diet2)

ash_gut <- ash_gut %>%
  left_join(diets) %>%
  drop_na(ash_diet) %>%
  group_by(species) %>%
  mutate(n = n())

fit_ash1 <- brm(ash_diet ~ diet2 + (1|species), 
                family = "student", data = ash_gut)

newdata <- select(ash_gut, species, diet2) %>% unique() %>% drop_na(diet2)
pred1 <- cbind(newdata, fitted(fit_ash1, newdata = newdata)) %>%
  select(species, diet2, ash_diet = Estimate)

ash_feces <- ash_feces %>%
  left_join(diets)

fit_ash2 <- brm(ash_poo ~ diet2 + (1|species), 
                family = "student", data = ash_feces)

newdata <- select(ash_feces, species, diet2) %>% unique() %>% drop_na(diet2)
pred2 <- cbind(newdata, fitted(fit_ash2, newdata = newdata))%>%
  select(species, diet2, ash_feces = Estimate)

pred <- left_join(pred1, pred2) %>%
  mutate(ash_ratio = ash_diet/ash_feces) %>%
  select(species, diet2, ash_ratio)

# per diet
newdata <- select(ash_gut, diet2) %>% unique() %>% drop_na(diet2) %>%
  mutate(species = NA)
pred1 <- cbind(newdata, fitted(fit_ash1, newdata = newdata)) %>%
  select(diet2, ash_diet = Estimate)
newdata <- select(ash_feces, diet2) %>% unique() %>% drop_na(diet2) %>%
  mutate(species = NA)
pred2 <- cbind(newdata, fitted(fit_ash2, newdata = newdata))%>%
  select(diet2, ash_feces = Estimate)
pred_diet <- left_join(pred1, pred2) %>%
  mutate(ash_ratio = ash_diet/ash_feces) %>%
  select(diet2, ash_ratio)
ggplot(pred) +
  geom_point(aes(x = ash_ratio, y = species, color = diet2))


drake::loadd(result_ext)

ashsp <- select(result_ext, species, diet2) %>%
  left_join(pred) %>%
  mutate(ash_ratio = case_when(species == "Acanthurus_lineatus" ~ 0.86,
                               species == "Scarus_schlegeli" ~ 0.62,
                               diet2 == "5_plank" ~ 0.7,
                               ash_ratio > 1 ~ NA_real_,
                               TRUE ~ ash_ratio)) %>%
  full_join(pred_diet, by = "diet2") %>%
  mutate(source_ash = case_when(diet2 == "5_plank" ~ "Bailey & Robertson (1982)",
                            species == "Acanthurus_lineatus" ~ "Crossman et al. (2005)",
                            species == "Scarus_schlegeli" ~ "Crossman et al. (2005)",
                            is.na(ash_ratio.x) ~ "This study, diet-level estimate",
                            TRUE ~ "This study, species-level estimate")) %>%
  mutate(ash_ratio = coalesce(ash_ratio.x, ash_ratio.y)) %>%
  select(species, diet2, ash_ratio, source_ash)
write_csv(ashsp, "output/data/ash_species_estimate.csv")
read_csv( "output/data/ash_species_estimate.csv")

