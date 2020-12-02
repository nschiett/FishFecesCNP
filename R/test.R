data <- readd(data_ae)

fit1 <- brm(
  mvbind(tarsus, back) )

fit <- brm(c ~ 0 + sploc + sploc:key, data = data, prior = priors, 
             chains = 4, cores = 2, family = "normal")

##### C #####
priors <- c(prior_string("normal(30, 14)", class = "b", 
                         coef = paste("sploc", unique(data$sploc), sep="")),
            prior_string("normal(-15, 10)", class = "b", ub = 0, 
                         coef = paste("sploc", unique(data$sploc), ":keyAE2", sep="")))

fit_c <- brm(c ~ 0 + sploc + sploc:key, data = data, prior = priors, 
             chains = 4, cores = 2, family = "normal")



priors <- c(set_prior('normal(30, 15)', class = 'b', lb = 0, nlpar = 'par1'),
            set_prior('normal(-15, 10)', class = 'b', ub = 0, nlpar = 'par2'))

model <- brm(bf(c ~ par1 + par2, par1 ~ 0 + sploc, par2 ~ 0 + sploc:key, nl = TRUE),
            prior = priors, data = data)

test <- readd(result_ae)


ggplot(test) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 50, y = 0.3 * 50, label = "AE = 0.7")) +
  geom_label(aes(x = 50, y = 0.5 * 50, label = "AE = 0.5")) +
  geom_label(aes(x = 50, y = 0.7 * 50, label = "AE = 0.3")) +
  geom_label(aes(x = 50, y = 0.9 * 50, label = "AE = 0.1")) +
  geom_point(aes(x = c1_m, y = c2_m, color = ac_m), size = 3) +
  geom_text_repel(aes(x = c1_m, y = c2_m, label = species), size = 3) +
  scale_color_fish(option = "Trimma_lantana") +
  theme_bw()

ggplot(test) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 13, y = 0.3 * 13, label = "AE = 0.7")) +
  geom_label(aes(x = 13, y = 0.5 * 13, label = "AE = 0.5")) +
  geom_label(aes(x = 13, y = 0.7 * 13, label = "AE = 0.3")) +
  geom_label(aes(x = 13, y = 0.9 * 13, label = "AE = 0.1")) +
  geom_point(aes(x = n1_m, y = n2_m, color = an_m), size = 3) +
  geom_text_repel(aes(x = n1_m, y = n2_m, label = species), size = 3) +
  scale_color_fish(option = "Trimma_lantana") +
  theme_bw()

ggplot(test[test$p2_m < test$p1_m,]) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 2.5, y = 0.3 * 2.5, label = "AE = 0.7")) +
  geom_label(aes(x = 2.5, y = 0.5 * 2.5, label = "AE = 0.5")) +
  geom_label(aes(x = 2.5, y = 0.7 * 2.5, label = "AE = 0.3")) +
  geom_label(aes(x = 2.5, y = 0.9 * 2.5, label = "AE = 0.1")) +
  geom_point(aes(x = p1_m, y = p2_m, color = ap_m), size = 3) +
  geom_text_repel(aes(x = p1_m, y = p2_m, label = species), size = 3) +
  scale_color_fish(option = "Trimma_lantana") +
  scale_x_continuous(limits = c(0,2.5)) +
  theme_bw()


pred <- newdata %>% 
  slice(rep(1:n(), each = 4000)) %>%
  mutate(value = c(fitted(model, newdata = newdata, summary = FALSE))) %>%
  mutate(iter = rep(1:4000, 126)) %>%
  pivot_wider(values_from = value, names_from = key) %>%
  mutate(a = 1 - (AE2/AE1)) %>%
  group_by(species, location, sploc) %>%
  summarise_at(vars(c(AE1, AE2, a)), c("mean", "sd", "lb", "ub"))

test <- data %>%
  select(id, c, key, species, location) %>%
   pivot_wider(names_from = key, values_from = c) %>%
  mutate(test = AE2 > AE1)

sum(test$test, na.rm = TRUE)/nrow(test)
test <- data %>%
  select(id, n, key, species, location) %>%
  pivot_wider(names_from = key, values_from = n) %>%
  mutate(test = AE2 > AE1) %>%
  group_by(species, location) %>%
  summarise(freq = sum(test, na.rm = TRUE)/length(test))
sum(test$test, na.rm = TRUE)/nrow(test)

test <- data %>%
  select(id, p, key, species, location) %>%
  pivot_wider(names_from = key, values_from = p) %>%
  mutate(test = AE2 > AE1)%>%
  group_by(species, location) %>%
  summarise(freq = sum(test, na.rm = TRUE)/length(test))
sum(test$test, na.rm = TRUE)/nrow(test)

#########test ############

newdata <- unique(select(data_ae, species, location, sploc, key))

##### C #####

lb <- function(x){
  quantile(x, 0.025)
}
ub <- function(x){
  quantile(x, 0.975)
}
m <- function(x){
  mean(x)
}

pred_c <- newdata %>% 
  slice(rep(1:n(), each = 4000)) %>%
  mutate(value = c(fitted(list[[1]], newdata = newdata, summary = FALSE))) %>%
  mutate(iter = rep(1:4000, 126)) %>%
  pivot_wider(values_from = value, names_from = key) %>%
  mutate(ac = 1 - (AE2/AE1)) %>%
  group_by(species, location, sploc) %>%
  summarise_at(vars(c(AE1, AE2, ac)), c("m", "sd", "lb", "ub")) %>%
  rename(c1_m = AE1_m,
         c1_sd = AE1_sd,
         c1_lb = AE1_lb,
         c1_ub = AE1_ub,
         c2_m = AE2_m,
         c2_sd = AE2_sd,
         c2_lb = AE2_lb,
         c2_ub = AE2_ub
         )
pred_n <- newdata %>% 
  slice(rep(1:n(), each = 4000)) %>%
  mutate(value = c(fitted(list[[1]], newdata = newdata, summary = FALSE))) %>%
  mutate(iter = rep(1:4000, 126)) %>%
  pivot_wider(values_from = value, names_from = key) %>%
  mutate(a = 1 - (AE2/AE1)) %>%
  group_by(species, location, sploc) %>%
  summarise_at(vars(c(AE1, AE2, a)), c("mean", "sd", "lb", "ub"))
pred_p <- newdata %>% 
  slice(rep(1:n(), each = 4000)) %>%
  mutate(value = c(fitted(list[[1]], newdata = newdata, summary = FALSE))) %>%
  mutate(iter = rep(1:4000, 126)) %>%
  pivot_wider(values_from = value, names_from = key) %>%
  mutate(a = 1 - (AE2/AE1)) %>%
  group_by(species, location, sploc) %>%
  summarise_at(vars(c(AE1, AE2, a)), c("mean", "sd", "lb", "ub"))

pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
                               values_from = Estimate, names_prefix = "m_")
pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
                                values_from = Est.Error, names_prefix = "sd_")

pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
                                values_from = Q2.5, names_prefix = "lb_")
pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
                                values_from = Q97.5, names_prefix = "ub_")

pred_c <- left_join(pred_m, pred_sd) %>%
  left_join(pred_lb) %>%
  left_join(pred_ub) 

colnames(pred_c) <-
  c("species", "location", "sploc", "c1_m", "c2_m", "c1_sd", "c2_sd", "c1_lb", "c2_lb", "c1_ub", "c2_ub")

##### N #####
pred <- cbind(newdata, fitted(list[[2]], newdata = newdata) ) 

pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
                               values_from = Estimate, names_prefix = "m_")
pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
                                values_from = Est.Error, names_prefix = "sd_")

pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
                                values_from = Q2.5, names_prefix = "lb_")
pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
                                values_from = Q97.5, names_prefix = "ub_")

pred_n <- left_join(pred_m, pred_sd) %>%
  left_join(pred_lb) %>%
  left_join(pred_ub) 

colnames(pred_n) <-
  c("species", "location", "sploc", "n1_m", "n2_m", "n1_sd", "n2_sd", "n1_lb", "n2_lb", "n1_ub", "n2_ub")

##### P #####
pred <- cbind(newdata, fitted(list[[3]], newdata = newdata) ) 

pred_m <- pred %>% pivot_wider(- c(6,7,8), names_from = key, 
                               values_from = Estimate, names_prefix = "m_")
pred_sd <- pred %>% pivot_wider(- c(5,7,8), names_from = key, 
                                values_from = Est.Error, names_prefix = "sd_")

pred_lb <- pred %>% pivot_wider(- c(5,6,8), names_from = key, 
                                values_from = Q2.5, names_prefix = "lb_")
pred_ub <- pred %>% pivot_wider(- c(5,6,7), names_from = key, 
                                values_from = Q97.5, names_prefix = "ub_")

pred_p <- left_join(pred_m, pred_sd) %>%
  left_join(pred_lb) %>%
  left_join(pred_ub) 

colnames(pred_p) <-
  c("species", "location", "sploc", "p1_m", "p2_m", "p1_sd", "p2_sd", "p1_lb", "p2_lb", "p1_ub", "p2_ub")

left_join(pred_c, pred_n) %>%
  left_join(pred_p)

####################### stan model ##############


ae_mod(model = model, data = data, element = "p" )


zs <- data_ae %>%
  filter(species == "Zebrasoma_scopas", location == "Moorea")

data <- list(
  N1 = nrow(filter(zs, key == "AE1")),
  N2 = nrow(filter(zs, key == "AE2")),
  x1 = (filter(zs, key == "AE1"))$n,
  x2 = (filter(zs, key == "AE2"))$n)

stan2 <- stan_model("stan/ae_normal.stan")

fit <- sampling(stan2, data = data)

summary(fit)



########## diet estimate #########

fit_diet_c <- brm(c_a_m ~ 0 + diet:family, data = data)
fit_diet_n <- brm(n_a_m ~ 0 + diet:family, data = data)
fit_diet_p <- brm(p_a_m ~ 0 + diet:family, data = data)

summary(fit_diet_c)
marginal_effects(fit_diet_c)

summary(fit_diet_n)
marginal_effects(fit_diet_n)

summary(fit_diet_p)
marginal_effects(fit_diet_p)

ggplot(data) +
  geom_boxplot(aes(x = family, y = c_a_m, color = diet))

data <- data %>%
  group_by(diet, family) %>%
  summarize_all(mean)

############ herbivores #############

diets <- read_csv("data/extrapolation_trophic_guilds.csv") %>%
  select(species, diet = trophic_guild_predicted) 

params <- read_csv("data/params_sst_glob.csv") %>%
  filter(v_m %in% c(26, 27, 28, 29)) %>%
  group_by(Family, species, Species) %>%
  summarize_all(mean) %>%
  select(-ac_m, -an_m, -ap_m)


loadd(data_ae)

data <- left_join(data_ae, params) %>%
  filter(key == "AE2") 


ggplot(data) +
  geom_point(aes(x = (sl), y = log(c)))
ggplot(data) +
  geom_point(aes(x = (sl), y = log(n)))
ggplot(data) +
  geom_point(aes(x = (sl), y = log(p)))


