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

############ test #############
library(drake)
library(tidyverse)
loadd(result_ext)



spmass <- data_ae %>%
  group_by(species) %>%
  summarise(biomass = mean(weight, na.rm = T))


data <- result_ext %>%
  left_join(diets) %>%
  left_join(spmass) %>%
  mutate(int_rel = log(int_surface/biomass))  %>%
  mutate(negp = ap_mean<0)


ggplot(data) +
  geom_point(aes(x = (biomass), y = int_surface)) 

ggplot(data) +
  geom_point(aes(color = log(int_surface/biomass), y = ap_mean, x = Dp_mean), size = 3) +
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

library(brms)

fit <- brm(ac_median ~ Dc_median + diet2, backend = "cmdstanr", family = "beta", data = data)
fitn <- brm(an_median ~ Dn_median + diet2, backend = "cmdstanr", family = "beta", data = data)
fitp <- brm(ap_median ~ Dp_median + diet2, backend = "cmdstanr", family = "beta", data = data)
fitp <- brm(ap_median ~ Dp_median, backend = "cmdstanr", family = "beta", data = data)

fitc_se <- brm(ac_mean | resp_se(`ac_sd`, sigma = FALSE) ~ me(Dc_mean, Dc_sd)  + log(int_surface/biomass),
               backend = "cmdstanr", family = "student", data = datac, iter = 4000, cores = 4, threads = 2)
# fitc_se <- brm(log(ac_mean)  ~ log(Dc_mean)  + log(int_surface/biomass),
#                backend = "cmdstanr", family = "student", data = datac, iter = 4000, cores = 4, threads = 2)

fitn_se <- brm(an_mean | resp_se(`an_sd`, sigma = FALSE) ~ me(Dn_mean, Dn_sd) +  log(int_surface/biomass),
               backend = "cmdstanr", family = "student", data = datan, iter = 4000, cores = 4, threads = 2)

# fitn_se <- brm(log(an_mean) ~ log(Dn_mean) +  log(int_surface/biomass),
#                backend = "cmdstanr", family = "student", data = datan, iter = 4000, cores = 4, threads = 2)


fitp_se <- brm(ap_mean | resp_se(`ap_sd`, sigma = FALSE) ~ me(Dp_mean, Dp_sd) + log(int_surface/biomass) ,
               backend = "cmdstanr", family = "student", data = datap, iter = 4000, cores = 4, threads = 2)

summary(fitc_se)
summary(fitn_se)
summary(fitp_se)

predc <- conditional_effects(fitc_se)[[1]]
predn <- conditional_effects(fitn_se)[[1]]
predp <- conditional_effects(fitp_se)[[1]]

nd1 <- expand_grid(Dp_mean = seq(0.05, 2.7, 0.01),
                  int_rel = seq(1.5,5.2,0.01))

fe <- fixef(fitp_se, summary = F)

nd1$estimate <- lapply(1:nrow(fe), function(i){
  df <- data.frame(c = fe[i,1] + nd1$int_rel*fe[i,2] + (nd1$Dp_mean)*fe[i,3])
}) %>% bind_cols() %>% rowMeans()

library(fishualize)

pp <-
ggplot(nd1[nd1$estimate<1&nd1$estimate>0,]) +
  geom_raster(aes(x = Dp_mean, y = int_rel, fill = estimate)) +
  geom_point(aes(x = Dp_mean, y = log(int_surface/biomass), shape = as.character(diet2)), 
             alpha = 0.9, data = data, size = 3) +
  scale_fill_fish(option = "Chaetodon_ephippium", limits = c(0,1), 
                  breaks = c(0,0.2, 0.4, 0.6, 0.8, 1)) +
  # geom_mark_hull(aes( label = diet2, x = Dp_mean, y = log(int_surface/biomass), filter = diet2 == "2_herb"), data = data) +
  # geom_mark_hull(aes( label = diet2, x = Dp_mean, y = log(int_surface/biomass), filter = diet2 == "6_carn"), data = data) +
  # geom_mark_hull(aes( label = diet2, x = Dp_mean, y = log(int_surface/biomass), filter = diet2 == "5_plank"), data = data) +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_shape_discrete(guide = "none") +
  theme(legend.position = 'none') +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                                       barwidth = unit(20, 'lines'), 
                                barheight = unit(.5, 'lines'))) +
  labs(x = "Stomach content P%", y = "log(intestine surface/ biomass)", fill = "Predicted absorption efficiency") +
  theme(text = element_text(size = 14))
pp  

bayes_R2(fitp_se)
# N

ndn <- expand_grid(Dn_mean = seq(0.8, 12, 0.05),
                   int_rel = seq(1.5,5.2,0.05))

fe <- fixef(fitn_se, summary = F)

ndn$estimate <- lapply(1:nrow(fe), function(i){
  df <- data.frame(c = fe[i,1] + ndn$int_rel*fe[i,2] + (ndn$Dn_mean)*fe[i,3])
}) %>% bind_cols() %>% rowMeans()


pn <-
ggplot(ndn[ndn$estimate<1&ndn$estimate>0,]) +
  geom_raster(aes(x = Dn_mean, y = int_rel, fill = estimate)) +
  geom_point(aes(x = Dn_mean, y = log(int_surface/biomass), shape = as.character(diet2)), 
             alpha = 0.7, data = data, size = 3) +
  scale_fill_fish(option = "Chaetodon_ephippium", limits = c(0,1), breaks = c(0, 0.2, 0.4,0.6, 0.8, 1), guide = "none")  +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = 'bottom') +
  labs(x = "Stomach content N%", y = "log(intestine surface/ biomass)", 
       fill = "Predicted absorption efficiency", shape = "") +
  theme(text = element_text(size = 14))
pn
####c


ndc <- expand_grid(Dc_mean = seq(5, 50, 0.5),
                   int_rel = seq(1.5,5.2,0.01))

fe <- fixef(fitc_se, summary = F)

ndc$estimate <- lapply(1:nrow(fe), function(i){
  df <- data.frame(c = fe[i,1] + ndc$int_rel*fe[i,2] + (ndc$Dc_mean)*fe[i,3])
}) %>% bind_cols() %>% rowMeans()

pc <-
ggplot(ndc[ndc$estimate<1,]) +
  geom_raster(aes(x = Dc_mean, y = int_rel, fill = estimate)) +
  geom_point(aes(x = Dc_mean, y = log(int_surface/biomass), shape = as.character(diet2)), 
             alpha = 0.8, data = data, size = 3) +
  scale_fill_fish(option = "Chaetodon_ephippium", limits = c(0,1), 
                  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))  +
  theme_classic() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_shape_discrete(guide = "none") +
  theme(legend.position = 'bottom') +
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(15, 'lines'), 
                               barheight = unit(.7, 'lines'))) +
  labs(x = "Stomach content C%", y = "log(intestine surface/ biomass)", fill = "Predicted absorption efficiency") +
  theme(text = element_text(size = 14))
pc

bayes_R2(fitn_se)
bayes_R2(fitc_se)
bayes_R2(fitp_se)

library(patchwork)
plot <-
pc + pn + pp + plot_annotation(tag_levels = "A")

plot

ggsave("test.pdf", plot, width = 12, height = 5)

pred <- fitted(fitp_se) %>%
  cbind(fitp_se$data)

ggplot(pred) +
  geom_point(aes(x = Dp_mean, color = Estimate, y = log(int_surface/biomass)))

ggplot(pred) +
  geom_ribbon(aes(x = (Dp_mean), ymin = (Q2.5), ymax = (Q97.5)), alpha = 0.5) +
  geom_pointrange(aes(x = Dp_mean, y = ap_mean, ymin = ap_mean - ap_sd, ymax = ap_mean + ap_sd)) +
  geom_point(aes(x = (Dp_mean), y = (ap_mean), color = log(int_surface/biomass))) +
   ylim(-2,2)

ggplot(pred) +
  geom_point(aes(x = Dp_mean, y = Estimate, color = log(int_surface/biomass)), size = 3) +
  geom_abline(slope = 1) +
  geom_pointrange(aes(x = Dp_mean, y = Estimate, ymin = , color = log(int_surface/biomass)), size = 3) +
  ylim(-2,2)



predc <- fitted(fitc_se, newdata = data) 
predn <- fitted(fitn_se, newdata = data) 
predp <- fitted(fitp_se, newdata = data) 

data <- data %>%
  mutate(ac_mean_pred = predc[,1],
         ac_sd_pred = predc[,1],
         an_mean_pred = predn[,1],
         an_sd_pred = predn[,1],
         ap_mean_pred = predp[,1],
         ap_sd_pred = predp[,1])



ggplot(data) +
  geom_point(aes(x = ap_lqr, y = ap_mean_pred, color = log(int_surface/biomass))) +
  geom_abline(slope = 1)

ggplot(data) +
  geom_point(aes(x = an_mean, y = an_mean_pred, color = log(int_surface/biomass))) +
  geom_abline(slope = 1)

write_csv(data, "output/data/result_ae_predict.csv")

ggplot() +
  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), data = predc, alpha = 0.5) +
  geom_line(aes(x = effect1__, y = estimate__), data = predc) +
  geom_pointrange(aes(x = Dc_mean, y = ac_mean, ymin = ac_lqn, ymax = ac_uqn, color = family), data = data)

ggplot() +
  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), data = predn, alpha = 0.5) +
  geom_line(aes(x = effect1__, y = estimate__), data = predn) +
  #geom_point(aes(x = Dn_mean, y = an_mean), data = data) +
  geom_pointrange(aes(x = Dn_mean, y = an_mean, ymin = an_lqn, ymax = an_uqn), data = datan)

ggplot() +
  geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), data = predp, alpha = 0.5) +
  geom_line(aes(x = effect1__, y = estimate__), data = predp) +
  geom_pointrange(aes(x = Dp_mean, y = ap_mean, ymin = ap_lqn, ymax = ap_uqn, color = family), data = datap)



test <- cbind(data, fitted(fitp_se, newdata = data))
test <- cbind(data, fitted(fitn_se, newdata = data))
test <- cbind(data, fitted(fitc_se, newdata = data))

ggplot(aes(ap_mean, Estimate, color = family), data = test) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

