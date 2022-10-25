library(drake)
library(tidyverse)
library(brms)
library(patchwork)
library(fishualize)
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


fitc_se <- brm(ac_mean | resp_se(`ac_sd`, sigma = FALSE) ~ me(Dc_mean, Dc_sd)  + log(int_surface/biomass),
               backend = "cmdstanr", family = "student", data = datac, iter = 4000, cores = 4, threads = 2)

fitn_se <- brm(an_mean | resp_se(`an_sd`, sigma = FALSE) ~ me(Dn_mean, Dn_sd) +  log(int_surface/biomass),
               backend = "cmdstanr", family = "student", data = datan, iter = 4000, cores = 4, threads = 2)

fitp_se <- brm(ap_mean | resp_se(`ap_sd`, sigma = FALSE) ~ me(Dp_mean, Dp_sd) + log(int_surface/biomass) ,
               backend = "cmdstanr", family = "student", data = datap, iter = 4000, cores = 4, threads = 2)

data_long1 <- data %>%
  select(species, ac_mean, an_mean, ap_mean) %>%
  pivot_longer(-species, names_to = "k_mean", values_to = "a_mean")
data_long2 <- data %>%
  select(species, ac_sd, an_sd, ap_sd) %>%
  pivot_longer(-species, names_to = "k_sd", values_to = "a_sd")
data_long <- cbind(data_long1, data_long2) %>%
  select(-4)

data_long1 <- data %>%
  select(species, Dc_mean, Dn_mean, Dp_mean) %>%
  pivot_longer(-species, names_to = "kd_mean", values_to = "d_mean")
data_long2 <- data %>%
  select(species, Dc_sd, Dn_sd, Dp_sd) %>%
  pivot_longer(-species, names_to = "kd_sd", values_to = "d_sd")

data_long_D <- cbind(data_long1, data_long2) %>%
  select(-4, -1) %>%
  cbind(data_long) %>%
  dplyr::mutate(element = rep(c("c","n", "p"), 51)) %>%
  select(species, element, a_mean, a_sd, d_mean, d_sd) %>%
  left_join(select(data, species, int_surface, biomass))

fitcnp_se <- brm(mean | resp_se(`sd`, sigma = FALSE) ~ 0+ k_mean + (1|species),
               backend = "cmdstanr", family = "student", data = data_long, iter = 4000, cores = 4, threads = 2)

bprior <- c(prior_string("normal(0,1)", class = "b"))
# fitcnp_se2 <- brm(a_mean | resp_se(`a_sd`, sigma = FALSE) ~ 0 +  element +
#                     element:me(d_mean, d_sd)  + element:log(int_surface/biomass),
#                  backend = "cmdstanr", family = "student", data = data_long_D, iter = 4000, 
#                  cores = 4, threads = 2,prior = bprior )


summary(fitc_se)
summary(fitn_se)
summary(fitp_se)

summary(fitcnp_se)
summary(fitcnp_se2)

conditional_effects(fitc_se)
conditional_effects(fitn_se)
conditional_effects(fitp_se)

conditional_effects(fitcnp_se2)
ranef(fitcnp_se)

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

pp_check(fitn_se)
pp_check(fitc_se)
pp_check(fitp_se)

fixef(fitn_se)
fixef(fitc_se)
fixef(fitp_se)

###### average AE's ####

nd <- data.frame(Dc_mean = median(data$Dc_mean),
                 Dn_mean = median(data$Dn_mean),
                 Dp_mean = median(data$Dp_mean),
                 int_rel = median(data$int_rel))

fec <- fixef(fitc_se, summary = F)
vc <- fe[,1] + nd$int_rel*fe[,2] + (nd$Dc_mean)*fe[,3]
mean(vc)
quantile(vc, c(0.025, 0.975))

fen <- fixef(fitn_se, summary = F)
vn <- fen[,1] + nd$int_rel*fen[,2] + (nd$Dn_mean)*fen[,3]
mean(vn)
quantile(vn, c(0.025, 0.975))

fep <- fixef(fitp_se, summary = F)
vp <- fep[,1] + nd$int_rel*fep[,2] + (nd$Dp_mean)*fep[,3]
mean(vp)
quantile(vp, c(0.025, 0.975))


dataci <- datac %>%
  summarise(Dc_mean = mean(Dc_mean), )

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

