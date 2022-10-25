

diets <- read_csv("data/extrapolation_trophic_guilds.csv") %>%
  select(species, diet = trophic_guild_predicted) 

result_ae <- read_csv("output/data/summary_results_aecnp.csv")


loadd(result_ae)

result <- result_ae %>% 
  #filter(!species == "Chaetodon quadrimaculatus") %>%
  left_join(diets) #%>%
  filter(n_ae2>5)

library(patchwork)
ggplot(result) +
  geom_point(aes(x = c_mu1_m, y = c_a_m)) +
ggplot(result) +
  geom_point(aes(x = c_a_m, y = n_a_m)) +
ggplot(result) +
  geom_point(aes(x = n_a_m, y = p_a_m)) +
  
plot_layout(nrow = 3)

# result <- filter(result, 
#                  c1_sd < 0.5 * c1_m,
#                  p1_sd < 0.5 * p1_m, n1_sd < 0.5 * n1_m,
#                  c2_sd < 0.5 * c2_m,
#                  p2_sd < 0.5 * p2_m, n2_sd < 0.5 * n2_m) 

library(ggrepel)

ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 50, y = 0.3 * 50, label = "AE = 0.7")) +
  geom_label(aes(x = 50, y = 0.5 * 50, label = "AE = 0.5")) +
  geom_label(aes(x = 50, y = 0.7 * 50, label = "AE = 0.3")) +
  geom_label(aes(x = 50, y = 0.9 * 50, label = "AE = 0.1")) +
  geom_point(aes(x = c_mu1_m, y = c_mu2_m, color =  as.character(diet), shape = location),
             size = 2) +
  geom_text_repel(aes(x = c_mu1_m, y = c_mu2_m, label = species, color = as.character(diet)), 
                  size = 3, alpha = 0.7) +
  theme_bw() +
  labs(x = "Gut content C%", y = "Feces C%", color = "Trophic guild", shape = "Location")

ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 13, y = 0.3 * 13, label = "AE = 0.7")) +
  geom_label(aes(x = 13, y = 0.5 * 13, label = "AE = 0.5")) +
  geom_label(aes(x = 13, y = 0.7 * 13, label = "AE = 0.3")) +
  geom_label(aes(x = 13, y = 0.9 * 13, label = "AE = 0.1")) +
  geom_point(aes(x = n_mu1_m, y = n_mu2_m, color = as.character(diet), shape = location),
             size = 2) +
  geom_text_repel(aes(x = n_mu1_m, y = n_mu2_m, label = species, color = as.character(diet)), 
                  size = 3, alpha = 0.7) +
  theme_bw() +
  labs(x = "Gut content N%", y = "Feces N%", color = "Trophic guild", shape = "Location")

  

ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 2.5, y = 0.3 * 2.5, label = "AE = 0.7")) +
  geom_label(aes(x = 2.5, y = 0.5 * 2.5, label = "AE = 0.5")) +
  geom_label(aes(x = 2.5, y = 0.7 * 2.5, label = "AE = 0.3")) +
  geom_label(aes(x = 2.5, y = 0.9 * 2.5, label = "AE = 0.1")) +
  geom_point(aes(x = p_mu1_m, y = p_mu2_m, color = as.character(diet), shape = location),
             size = 2) +
  geom_text_repel(aes(x = p_mu1_m, y = p_mu2_m, label = species, color = as.character(diet)), 
                  size = 3, alpha = 0.7) +
  theme_bw() +
  labs(x = "Gut content P%", y = "Feces P%", color = "Trophic guild", shape = "Location")

## with errorbars
ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 50, y = 0.3 * 50, label = "AE = 0.7")) +
  geom_label(aes(x = 50, y = 0.5 * 50, label = "AE = 0.5")) +
  geom_label(aes(x = 50, y = 0.7 * 50, label = "AE = 0.3")) +
  geom_label(aes(x = 50, y = 0.9 * 50, label = "AE = 0.1")) +
  geom_point(aes(x = c_mu1_m, y = c_mu2_m, shape = location),
             size = 2) +
  geom_errorbar(aes(x = c_mu1_m, ymin = c_mu2_lb, ymax = c_mu2_ub), 
                size = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(y = c_mu2_m, xmin = c_mu1_lb, xmax = c_mu1_ub), 
                 size = 0.5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Gut content C%", y = "Feces C%", color = "Trophic guild", shape = "Location")

ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 13, y = 0.3 * 13, label = "AE = 0.7")) +
  geom_label(aes(x = 13, y = 0.5 * 13, label = "AE = 0.5")) +
  geom_label(aes(x = 13, y = 0.7 * 13, label = "AE = 0.3")) +
  geom_label(aes(x = 13, y = 0.9 * 13, label = "AE = 0.1")) +
  geom_point(aes(x = n_mu1_m, y = n_mu2_m, color = as.character(diet), shape = location),
             size = 2) +
  geom_errorbar(aes(x = n_mu1_m, ymin = n_mu2_25, ymax = n_mu2_75, color = as.character(diet)), 
                size = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(y = n_mu2_m, xmin = n_mu1_25, xmax = n_mu1_75, color = as.character(diet)), 
                 size = 0.5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Gut content N%", y = "Feces N%", color = "Trophic guild", shape = "Location")



ggplot(result) +
  geom_abline(slope = 0.3) +
  geom_abline(slope = 0.5) +
  geom_abline(slope = 0.7) +
  geom_abline(slope = 0.9) +
  geom_label(aes(x = 2.5, y = 0.3 * 2.5, label = "AE = 0.7")) +
  geom_label(aes(x = 2.5, y = 0.5 * 2.5, label = "AE = 0.5")) +
  geom_label(aes(x = 2.5, y = 0.7 * 2.5, label = "AE = 0.3")) +
  geom_label(aes(x = 2.5, y = 0.9 * 2.5, label = "AE = 0.1")) +
  geom_point(aes(x = p_mu1_m, y = p_mu2_m, color = as.character(diet), shape = location),
             size = 2) +
  geom_errorbar(aes(x = p_mu1_m, ymin = p_mu2_25, ymax = p_mu2_75, color = as.character(diet)), 
                  size = 0.5, alpha = 0.5) +
  geom_errorbarh(aes(y = p_mu2_m, xmin = p_mu1_25, xmax = p_mu1_75, color = as.character(diet)), 
                size = 0.5, alpha = 0.5) +
  theme_bw() +
  labs(x = "Gut content P%", y = "Feces P%", color = "Trophic guild", shape = "Location")


######
## location comparison


subm <- result %>%
  group_by(species) %>%
  mutate(n = n()) %>%
  filter(n > 1)

p1 <- 
  ggplot(subm) +
  geom_pointrangeh(aes(x = p_mu1_m, xmin = p_mu1_lb, xmax = p_mu1_ub, y = species, color = location), 
                   position=position_dodgev(0.5), alpha = 0.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(), legend.position = "bottom") +
  labs( x = "P (%)", y = "", title = "AE1")# +
p1
p2 <- 
  ggplot(subm) +
  geom_pointrangeh(aes(x = p_mu2_m, xmin = p_mu2_lb, xmax = p_mu2_ub, y = species, color = location), 
                   position=position_dodgev(0.5), alpha = 0.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(), legend.position = "bottom") +
  labs( x = "P (%)", y = "", title = "AE2")# +
p2

p3 <- 
  ggplot(subm) +
  geom_pointrangeh(aes(x = p_a_m, xmin = p_a_lb, xmax = p_a_ub, y = species, color = location), 
                   position = position_dodgev(0.5), alpha = 0.5) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(), legend.position = "bottom") +
  labs( x = "AE", y = "", title = "AE")# +
p3

############# size ############

loadd(data_ae)

sizes <- data_ae %>%
  group_by(species, location) %>%
  summarise(size = median(sl))

############ intestines ########
int <- readr::read_csv("data/intestine_dataset.csv")

tl <- lapply(gsub("_", " ",unique(result_ae$species)),
             fishflux::trophic_level) %>%
  plyr::ldply()

tl <- tl %>% mutate(species = gsub(" ", "_", species))
tl <- left_join(tl, result_ae) %>% left_join(params)

ggplot(tl) +
  geom_point(aes(x = trophic_level, y = c_a_m, color = c_mu1_m)) +
  geom_smooth(aes(x = trophic_level, y = c_a_m), method = "lm") +
  theme_bw()

ggplot(tl) +
  geom_point(aes(x = trophic_level, y = n_a_m)) +
  geom_smooth(aes(x = trophic_level, y = n_a_m), method = "lm") +
  theme_bw()
ggplot(tl) +
  geom_point(aes(x = trophic_level, y = p_a_m)) +
  geom_smooth(aes(x = trophic_level, y = p_a_m), method = "lm") +
  theme_bw()

ggplot(tl) +
  geom_point(aes(x = trophic_level, y = c_mu2_m, color = c_mu1_m)) +
  geom_smooth(aes(x = trophic_level, y = c_mu2_m), method = "lm") +
  theme_bw()
ggplot(tl) +
  geom_point(aes(x = trophic_level, y = n_mu2_m)) +
  geom_smooth(aes(x = trophic_level, y = n_mu2_m), method = "lm") +
  theme_bw()
ggplot(tl) +
  geom_point(aes(x = trophic_level, y = p_mu2_m)) +
  geom_smooth(aes(x = trophic_level, y = p_mu2_m), method = "lm") +
  theme_bw()

fit <- lm(c_a_m ~ trophic_level, data = tl)
summary(fit)
fit <- lm(n_a_m ~ trophic_level , data = tl)
summary(fit)
fit <- lm(p_a_m ~ trophic_level , data = tl)
summary(fit)

loadd(result_ae)


head(tl)

instead of one drawing, we have two drawings showing contrasting communities: a “pristine” community and an impacted community with low complexity and a lot of fishing (lots of small herbivores, like Moorea).
A more extensive analysis showing the effect of different community compositions on egestion rates as well as poo quality We can use the RLS for this. I contacted Rick and Graham and got all the available community data for French Polynesia.
Another option may be to look at time series of community-level egestion rates

int <- readr::read_csv("data/intestine_dataset.csv")
ints <- int %>%
  filter(location == "Moorea") %>%
  dplyr::group_by(family, species, location) %>%
  summarize(int_surface = mean(intestine_surface),
            sl = mean(sl)) %>% 
  mutate(species = gsub(" ", "_", species)) %>%
  inner_join(result_ae) 


ggplot(ints) +
  geom_point(aes(x = log(sl), y = log(int_surface)))

ggplot(ints) +
  geom_point(aes(y = c_mu1_m, x = int_surface_st))
ggplot(ints) +
  geom_point(aes(y = n_mu1_m, x = int_surface_st))
ggplot(ints) +
  geom_point(aes(color = log(p_mu1_m), x = int_surface_st, y = p_a_m)) 

ggplot(ints) +
  geom_point(aes(x = int_surface_st, y = c_a_m, color = c_mu1_m))

ggplot(ints) +
  geom_point(aes(x = int_surface_st, y = n_a_m, color = n_mu1_m))

ggplot(ints) +
  geom_point(aes(x = int_surface_st, y = p_a_m, color = log(p_mu1_m)))

fit <- lm(c_a_m ~ c_mu1_m + int_surface_st, data = ints)
summary(fit)

fit <- lm(c_mu1_m ~  int_surface_st, data = ints)
summary(fit)
fit <- lm(n_mu1_m ~  int_surface_st, data = ints)
summary(fit)
fit <- lm(p_mu1_m ~  int_surface_st, data = ints)
summary(fit)

fit <- lm(n_a_m ~ n_mu1_m + int_length_st, data = ints)
summary(fit)
fit <- lm(p_a_m ~ p_mu1_m + int_length_st, data = ints)
summary(fit)

fit <- brm(ap_m ~ p1_m + log(int_surface_st) + (1|family), data = ints, family = "beta")
summary(fit)

ranef(fit)

######## coprophagy ######
cop <- read_csv("data/coprophagy_robertson1982.csv") %>%
  mutate(species = gsub(" ", "_", species))

cop <- left_join(cop, diets)

ggplot(cop) +
  geom_boxplot(aes(x = as.character(diet), y = prop_eaten)) +
  geom_jitter(aes(x = as.character(diet), y = prop_eaten), width = 0.2)

cop <- left_join(cop, result_ae)

ggplot(cop) +
  geom_point(aes(x = c_mu2_m, y = prop_eaten))
ggplot(cop) +
  geom_point(aes(x = n_mu2_m, y = prop_eaten))
ggplot(cop) +
  geom_point(aes(x = p_mu2_m, y = prop_eaten))

ggplot(cop) +
  geom_boxplot(aes(x = as.character(coprophage), y = c_mu1_m))
ggplot(cop) +
  geom_boxplot(aes(x = as.character(coprophage), y = n_mu1_m))
ggplot(cop) +
  geom_boxplot(aes(x = as.character(coprophage), y = p_mu1_m))

fit_copr <- brm(coprophage ~ c_mu1_m + n_mu1_m + p_mu1_m,
                data = cop, family = "bernoulli")
fit_copr2 <- update(fit_copr, formula = coprophage ~ c_mu1_m)
fit_copr3 <- update(fit_copr, formula = coprophage ~ n_mu1_m)
fit_copr4 <- update(fit_copr, formula = coprophage ~ p_mu1_m)
fit_copr5 <- update(fit_copr, formula = coprophage ~ c_mu1_m + p_mu1_m)
fit_copr6 <- update(fit_copr, formula = coprophage ~ n_mu1_m + p_mu1_m)

loo(fit_copr, fit_copr2, fit_copr3, fit_copr4, fit_copr5, fit_copr6)

fixef(fit_copr)
conditional_effects(fit_copr4)

#####

cop2 <- drop_na(cop, c_mu1_m) %>%
  mutate(one = round(prop_eaten * n_feces)) %>%
  mutate(zero = n_feces - one) %>%
  pivot_longer(names_to = "key", values_to = "n", c(one, zero)) %>%
  uncount(n)

fit_cop <- brm(key ~ c_mu2_m + n_mu2_m + p_mu2_m, 
               data = cop2, family = "bernoulli")

fit_cop2 <- update(fit_cop, formula = key ~ p_mu2_m)
fit_cop3 <- update(fit_cop, formula = key ~ n_mu2_m)
fit_cop4 <- update(fit_cop, formula = key ~ c_mu2_m)
fit_cop5 <- update(fit_cop, formula = key ~ c_mu2_m + p_mu2_m)
fit_cop6 <- update(fit_cop, formula = key ~ n_mu2_m + p_mu2_m)
fit_cop7 <- update(fit_cop, formula = key ~ c_mu2_m + n_mu2_m)

loo(fit_cop, fit_cop2, fit_cop3, fit_cop4, fit_cop5, fit_cop6, fit_cop7)


p <- fitted(fit_cop2) 
colnames(p) <- c("estimate__", "se", "lower__", "upper__")
p <- cbind(fit_cop2$data, p)
a <-
ggplot() +
  geom_ribbon(aes(x = p_mu2_m, ymin = 1 - lower__, ymax = 1 - upper__), 
              data = p, alpha = 0.3) +
  geom_line(aes(x = p_mu2_m, y = 1 - estimate__), 
            data = p, size = 2) +
  geom_point(aes(x = p_mu2_m, y = prop_eaten), data = cop) +
  theme_bw() +
  labs(x = "%P in feces", y = "Proportion feces being eaten")

p <- conditional_effects(fit_copr4)[[1]]

b <-
ggplot() +
  geom_ribbon(aes(x = p_mu1_m, ymin = lower__, ymax = upper__), 
              data = p, alpha = 0.3) +
  geom_line(aes(x = p_mu1_m, y = estimate__), 
            data = p, size = 2) +
  geom_point(aes(x = p_mu2_m, y = coprophage), data = cop) +
  theme_bw() +
  labs(x = "%P in gut", y = "Probability being coprophage")
library(patchwork)
a + b + plot_layout(nrow = 2)
ggsave("output/plots/copr_P.png")


##### SIA #####
sia <- read.csv("data/moorea.sia.2019.csv", sep = ";") %>%
  select(-1) %>%
  select(species = Names, N = X.N, C = X.C, DN, DC, weight = Weight) %>%
  mutate(species = gsub("  ", " ", species)) %>%
  mutate(species = gsub(" ", "_", species)) %>%
  group_by(species) %>%
  mutate(n = n()) %>%
  filter(n > 5) 

fit <- brms::brm(DN ~ (0 + species), data = sia)
fit2 <- update(fit, DN ~ 0 + species)

summary(fit2)

pred <- fitted(fit2, newdata = data.frame(species = unique(fit2$data$species)))

data.frame(species = unique(fit2$data$species),
           dn = pred[,1],
           sn_sd = pred[,2])

ggplot(aes(x = species, y = DN), data = sia) +
  geom_boxplot()


sia_species <- sia %>%
  group_by(species) %>%
  summarize_all(median, na.rm = TRUE) %>%
  inner_join(result)

shapiro.test(sia_species$DN)
shapiro.test(sia_species$DC)
shapiro.test(sia_species$n_mu1_m)
shapiro.test(log(sia_species$p_mu1_m))
œœshapiro.test(sia_species$c_mu1_m)
shapiro.test(sia_species$p_a_m)
shapiro.test(sia_species$n_a_m)
hist(sia_species$c_a_m)

ggplot(aes(x = DC, y = DN, color = n_a_m), data = sia_species) +
  geom_point() +
  geom_text_repel(aes(label = species), 
                  data = sia_species, size = 3) +
  scale_color_fish()
  
ggplot(aes(y = p_mu1_m, x = DN), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1, method = "lm") +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(y = p_mu2_m, x = DN), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1, method = "lm") +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3)  +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(x = DN, y = p_a_m), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1) +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3)  +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(x = DN, y = n_a_m), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1) +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3)  +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(x = DN, y = c_a_m), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1) +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3)  +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(x = DN, y = c_a_m), data = sia_species) +
  geom_point(aes(color = c_mu1_m)) +
  geom_smooth(se = FALSE, color = "grey", size = 1) +
  geom_text_repel(aes(label = species, color = c_mu1_m), 
                  data = sia_species, size = 3)  +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log")

ggplot(aes(x = DN, y = p_mu2_m), data = sia_species) +
  geom_point(aes(color = as.character(diet))) +
  geom_smooth(se = FALSE, color = "grey", size = 1) +
  geom_text_repel(aes(label = species, color = as.character(diet)), 
                  data = sia_species, size = 3) 

ggplot(aes(x = as.numeric(weight), y = DN), data = sia) +
  geom_point() +
  geom_smooth(aes(group = species), method = "lm", se = FALSE) 

require(FactoMineR)
require(factoextra)
require(ggplot2)

sia_species <- sia %>%
  group_by(species) %>%
  summarize_all(median, na.rm = TRUE) %>%
  inner_join(result) %>%
  drop_na(DN) %>%
  filter(location == "Moorea") %>%
  mutate(diet = as.character(diet)) %>%
  mutate(cn_mu1 = c_mu1_m/n_mu1_m,
         cp_mu1 = c_mu1_m/p_mu1_m) 
#%>%
#  mutate_if(is.numeric, scale) 
  


pca1 <- PCA(result_ext[, c( "Dn_mean",  "ac_mean",
                            "an_mean", "ap_mean")])

plot(pca1)

ind <- pca1$ind$coord %>%
  as.data.frame() %>%
  cbind(result_ext)

var <- pca1$var$coord %>%
  as.data.frame() 

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ <- circleFun(c(0,0),2,npoints = 500)

library(ggrepel)
ggplot(ind, aes(x = Dim.1, y = Dim.2, label = species, color = as.character(diet2))) +
  geom_point() +
  geom_text_repel()



vars.p <-  ggplot() +
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_segment(data = var, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               lwd = 1) + 
  geom_text(data = var, 
            aes(x = Dim.1*1.15, y =  Dim.2*1.15, 
                label = c( "Dn_mean", "ac_mean",
                           "an_mean", "ap_mean")), 
            check_overlap = F, size = 3) +
  xlab("PC 1") + 
  ylab("PC2") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_point(data = ind, aes(x = Dim.1, y = Dim.2, label = species, color = as.character(diet2))) +
  geom_text_repel(data = ind, aes(x = Dim.1, y = Dim.2, label = species, color = as.character(diet2)))

vars.p

ggplot() +
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_segment(data = var, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               lwd = 1) + 
  geom_text(data = var, 
            aes(x = Dim.1*1.15, y =  Dim.2*1.15, 
                label = c("DN", "DC", "n_mu1_m", "p_mu1_m",
                          "n_a_m", "p_a_m")), 
            check_overlap = F, size = 3) +
  xlab("PC 1") + 
  ylab("PC2") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_point(data = ind, aes(x = Dim.1, y = Dim.2, label = species, color = c_mu2_m), size = 3) +
  geom_text_repel(data = ind, aes(x = Dim.1, y = Dim.2, label = species)) +
  scale_color_fish()

ggplot() +
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_segment(data = var, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               lwd = 1) + 
  geom_text(data = var, 
            aes(x = Dim.1*1.15, y =  Dim.2*1.15, 
                label = c("DN", "DC", "n_mu1_m", "p_mu1_m",
                          "n_a_m", "p_a_m")), 
            check_overlap = F, size = 3) +
  xlab("PC 1") + 
  ylab("PC2") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_point(data = ind, aes(x = Dim.1, y = Dim.2, label = species, color = n_mu2_m), size = 3) +
  geom_text_repel(data = ind, aes(x = Dim.1, y = Dim.2, label = species)) +
  scale_color_fish()

ggplot() +
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_segment(data = var, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               lwd = 1) + 
  geom_text(data = var, 
            aes(x = Dim.1*1.15, y =  Dim.2*1.15, 
                label = c("DN", "DC", "n_mu1_m", "p_mu1_m",
                          "n_a_m", "p_a_m")), 
            check_overlap = F, size = 3) +
  xlab("PC 1") + 
  ylab("PC2") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_point(data = ind, aes(x = Dim.1, y = Dim.2, label = species, color = p_mu2_m), size = 3) +
  geom_text_repel(data = ind, aes(x = Dim.1, y = Dim.2, label = species)) +
  scale_color_fish()
ggplot(result) +
  geom_point(aes(x = c_mu1_m, y = n_mu1_m))
ggplot(result) +
  geom_point(aes(x = c_mu1_m, y = p_mu1_m))

ggplot(result_ext) +
  geom_point(aes(x = Dn_mean, y = an_mean, color = diet2)) +
  geom_smooth(aes(x = Dn_mean, y = an_mean, color = diet2, fill = diet2), 
              method = "lm", alpha = 0.2) +
  geom_smooth(aes(x = Dn_mean, y = an_mean), 
              method = "lm", alpha = 0.2) +
  ylim(c(0,1))

ggplot(result_ext) +
  geom_point(aes(x = Dc_mean, y = ac_mean, color = diet2)) +
  geom_smooth(aes(x = Dc_mean, y = ac_mean, color = diet2, fill = diet2), 
              method = "lm", alpha = 0.2) +
  ylim(c(0,1))


ggplot(result_ext) +
  geom_point(aes(x = Dp_mean, y = ap_mean, color = diet2)) +
  geom_smooth(aes(x = Dp_mean, y = ap_mean), 
              method = "lm", alpha = 0.2) +
  ylim(c(0,1))

data <- dplyr::filter(result_ext, ac_mean > 0, an_mean>0, ap_mean>0)

fitn <- brm(an_mean ~ Dn_mean + (1|diet2), data = data)
fitn2 <- brm(an_mean ~ 0 + diet2, data = result_ext)

summary(fitn)
ranef(fitn)

preds <- fitted(fitn)
preds <- cbind(data, preds)
ggplot(preds) +
  geom_point(aes(x = Dn_mean, y = Estimate, color = diet2))
