drake::loadd(spflux)

theme_ppt <- function(){
  theme_classic() +
    theme(rect = element_rect(fill = cols()[1], color = cols()[1]), 
          panel.background = element_rect(fill = cols()[1], color = cols()[1]), 
          plot.background = element_rect(fill = cols()[1], color = cols()[1]), 
          
          text = element_text(color = cols()[3], size = 14),
          axis.text = element_text(color = cols()[3], size = 12),
          axis.ticks = element_line(color = cols()[3]), 
          axis.line = element_line(colour = cols()[3], size = 1) 
    ) 
}

cols <- function(){
  c("#00183aff","#c2c2c2ff","#e6e6e6ff","#6ac2e5ff",
    "#85ffbcff","#fffc5cff","#f6ab13ff","#df3416ff")
}

save_plot<- function(plot, name, width = 27, height = 12){
  ggsave(filename = paste0("ppt/plots/", name, ".png"), plot = plot, 
         width = width, height = height,
         units = "cm")
}


spsub <- spflux[[1]] %>%
  filter(species %in% c("Cephalopholis_argus", "Acanthurus_nigricans", 
                        "Chromis_xanthura", "Chaetodon_ornatissimus", "Chlorurus_spilurus")) %>%
  dplyr::mutate(check = (round(size) - round(size_median))) %>%
  filter(check == 0)

sum$species
com1 <- sum %>%
  mutate(abundance = c(3, 4, 3, 3, 5)) %>%
  uncount(abundance) %>%
  summarise(Wnp = mean(Wn/Wp),
            Wcn = mean(Wc/Wn),
            Wcp = median(Wc/Wp),
            Wc = 1000*sum(Wc)/sum(biomass),
            Wn = 1000*sum(Wn)/sum(biomass),
            Wp = 1000*sum(Wp)/sum(biomass),
            Fn = 1000*sum(Fn)/sum(biomass),
            Fp = 1000*sum(Fp)/sum(biomass)
  ) %>%
  mutate(RRn = log(Wn/Fn),
         RRp = log(Wp/Fp)) %>%
  mutate(com = "A")

com2 <- sum %>%
  mutate(abundance = c(7, 1, 1, 7, 2)) %>%
  uncount(abundance) %>%
  summarise(Wnp = mean(Wn/Wp),
            Wcn = mean(Wc/Wn),
            Wcp = median(Wc/Wp),
            Wc = 1000*sum(Wc)/sum(biomass),
            Wn = 1000*sum(Wn)/sum(biomass),
            Wp = 1000*sum(Wp)/sum(biomass),
            Fn = 1000*sum(Fn)/sum(biomass),
            Fp = 1000*sum(Fp)/sum(biomass)
  ) %>%
  mutate(RRn = log(Wn/Fn),
         RRp = log(Wp/Fp)) %>%
  mutate(com = "B")

com <- rbind(com1, com2)


vars <- colnames(com)[1:10]
vnames <- data.frame(vars = vars) %>%
  mutate(names = c("N:P ratio egestion",
                   "C:N ratio egestion",
                   "C:P ratio egestion",
                   "C egestion (gC/day)",
                   "N egestion (gN/day)",
                   "P egestion (gP/day)",
                   "N excretion (gN/day)",
                   "P excretion (gP/day)",
                   "Release ratio N",
                   "Release ratio P"))

for (v in vars){
  print(v)
  plot <- 
    ggplot(com) +
    geom_col(aes(x = com, y = .data[[v]], fill = com)) +
    theme_ppt() +
    labs(x = "", y = vnames[vnames$vars == v, "names"]) +
    scale_fill_manual(values = cols()[c(3,3)]) +
    theme(legend.position = ",one")
  save_plot(plot = plot, name = paste("com", v, sep = "_"), width = 8, height = 8)
  
}


long <- com %>%
  select(com, Fn, Wn) %>%
  gather("N_type", "N_value", c(Fn, Wn))

plot <- 
  ggplot(long) +
  geom_col(aes(x = com, y = N_value, fill = com, alpha = N_type)) +
  theme_ppt() +
  labs(x = "", y = "N release (gN/day)") +
  scale_fill_manual(values = cols()[c(2,2)]) +
  scale_alpha_discrete(range = c(0.7, 1)) +
  scale_y_continuous(limits = c(0, 1.7))+
  theme(legend.position = ",one")
plot
save_plot(plot = plot, name = "com_Nout", width = 8, height = 8)

long <- com %>%
  select(com, Fp, Wp) %>%
  gather("P_type", "P_value", c(Fp, Wp))

plot <- 
  ggplot(long) +
  geom_col(aes(x = com, y = P_value, fill = com, alpha = P_type)) +
  theme_ppt() +
  labs(x = "", y = "P release (gN/day)") +
  scale_fill_manual(values = cols()[c(2,2)]) +
  scale_alpha_discrete(range = c(0.7, 1)) +
  scale_y_continuous(limits = c(0, 0.2))+
  theme(legend.position = ",one")
plot
save_plot(plot = plot, name = "com_Pout", width = 8, height = 8)

v <- "Wn"
plot <- 
  ggplot(com) +
  geom_col(aes(x = com, y = .data[[v]], fill = com)) +
  theme_ppt() +
  labs(x = "", y = vnames[vnames$vars == v, "names"]) +
  scale_fill_manual(values = cols()[c(2,2)]) +
  scale_y_continuous(limits = c(0, 1.25))+
  theme(legend.position = ",one")
save_plot(plot = plot, name = paste("com", v, sep = "_"), width = 8, height = 8)
v <- "Fp"
plot <- 
  ggplot(com) +
  geom_col(aes(x = com, y = .data[[v]], fill = com)) +
  theme_ppt() +
  labs(x = "", y = vnames[vnames$vars == v, "names"]) +
  scale_fill_manual(values = cols()[c(2,2)]) +
  scale_y_continuous(limits = c(0, 0.17))+
  theme(legend.position = ",one")
save_plot(plot = plot, name = paste("com", v, sep = "_"), width = 8, height = 8)
v <- "Wp"
plot <- 
  ggplot(com) +
  geom_col(aes(x = com, y = .data[[v]], fill = com)) +
  theme_ppt() +
  labs(x = "", y = vnames[vnames$vars == v, "names"]) +
  scale_fill_manual(values = cols()[c(2,2)]) +
  scale_y_continuous(limits = c(0, 0.17))+
  theme(legend.position = ",one")
save_plot(plot = plot, name = paste("com", v, sep = "_"), width = 8, height = 8)

com2[,-11]/com1[,-11]
^



