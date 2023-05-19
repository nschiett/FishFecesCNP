
theme_custom <- function(){

    font <- "Helvetica"   #assign font family up front
    
    theme_bw() %+replace%    #replace elements we want to change
      
      theme(
        
        #grid elements
        #panel.grid.major = element_blank(),    #strip major gridlines
        #panel.grid.minor = element_blank(),    #strip minor gridlines
        #axis.ticks = element_blank(),          #strip axis ticks
        
        #since theme_minimal() already strips axis lines, 
        #we don't need to do that again
        
        #text elements
        plot.title = element_text(             #title
          family = font,            #set font family
          size = 20,                #set font size
          face = 'bold',            #bold typeface
          hjust = 0,                #left align
          vjust = 2,
          color = "black"),               #raise slightly
        
        plot.subtitle = element_text(          #subtitle
          family = font,            #font family
          size = 10,
          color = "black"),               #font size
        
        plot.caption = element_text(           #caption
          family = font,            #font family
          size = 9,                 #font size
          hjust = 1,
          color = "black"),               #right align
        
        axis.title = element_text(             #axis titles
          family = font,            #font family
          size = 12,
          color = "black"),               #font size
        
        axis.text = element_text(              #axis text
          family = font,            #axis famuly
          size = 10,
          color = "black"),                #font size
        
        axis.text.x = element_text(            #margin for axis text
          margin = margin(5, b = 10)),
        
        legend.text = element_text(
          family = font,
          size = 10
        ),
        legend.title = element_text(
          family = font,
          size = 12
        )
    
      )
}

#' Title
#'
#' @param plot 
#' @param name 
#' @param width 
#' @param height 
#'
#' @return
#' @export
#'
#' @examples
save_plot <- function(plot, name, width = 8, height = 6, type = "png"){
  
  ggsave(plot, filename = paste0("output/plots/", name, ".", type), 
         width = width, height = height)
  return("name")
}

#' fig 1
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @import ggplot2
#' @import fishualize
#' @import patchwork
#'
#' @examples
make_fig1 <- function(data){
  
  dat <- data %>%
    dplyr::mutate(cdif1 = Dc_lqn > Wc_uqn | Dc_uqn < Wc_lqn,
           ndif1 = Dn_lqn > Wn_uqn | Dn_uqn < Wn_lqn,
           pdif1 = Dp_lqn > Wp_uqn | Dp_uqn < Wp_lqn,
           cndif1 = Dcn_lqn > Wcn_uqn | Dcn_uqn < Wcn_lqn,
           npdif1 = Dnp_lqn > Wnp_uqn | Dnp_uqn < Wnp_lqn,
           cpdif1 = Dcp_lqn > Wcp_uqn | Dcp_uqn < Wcp_lqn,
           cdif2 = Dc_lqr > Wc_uqr | Dc_uqr < Wc_lqr,
           ndif2 = Dn_lqr > Wn_uqr | Dn_uqr < Wn_lqr,
           pdif2 = Dp_lqr > Wp_uqr | Dp_uqr < Wp_lqr,
           cndif2 = Dcn_lqr > Wcn_uqr | Dcn_uqr < Wcn_lqr,
           npdif2 = Dnp_lqr > Wnp_uqr | Dnp_uqr < Wnp_lqr,
           cpdif2 = Dcp_lqr > Wcp_uqr | Dcp_uqr < Wcp_lqr) 
  
  p1 <- ggplot(dat) +
    geom_abline(slope = 1) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    geom_abline(slope = 0.25, alpha = 0.5) +
    geom_abline(slope = 0.75, alpha = 0.5) +
    geom_errorbar(aes(x = Dc_median, ymin = Wc_lqr, ymax = Wc_uqr, color = diet2), 
                  alpha = 0.3, size = 1) +
    geom_errorbarh(aes(xmin = Dc_lqr, xmax = Dc_uqr, y = Wc_median,  color = diet2), 
                   alpha = 0.3, size = 1) +
    geom_point(aes(x = Dc_median, y = Wc_median, color = diet2), 
               alpha = 0.7, size = 3) +
    geom_label(aes(x = 50, y = 1 * 50, label = "1")) +
    geom_label(aes(x = 50, y = 0.5 * 50, label = "0.50")) +
    geom_label(aes(x = 50, y = 0.75 * 50, label = "0.75")) +
    labs(x = "Stomach content C%", y = "Feces C%", color = "diet") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia") +
    theme_custom() + theme(legend.position = "none")

  p2 <- ggplot(dat) +
    geom_abline(slope = 1) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    geom_abline(slope = 0.25, alpha = 0.5) +
    geom_abline(slope = 0.75, alpha = 0.5) +
    geom_errorbar(aes(x = Dn_median, ymin = Wn_lqr, ymax = Wn_uqr, color = diet2), 
                  alpha = 0.3, size = 1) +
    geom_errorbarh(aes(xmin = Dn_lqr, xmax = Dn_uqr, y = Wn_median,  color = diet2), 
                   alpha = 0.3, size = 1) +
    geom_point(aes(x = Dn_median, y = Wn_median, color = diet2), 
               alpha = 0.7, size = 3) +
    geom_label(aes(x = 12, y = 1 * 12, label = "1")) +
    geom_label(aes(x = 12, y = 0.5 * 12, label = "0.50")) +
    geom_label(aes(x = 12, y = 0.75 * 12, label = "0.75")) +
    geom_label(aes(x = 12, y = 0.25 * 12, label = "0.25")) +
    labs(x = "Stomach content N%", y = "Feces N%", color = "Trophic guild") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia", 
                       labels = c("DE", "HE", "MI", "CO", "PL", "CA")) +
    theme_custom()
  
  p3 <- ggplot(dat) +
    geom_abline(slope = 1) +
    geom_abline(slope = 0.5, alpha = 0.5) +
    #geom_abline(slope = 0.25, alpha = 0.5) +
    geom_abline(slope = 0.75, alpha = 0.5) +
    geom_errorbar(aes(x = Dp_median, ymin = Wp_lqr, ymax = Wp_uqr, color = diet2), 
                  alpha = 0.3, size = 1) +
    geom_errorbarh(aes(xmin = Dp_lqr, xmax = Dp_uqr, y = Wp_median,  color = diet2), 
                   alpha = 0.3, size = 1) +
    geom_point(aes(x = Dp_median, y = Wp_median, color = diet2), 
               alpha = 0.7, size = 3) +
    geom_label(aes(x = 2.2, y = 1 * 2.2, label = "1")) +
    geom_label(aes(x = 2.2, y = 0.5 * 2.2, label = "0.50")) +
    geom_label(aes(x = 2.2, y = 0.75 * 2.2, label = "0.75")) +
    labs(x = "Stomach content P%", y = "Feces P%") +
    scale_color_fish_d(option = "Pseudocheilinus_tetrataenia") +
    theme_custom() + theme(legend.position = "none")

  plot <-
  p1 + p2 + p3 +
    plot_layout(nrow = 3) + 
    plot_annotation(tag_levels = "A")
  
  return(plot)
  
}


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#' 
#' @import ggplot2
#' @import fishualize
#' @import ggstance
#' @import patchwork
#'
#' @examples
make_figs1 <- function(data){
  
  dat <- data %>%
    dplyr::mutate(cdif1 = Dc_lqn > Wc_uqn | Dc_uqn < Wc_lqn,
                  ndif1 = Dn_lqn > Wn_uqn | Dn_uqn < Wn_lqn,
                  pdif1 = Dp_lqn > Wp_uqn | Dp_uqn < Wp_lqn,
                  cndif1 = Dcn_lqn > Wcn_uqn | Dcn_uqn < Wcn_lqn,
                  npdif1 = Dnp_lqn > Wnp_uqn | Dnp_uqn < Wnp_lqn,
                  cpdif1 = Dcp_lqn > Wcp_uqn | Dcp_uqn < Wcp_lqn,
                  cdif2 = Dc_lqr > Wc_uqr | Dc_uqr < Wc_lqr,
                  ndif2 = Dn_lqr > Wn_uqr | Dn_uqr < Wn_lqr,
                  pdif2 = Dp_lqr > Wp_uqr | Dp_uqr < Wp_lqr,
                  cndif2 = Dcn_lqr > Wcn_uqr | Dcn_uqr < Wcn_lqr,
                  npdif2 = Dnp_lqr > Wnp_uqr | Dnp_uqr < Wnp_lqr,
                  cpdif2 = Dcp_lqr > Wcp_uqr | Dcp_uqr < Wcp_lqr) 
  
  p1 <-
  ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
    geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rc_median)), 
               alpha = 0.3, size = 5, data = dat[dat$cdif2 == TRUE,]) +
    geom_segment(aes(x = Dc_lqn, xend = Dc_uqn), size = 0.5, color = "grey40",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Dc_lqr, xend = Dc_uqr), size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_point(aes(x = Dc_median), size = 2,
               position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Wc_lqn, xend = Wc_uqn), color = "grey40",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_segment(aes(x = Wc_lqr, xend = Wc_uqr),  size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_point(aes(x = Wc_median), size = 2, shape = 17,
               position = position_nudge(y = -0.2)) +
    fishualize::scale_color_fish(option = "Hypsypops_rubicundus",
                                 trans = "log", breaks = c(0.5, 1, 2, 4)) +
    scale_shape_discrete(name = "", labels = c("gut", "feces")) +
    theme_custom() +
    labs(x = "C %", y = "", color = "feces:diet")  +
    theme(legend.position = "bottom")
  
  p2 <-
  ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
    geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rn_median)), 
               alpha = 0.3, size = 5, data = dat[dat$ndif2 == TRUE,]) +
    geom_segment(aes(x = Dn_lqn, xend = Dn_uqn), size = 0.5, color = "grey40",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Dn_lqr, xend = Dn_uqr), size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_point(aes(x = Dn_median), size = 2,
               position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Wn_lqn, xend = Wn_uqn), color = "grey40",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_segment(aes(x = Wn_lqr, xend = Wn_uqr),  size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_point(aes(x = Wn_median), size = 2, shape = 17,
               position = position_nudge(y = -0.2)) +
    fishualize::scale_color_fish(option = "Hypsypops_rubicundus", 
                                 trans = "log", breaks = c(0.5, 1, 2, 4)) +
    scale_shape_discrete(name = "", labels = c("gut", "feces")) +
    theme_custom() +
    labs(x = "N %", y = "", color = "feces:diet")  +
    theme(legend.position = "bottom", axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  
  p3 <-
  ggplot(dat, aes(y = forcats::fct_rev(species), yend = forcats::fct_rev(species))) +
    geom_hline(aes(yintercept = forcats::fct_rev(species), color = (Rp_median)), 
               alpha = 0.3, size = 5, data = dat[dat$pdif2 == TRUE,]) +
    geom_segment(aes(x = Dp_lqn, xend = Dp_uqn), size = 0.5, color = "grey40",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Dp_lqr, xend = Dp_uqr), size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = 0.2)) +
    geom_point(aes(x = Dp_median), size = 2,
               position = position_nudge(y = 0.2)) +
    geom_segment(aes(x = Wp_lqn, xend = Wp_uqn), color = "grey40",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_segment(aes(x = Wp_lqr, xend = Wp_uqr),  size = 1.5, color = "grey30",
                 linetype = 1, position = position_nudge(y = -0.2)) +
    geom_point(aes(x = Wp_median), size = 2, shape = 17,
               position = position_nudge(y = -0.2)) +
    fishualize::scale_color_fish(option = "Hypsypops_rubicundus",
                                 trans = "log", breaks = c(0.5, 1, 2, 4)) +
    scale_shape_discrete(name = "", labels = c("gut", "feces")) +
    theme_custom() +
    labs(x = "P %", y = "", color = "feces:diet") +
    theme(legend.position = "bottom", axis.text.y = element_blank(), 
          axis.ticks.y = element_blank())
  
  plot <-
  p1 + p2 + p3 + plot_layout(nrow = 1)
  
  plot
  
}

#' #' Title
#' #'
#' #' @param model_ae_diet 
#' #'
#' #' @return
#' #' @export
#' #' @import ggplot2
#' #' @import fishualize
#' #' @import patchwork
#' #'
#' make_fig2 <- function(model_ae_diet) {
#'   
#'   pred_data <- model_ae_diet[[2]]
#'   data <- model_ae_diet[[1]]$data
#'   
#'   a <- 
#'     ggplot(pred_data) +
#'     geom_ribbon(aes(x = mu1_st, ymin= y_a_lb, ymax = y_a_ub, fill = element), 
#'                 alpha = 0.3) +
#'     geom_line(aes(x = mu1_st, y = y_a_m, color = element), size = 1) +
#'     geom_point(aes(x = mu1_st, y = ae, color = element), data = data) +
#'     scale_color_fish_d(end = 0.8) + scale_fill_fish_d(end = 0.8) +
#'     labs(y = "Assimilation efficiency", x = "Standardised Food %", 
#'          color = "", fill = "") +
#'     theme_custom() +
#'     theme(legend.position = "top")
#'   
#'   b <-
#'     ggplot(pred_data) +
#'     geom_ribbon(aes(x = dn, ymin= y_mu1_lb, ymax = y_mu1_ub, fill = element), 
#'                 alpha = 0.3) +
#'     geom_line(aes(x = dn, y = y_mu1_m, color = element), size = 1) +
#'     geom_point(aes(x = dn, y = mu1_st, color = element), data = data) +
#'     scale_color_fish_d(end = 0.8) + scale_fill_fish_d(end = 0.8) +
#'     labs(x = "%dN", y = "Standardised Food %") +
#'     theme_custom() + theme(legend.position = "none")
#'   
#'   a + b + plot_layout(nrow = 2)
#'   
#' }


#' Title
#'
#' @param model_ae_diet 
#'
#' @return
#' @export
#' @import ggplot2
#' @import fishualize
#' @import patchwork
#'
make_fig3 <- function(models_ae_diet, result_ext) {

  data <- result_ext
  nd1 <- expand_grid(Dp_mean = seq(0.05, 2.7, 0.01),
                     int_rel = seq(1.5,5.2,0.01))
  
  fe <- fixef(models_ae_diet[[3]], summary = F)
  
  nd1$estimate <- lapply(1:nrow(fe), function(i){
    df <- data.frame(c = fe[i,1] + nd1$int_rel*fe[i,2] + (nd1$Dp_mean)*fe[i,3])
  }) %>% bind_cols() %>% rowMeans()
  
  pp <-
    ggplot(nd1[nd1$estimate<1&nd1$estimate>0,]) +
    geom_raster(aes(x = Dp_mean, y = int_rel, fill = estimate)) +
    geom_point(aes(x = Dp_mean, y = log(int_rel), shape = as.character(diet2)), 
               alpha = 0.9, data = data, size = 3) +
    scale_fill_fish(option = "Chaetodon_ephippium", limits = c(0,1), 
                    breaks = c(0,0.2, 0.4, 0.6, 0.8, 1)) +
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
  

  # N
  
  ndn <- expand_grid(Dn_mean = seq(0.8, 12, 0.05),
                     int_rel = seq(1.5,5.2,0.05))
  
  fe <- fixef(models_ae_diet[[2]], summary = F)
  
  ndn$estimate <- lapply(1:nrow(fe), function(i){
    df <- data.frame(c = fe[i,1] + ndn$int_rel*fe[i,2] + (ndn$Dn_mean)*fe[i,3])
  }) %>% bind_cols() %>% rowMeans()
  
  
  pn <-
    ggplot(ndn[ndn$estimate<1&ndn$estimate>0,]) +
    geom_raster(aes(x = Dn_mean, y = int_rel, fill = estimate)) +
    geom_point(aes(x = Dn_mean, y = log(int_rel), shape = as.character(diet2)), 
               alpha = 0.7, data = data, size = 3) +
    scale_fill_fish(option = "Chaetodon_ephippium", limits = c(0,1), breaks = c(0, 0.2, 0.4,0.6, 0.8, 1), guide = "none")  +
    theme_classic() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_shape_discrete(labels=c('DE', 'HE', 'MI', 'CO', 'PL', 'CA')) +
    theme(legend.position = 'bottom') +
    labs(x = "Stomach content N%", y = "log(intestine surface/ biomass)", 
         fill = "Predicted absorption efficiency", shape = "") +
    theme(text = element_text(size = 14))
  
  ####c
  ndc <- expand_grid(Dc_mean = seq(5, 50, 0.5),
                     int_rel = seq(1.5,5.2,0.01))
  
  fe <- fixef(models_ae_diet[[1]], summary = F)
  
  ndc$estimate <- lapply(1:nrow(fe), function(i){
    df <- data.frame(c = fe[i,1] + ndc$int_rel*fe[i,2] + (ndc$Dc_mean)*fe[i,3])
  }) %>% bind_cols() %>% rowMeans()
  
  pc <-
    ggplot(ndc[ndc$estimate<1,]) +
    geom_raster(aes(x = Dc_mean, y = int_rel, fill = estimate)) +
    geom_point(aes(x = Dc_mean, y = log(int_rel), shape = as.character(diet2)), 
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

  plot <-
    pc + pn + pp + plot_annotation(tag_levels = "A")
  save_plot(plot, "fig3", height = 6, width = 12)
  
  return(plot)
  
}

make_fig2 <- function(result_ext, models_copro){
  
  ## Coprophagy models
  ce1n <- brms::conditional_effects(models_copro[[1]])[[1]]
  ce2n <- brms::conditional_effects(models_copro[[2]])[[1]]
  ce1p <- brms::conditional_effects(models_copro[[3]])[[1]]
  ce2p <- brms::conditional_effects(models_copro[[4]])[[1]]
  
  p1 <- 
    ggplot(ce1n) + 
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), 
                alpha = 0.3) +
    geom_line(aes(x = effect1__, y = estimate__),
              size = 1) +
    labs(x = "Stomach content N%", y = "Probability being coprophageous") +
    coord_cartesian(expand= FALSE) +
    theme_custom() +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
    
  p1
  
  library(ggdist)
  
 p1d <-  ggplot(result_ext) +
    stat_pointinterval(aes(x = Dn_mean, color = diet2, fill = diet2, y = diet2), alpha = 1, .width = c(0.95, 0.5))+
    scale_x_continuous(limits = c(0, 12)) +
   coord_cartesian(expand= 0.0, clip = "off") +
    theme_custom() +
   theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
         axis.title.x = element_blank(), axis.title.y = element_blank())  +
   theme(axis.text.x = element_text(size = 12), 
         axis.text.y = element_text(size = 12), 
         axis.title.x = element_text(size = 16),
         legend.text = element_text(size = 16))
 p1b <- p1d + p1 +
   plot_layout(nrow = 2, heights = c(1,5))
  
  p2 <- 
    ggplot(ce2n) + 
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), 
                alpha = 0.3) +
    geom_line(aes(x = effect1__, y = estimate__),
              size = 1) +
    labs(x = "Feces N%", y = "Probability feces eaten by coprophage") +
    coord_cartesian(expand= FALSE) +
    theme_custom() +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
  p2
  pd <-  ggplot(result_ext) +
    geom_jitter(aes(x = Wn_mean, color = diet2, fill = diet2, y = 1), alpha = 0.3) +
    scale_x_continuous(limits = c(0, 9)) +
    geom_hline(yintercept = 0) +
    coord_cartesian(expand= FALSE) +
    theme_void()  +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
  p2b <- pd + p2 +
    plot_layout(nrow = 2, heights = c(1, 3)) 
  
  p3 <- 
    ggplot(ce1p) + 
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), 
                alpha = 0.3) +
    geom_line(aes(x = effect1__, y = estimate__),
              size = 1) +
    labs(x = "Stomach content P%", y = "Probability being coprophageous") +
    coord_cartesian(expand= FALSE) +
    theme_custom() +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
  p3
  pd <-  ggplot(result_ext) +
    geom_density(aes(x = Dp_mean, color = diet2, fill = diet2), alpha = 0.3)+
    scale_x_continuous(limits = c(0, 12)) +
    geom_hline(yintercept = 0) +
    coord_cartesian(expand= FALSE) +
    theme_void() +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
  p3b <- pd + p3 +
    plot_layout(nrow = 2)
  p4 <- 
    ggplot(ce2p) + 
    geom_ribbon(aes(x = effect1__, ymin = lower__, ymax = upper__), 
                alpha = 0.3) +
    geom_line(aes(x = effect1__, y = estimate__),
              size = 1) +
    labs(x = "Feces P%", y = "Probability feces eaten by coprophage") +
    theme_custom()  +
    theme(axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          axis.title.x = element_text(size = 16),
          legend.text = element_text(size = 16))
  p4
  
  
  
  result_cnp <- (result_ext)
  
  cop <- result_cnp %>%
    dplyr::select(species, Dn_median, Dp_median, Wn_median, Wp_median) %>%
    dplyr::mutate(copn = Dn_median < 4.9, copp = Dp_median < 0.8,
                  fecn = Wn_median > 2.6, fecp = Wp_median > 0.58)
  
  ## heatplot N
  on <- outer(result_cnp$Wn_median, result_cnp$Dn_median, "-") 
  on2 <- outer(result_cnp$Wn_lqr, result_cnp$Dn_uqr, ">") 
  on <- on*on2
  
  
  colnames(on) <-  result_cnp$species
  rownames(on) <-  result_cnp$species
  on <- as.data.frame(on) %>%
    mutate(sp_feces = result_cnp$species) %>%
    pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
    mutate(value = case_when(value <= 0 ~ NA_real_,
                             sp_feces == sp_diet ~ NA_real_,
                             TRUE ~ value))
  
  species <- result_cnp %>%
    select(species, diet2) %>%
    unique() %>%
    arrange(diet2, species)
  
  sp1 <- species %>%
    rename(sp_diet = species, diet1 = diet2)
  sp2 <- species %>%
    rename(sp_feces = species)
  
  on <- on %>% left_join(sp1) %>% left_join(sp2) %>%
    mutate(sp_diet = as.factor(sp_diet),
           sp_feces = as.factor(sp_feces)) %>%
    mutate(sp_diet = fct_relevel(sp_diet, species$species),
           sp_feces = fct_relevel(sp_feces, species$species))
  n <- 
    ggplot(on) +
    geom_hline(aes(yintercept = species), 
               size = 0.3, alpha = 0.5, 
               data = cop[cop$copn == TRUE, ]) +
    geom_vline(aes(xintercept = species), 
               size = 0.3, alpha = 0.5, 
               data = cop[cop$fecn == TRUE, ]) +
    geom_tile(aes(x = (sp_feces),
                  y = fct_rev(sp_diet),
                  fill = value), alpha = 0.8) +
    fishualize::scale_fill_fish(option = "Trimma_lantana",
                                name = "N diff (%)", na.value = NA,
                                trans = "sqrt") +
    theme_bw() +
    facet_grid(diet1~diet2, scales = "free",
               space = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
          strip.background = element_rect(fill = "white"),  # Make facet label background white.
          # axis.title.x = element_blank(),
          # axis.text.x = element_blank(), 
          panel.grid = element_blank()) +
    labs(y = "Food", x = "Feces") 
  n
  ##### P #######
  op <- outer(result_cnp$Wp_median, result_cnp$Dp_median, "-") 
  op2 <- outer(result_cnp$Wp_lqr, result_cnp$Dp_uqr, ">") 
  op <- op*op2
  
  
  colnames(op) <-  result_cnp$species
  rownames(op) <-  result_cnp$species
  op <- as.data.frame(op) %>%
    mutate(sp_feces = result_cnp$species) %>%
    pivot_longer(cols = 1:51, names_to = "sp_diet") %>%
    mutate(value = case_when(value <= 0 ~ NA_real_,
                             sp_feces == sp_diet ~ NA_real_,
                             TRUE ~ value))
  
  species <- result_cnp %>%
    select(species, diet2) %>%
    unique() %>%
    arrange(diet2, species)
  
  sp1 <- species %>%
    rename(sp_diet = species, diet1 = diet2)
  sp2 <- species %>%
    rename(sp_feces = species)
  
  op <- op %>% left_join(sp1) %>% left_join(sp2) %>%
    mutate(sp_diet = as.factor(sp_diet),
           sp_feces = as.factor(sp_feces)) %>%
    mutate(sp_diet = fct_relevel(sp_diet, species$species),
           sp_feces = fct_relevel(sp_feces, species$species))
  
  p <-
    ggplot(op) +
    geom_hline(aes(yintercept = species), 
               size = 0.3, alpha = 0.5, 
               data = cop[cop$copp == TRUE, ]) +
    geom_vline(aes(xintercept = species), 
               size = 0.3, alpha = 0.5, 
               data = cop[cop$fecp == TRUE, ]) +
    geom_tile(aes(x = (sp_feces),
                  y = fct_rev(sp_diet),
                  fill = value),
              alpha = 0.8) +
    fishualize::scale_fill_fish(option = "Trimma_lantana",
                                name = "P diff (%)", na.value = NA,
                                trans = "sqrt") +
    theme_bw() +
    facet_grid(diet1~diet2, scales = "free",
               space = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
          strip.background = element_rect(fill = "white"),
          # axis.title.y = element_blank(), 
          panel.grid = element_blank()) +
    labs(x = "Feces", y = "Food")
  p

 plot_species <- n + p + plot_layout(nrow = 2)
 save_plot(plot_species, "figS2", 10, 18)
 
 ## second plot
 on <- outer(result_cnp$Wn_median, result_cnp$Dn_median, "-") 
 colnames(on) <-  result_cnp$species
 rownames(on) <-  result_cnp$species
 on <- as.data.frame(on) %>%
   mutate(sp_feces = result_cnp$species) %>%
   pivot_longer(cols = 1:51, names_to = "sp_diet") 
 
 on <- on %>% left_join(sp1) %>% left_join(sp2) %>%
   mutate(sp_diet = as.factor(sp_diet),
          sp_feces = as.factor(sp_feces)) %>%
   mutate(sp_diet = fct_relevel(sp_diet, species$species),
          sp_feces = fct_relevel(sp_feces, species$species))
 

 on_sum <- on %>%
   # mutate(value = case_when(is.na(value) ~ 0,
   #                  TRUE ~ value)) %>%
   group_by(diet1, diet2) %>%
   summarise(value = median(value, na.rm = T)) %>%
   filter(!diet1==diet2, value>0) %>%
   drop_na()
 
 ps1 <- 
 ggplot(on_sum) +
   geom_tile(aes(x = (diet2),
                 y = fct_rev(diet1),
                 fill = value),
             alpha = 0.8) +
   fishualize::scale_fill_fish(option = "Trimma_lantana",
                               name = "Median (feces N - stomach content N) (%)", na.value = NA) +
   scale_x_discrete(labels = c( "HE", "MI", "CO", "PL", "CA")) +
   scale_y_discrete(labels = c( "MI", "HE", "DE")) +
   theme_custom() +
   theme(axis.text.x = element_text(angle = 0)) +
   theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
         strip.background = element_rect(fill = "white"),
         panel.grid = element_blank(),
         legend.position = "top") +
   guides(fill = guide_colorbar(title.position = 'top', 
                                title.hjust = .5, 
                                barwidth = unit(20, 'lines'), 
                                barheight = unit(.5, 'lines'))) +
   labs(x = "Feces", y = "Stomach content")  +
   theme(axis.text.x = element_text(size = 12), 
         axis.text.y = element_text(size = 12), 
         axis.title.x = element_text(size = 16),
         axis.title.y = element_text(size = 16),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 16))
 
 op <- outer(result_cnp$Wp_median, result_cnp$Dp_median, "-") 
 
 colnames(op) <-  result_cnp$species
 rownames(op) <-  result_cnp$species
 op <- as.data.frame(op) %>%
   mutate(sp_feces = result_cnp$species) %>%
   pivot_longer(cols = 1:51, names_to = "sp_diet") 


 sp1 <- species %>%
   rename(sp_diet = species, diet1 = diet2)
 sp2 <- species %>%
   rename(sp_feces = species)
 
 op <- op %>% left_join(sp1) %>% left_join(sp2) %>%
   mutate(sp_diet = as.factor(sp_diet),
          sp_feces = as.factor(sp_feces)) %>%
   mutate(sp_diet = fct_relevel(sp_diet, species$species),
          sp_feces = fct_relevel(sp_feces, species$species))
 
 op_sum <- op %>%
   group_by(diet1, diet2) %>%
   summarise(value = median(value))%>%
   filter(!diet1==diet2, value>0) 
 
 ps2 <- ggplot(op_sum) +
   geom_tile(aes(x = (diet2),
                 y = fct_rev(diet1),
                 fill = value),
             alpha = 0.8) +
   fishualize::scale_fill_fish(option = "Trimma_lantana",
                               name = "Median (feces P - stomach content P) (%)", na.value = NA) +
   scale_x_discrete(labels = c( "HE", "MI", "CO", "PL", "CA")) +
   scale_y_discrete(labels = c( "PL", "CO", "MI", "HE", "DE")) +
   theme_custom() +
   theme(axis.text.x = element_text(angle = 0)) +
   theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
         strip.background = element_rect(fill = "white"),
         axis.title.y = element_blank(), 
         panel.grid = element_blank(),
         legend.position = "top") +
   guides(fill = guide_colorbar(title.position = 'top', 
                                title.hjust = .5, 
                                barwidth = unit(20, 'lines'), 
                                barheight = unit(.5, 'lines'))) +
   labs(x = "Feces") +
   theme(axis.text.x = element_text(size = 12), 
         axis.text.y = element_text(size = 12), 
         axis.title.x = element_text(size = 16),
         legend.text = element_text(size = 12),
         legend.title = element_text(size = 16))
  des <- 
    "AB
     CD
     EF"
  plot <-
    ps1 + ps2 + p1 + p3 + p2 + p4 + plot_annotation(tag_levels = "A") +
    plot_layout(ncol = 2, design = des) & theme(strip.placement = NULL)
  ggsave("output/plots/fig2_diet_poo_pairwise.png", 
         plot, height = 14, width = 12)
  return(plot)
}

make_fig4 <- function(spflux){
 print("test")
  data <- spflux[[2]] %>%
    dplyr::mutate(RRn = log(Wn/Fn),
                  RRp = log(Wp/Fp),
                  RRnp = log((Wn/Wp)/(Fn/Fp))) %>%
    tidyr::pivot_longer(c(RRn, RRp))
  
  data2 <- spflux[[2]] %>%
    dplyr::mutate(RRnp = log((Wn/Wp)/(Fn/Fp))) 
  
  data %>% group_by(name) %>%
    summarise(n = sum(value>0))
  
  plot <-
    ggplot(data) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_boxplot(aes(x = diet2, y = value, color = name, fill = name),
                 alpha = 0.5) +
    scale_color_fish_d(end = 0.8, begin = 0.4, labels = c( "N", "P")) +
    scale_fill_fish_d(end = 0.8, begin = 0.4, labels = c( "N", "P")) +
    scale_x_discrete(labels = c("DE", "HE", "MI", "CO", "PL", "CA")) +
    scale_y_continuous(limits = c(-2, 4.9)) +
    theme_custom() +
    labs(x = "", y = "Release ratio \n ln(egestion/excretion)", 
         fill = "", color = "") +
    add_fishape(family = "Acanthuridae", option = "Ctenochaetus_striatus", 
                xmin = 0.5, xmax = 1.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    add_fishape(family = "Acanthuridae", option = "Zebrasoma_scopas", 
                xmin = 1.5, xmax = 2.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    add_fishape(family = "Balistidae", option = NA, 
                xmin = 2.5, xmax = 3.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    add_fishape(family = "Chaetodontidae", option = "Chaetodon_ornatissimus", 
                xmin = 3.5, xmax = 4.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    add_fishape(family = "Pomacentridae", option = NA, 
                xmin = 4.5, xmax = 5.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    add_fishape(family = "Serranidae", option = "Cephalopholis_argus", 
                xmin = 5.5, xmax = 6.5, ymin = 3.9, ymax = 4.9, fill = "grey20") +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "top") +
   
    ggplot(data2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_boxplot(aes(x = diet2, y = RRnp),
                 alpha = 0.5, fill = "grey") +
    scale_x_discrete(labels = c("DE", "HE", "MI", "CO", "PL", "CA")) +
    theme_custom() +
    labs(x = "", y = "Release ratio N:P \n ln(egestion/excretion)", 
         fill = "", color = "") +
    theme(axis.text.x = element_text(angle = 0),
          legend.position = "top") +
    
    plot_layout(nrow = 2) + plot_annotation(tag_levels = "A")
      
  save_plot(plot, "fig4_RR", width = 7, height = 8)
  
  return(plot)
}

make_table1 <- function(data_ae, result_ext){
 
  
  sub <- result_ext %>%
    dplyr::select(species, family, diet2)
  
  data <- data_ae %>%
    dplyr::ungroup() %>%
    dplyr::filter(location == "Moorea",
                  species %in% result_ext$species,
                  c>0) %>%
    dplyr::select(species, tl, weight, id) %>%
    unique() %>%
    dplyr::group_by(species) %>%
    dplyr::summarize(
      tl_m = mean(tl, na.rm=TRUE),
      tl_min = min(tl, na.rm=TRUE),
      tl_max = max(tl, na.rm=TRUE),
      w_m = mean(weight, na.rm=TRUE),
      w_min = min(weight, na.rm=TRUE),
      w_max = max(weight, na.rm=TRUE),
      n = length(unique(id))
    ) %>%
    dplyr::mutate(tl_range = paste0(round(tl_min,1), "-", round(tl_max, 1)),
                  w_range = paste0(round(w_min,1), "-", round(w_max, 1))) %>%
    dplyr::left_join(sub) %>%
    dplyr::mutate(species = gsub("_", " ", species),
                  diet = dplyr::case_when(
                    diet2 == "1_detr" ~ "detritivore",
                    diet2 == "2_herb" ~ "herbivore",
                    diet2 == "3_imix" ~ "mixed invertivore",
                    diet2 == "4_cor" ~ "corallivore",
                    diet2 == "5_plank" ~ "planktivore",
                    diet2 == "6_carn" ~ "carnivore"
                  )) %>%
    dplyr::select(Species = species, Family = family, 
                  Diet = diet, 
                  `Range TL (cm)` = tl_range, 
                  `Range weight (g)` = w_range, 
                  Replicates = n) %>%
    dplyr::arrange(Family)
   data
   
   readr::write_csv(data, "output/data/Table_S1.csv")
}

make_table2 <- function(result_ext){
  
  sub <- result_ext %>%
    dplyr::select(species, family, diet2, 
                  Dc_mean, Dc_lqn, Dc_uqn,
                  Dn_mean, Dn_lqn, Dn_uqn,
                  Dp_mean, Dp_lqn, Dp_uqn,
                  Wc_mean, Wc_lqn, Wc_uqn,
                  Wn_mean, Wn_lqn, Wn_uqn,
                  Wp_mean, Wp_lqn, Wp_uqn,
                  ac_mean, ac_lqn, ac_uqn,
                  an_mean, an_lqn, an_uqn,
                  ap_mean, ap_lqn, ap_uqn) %>%
    dplyr::mutate(species = gsub("_", " ", species),
                  diet = dplyr::case_when(
                    diet2 == "1_detr" ~ "detritivore",
                    diet2 == "2_herb" ~ "herbivore",
                    diet2 == "3_imix" ~ "mixed invertivore",
                    diet2 == "4_cor" ~ "corallivore",
                    diet2 == "5_plank" ~ "planktivore",
                    diet2 == "6_carn" ~ "carnivore"
                  )) %>%
    dplyr::mutate(Dc = paste0(round(Dc_mean, 1)," (", 
                              round(Dc_lqn, 1), "-", 
                              round(Dc_uqn, 1), ")"),
                  Dn = paste0(round(Dn_mean, 1)," (", 
                              round(Dn_lqn, 1), "-", 
                              round(Dn_uqn, 1), ")"),
                  Dp = paste0(round(Dp_mean, 1)," (", 
                              round(Dp_lqn, 1), "-", 
                              round(Dp_uqn, 1), ")"),
                  Wc = paste0(round(Wc_mean, 1)," (", 
                              round(Wc_lqn, 1), "-", 
                              round(Wc_uqn, 1), ")"),
                  Wn = paste0(round(Wn_mean, 1)," (", 
                              round(Wn_lqn, 1), "-", 
                              round(Wn_uqn, 1), ")"),
                  Wp = paste0(round(Wp_mean, 1)," (", 
                              round(Wp_lqn, 1), "-", 
                              round(Wp_uqn, 1), ")"),
                  ac = paste0(round(ac_mean, 1)," (", 
                              round(ac_lqn, 1), "-", 
                              round(ac_uqn, 1), ")"),
                  an = paste0(round(an_mean, 1)," (", 
                              round(an_lqn, 1), "-", 
                              round(an_uqn, 1), ")"),
                  ap = paste0(round(ap_mean, 1)," (", 
                              round(ap_lqn, 1), "-", 
                              round(ap_uqn, 1), ")")) %>%
    dplyr::select(Species = species, 
                  Family = family, Diet = diet, 
                  `Diet C (%)` = Dc, 
                  `Diet N (%)` = Dn, 
                  `Diet P (%)` = Dp, 
                  `Feces C (%)` = Wc, 
                  `Feces N (%)` = Wn, 
                  `Feces P (%)` = Wp) %>%
    dplyr::arrange(Family)
  
  
  readr::write_csv(sub, "output/data/Table_results2.csv")
}


make_table3 <- function(result_ext){
  
  sub <- result_ext %>%
    dplyr::select(species, family, diet2, 
                  Dc_mean, Dc_lqn, Dc_uqn,
                  Dn_mean, Dn_lqn, Dn_uqn,
                  Dp_mean, Dp_lqn, Dp_uqn,
                  Wc_mean, Wc_lqn, Wc_uqn,
                  Wn_mean, Wn_lqn, Wn_uqn,
                  Wp_mean, Wp_lqn, Wp_uqn,
                  ac_mean, ac_lqn, ac_uqn,
                  an_mean, an_lqn, an_uqn,
                  ap_mean, ap_lqn, ap_uqn) %>%
    dplyr::mutate(species = gsub("_", " ", species),
                  diet = dplyr::case_when(
                    diet2 == "1_detr" ~ "detritivore",
                    diet2 == "2_herb" ~ "herbivore",
                    diet2 == "3_imix" ~ "mixed invertivore",
                    diet2 == "4_cor" ~ "corallivore",
                    diet2 == "5_plank" ~ "planktivore",
                    diet2 == "6_carn" ~ "carnivore"
                  )) %>%
    dplyr::mutate(Dc = paste0(round(Dc_mean, 1)," (", 
                              round(Dc_lqn, 1), "-", 
                              round(Dc_uqn, 1), ")"),
                  Dn = paste0(round(Dn_mean, 1)," (", 
                              round(Dn_lqn, 1), "-", 
                              round(Dn_uqn, 1), ")"),
                  Dp = paste0(round(Dp_mean, 1)," (", 
                              round(Dp_lqn, 1), "-", 
                              round(Dp_uqn, 1), ")"),
                  Wc = paste0(round(Wc_mean, 1)," (", 
                              round(Wc_lqn, 1), "-", 
                              round(Wc_uqn, 1), ")"),
                  Wn = paste0(round(Wn_mean, 1)," (", 
                              round(Wn_lqn, 1), "-", 
                              round(Wn_uqn, 1), ")"),
                  Wp = paste0(round(Wp_mean, 1)," (", 
                              round(Wp_lqn, 1), "-", 
                              round(Wp_uqn, 1), ")"),
                  ac = paste0(round(ac_mean, 2)," (", 
                              round(ac_lqn, 2), "-", 
                              round(ac_uqn, 2), ")"),
                  an = paste0(round(an_mean, 2)," (", 
                              round(an_lqn, 2), "-", 
                              round(an_uqn, 2), ")"),
                  ap = paste0(round(ap_mean, 2)," (", 
                              round(ap_lqn, 2), "-", 
                              round(ap_uqn, 2), ")")) %>%
    dplyr::select(Species = species, 
                  Family = family, Diet = diet, 
                  `AE C` = ac, 
                  `AE N` = an, 
                  `AE P` = ap) %>%
    dplyr::arrange(Family)
  
  
  readr::write_csv(sub, "output/data/Table_results3.csv")
}


make_table4 <- function(pflux){
  
  sub <- pflux %>%
    filter(reef_zone == "forereef") %>%
    ungroup() %>%
    dplyr::mutate(diet = dplyr::case_when(
                    diet2 == "1_detr" ~ "detritivore",
                    diet2 == "2_herb" ~ "herbivore",
                    diet2 == "3_imix" ~ "mixed invertivore",
                    diet2 == "4_cor" ~ "corallivore",
                    diet2 == "5_plank" ~ "planktivore",
                    diet2 == "6_carn" ~ "carnivore"
                  )) %>%
    dplyr::select(Diet = diet, 
                  `P consumption` = Ip, 
                  `P Excretion` = Fp, 
                  `P Egestion` = Wp, 
                  `P to coprophagy` = pflux_cop) %>%
    dplyr::arrange(Diet)
  
  
  readr::write_csv(sub, "output/data/Table_results4.csv")
}


