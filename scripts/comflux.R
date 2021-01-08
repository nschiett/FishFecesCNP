library(ggplot2)

loadd(comflux)
devtools::load_all()

comflux = summ
ggplot(comflux) +
  geom_point(aes(x = year, y = Wc, color = site_name), se = FALSE) +
  facet_wrap(reef_zone~diet2, scales = "free_y")

comflux <- comflux %>%
  dplyr::mutate(Wn_prop = Wn/(Wn + Fn),
                Wp_prop = Wp/(Wp + Fp),
                Wnp = Wn/Wp,
                Wcn = Wc/Wn,
                Inp = In/Ip,
                Icn = Ic/In)
hist(comflux$Inp)
hist(comflux$Wnp)

comflux_prop <- 
  comflux %>%
  dplyr::group_by(year, site_name, reef_zone) %>%
  dplyr::summarise(Sn_prop = 
                  (Wn[diet2 == "2_imix"] + Wn[diet2 == "3_plank"] + Wn[diet2 == "4_carn"])/
                   (2 *  Sn[diet2 == "1_hmd"]),
                  Sp_prop = 
                    (Wp[diet2 == "2_imix"] + Wp[diet2 == "3_plank"] + Wp[diet2 == "4_carn"])/
                    (2 *  Sp[diet2 == "1_hmd"]),
                  Sc_prop = 
                    (Wc[diet2 == "2_imix"] + Wc[diet2 == "3_plank"] + Wc[diet2 == "4_carn"])/
                    (2 *  Sc[diet2 == "1_hmd"]),
                  Sp_prop2 = 
                    (Wp[diet2 == "2_imix"])/
                    (2 *  Sp[diet2 == "1_hmd"]),
                  Sp_prop3 = 
                    (Wp[diet2 == "3_plank"])/
                    (2 *  Sp[diet2 == "1_hmd"]),
                  Sp_prop4 = 
                    (Wp[diet2 == "4_carn"])/
                    (2 *  Sp[diet2 == "1_hmd"])) %>%
  dplyr::group_by(reef_zone) %>%
  dplyr::summarize_if(is.numeric, mean)

sum <-
comflux %>%
  dplyr::group_by(site_name, reef_zone, year) %>%
  dplyr::mutate(Ic_sum = sum(Ic),
                In_sum = sum(In),
                Ip_sum = sum(Ip)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Ic_p = Ic/Ic_sum,
                In_p = In/In_sum,
                Ip_p = Ip/Ip_sum,
                Wc_p = Wc/Ic_sum,
                Wn_p = Wn/In_sum,
                Wp_p = Wp/Ip_sum) %>%
  dplyr::group_by(reef_zone, diet2) %>%
  dplyr::summarize_if(is.numeric, median)

sum[,c(1,2,25:30)]

ggplot(comflux_prop) +
  geom_histogram(aes(x = Sn_prop, fill = reef_zone), alpha = 0.5)

ggplot(comflux_prop) +
  geom_histogram(aes(x = Sp_prop, fill = reef_zone), alpha = 0.5)

ggplot(comflux_prop) +
  geom_histogram(aes(x = Sc_prop, fill = reef_zone), alpha = 0.5)





