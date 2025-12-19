library(tidysynth); library(ggplot2); library(htmlTable); library(dplyr); library(tidyr); library(patchwork); library(zoo)
library(zoo); library(lubridate)
load('county_data_r.RDa'); data = data_r;table(data$name); unique(data$name)
(gc_mean = data %>% filter(name == 'Garland', time %in% period1) %>% summarise(mean_z = mean(price)) %>% pull())
(jc_mean = data %>% filter(name == 'Jefferson', time %in% period2) %>% summarise(mean_z = mean(price)) %>% pull())
(cc_mean = data %>% filter(name == 'Crittenden', time %in% period1) %>% summarise(mean_z = mean(price)) %>% pull())
(gc = data %>% group_by(name) %>%  # keep 30 based on pre price mean values
    mutate(city_mean = mean(price[time %in% period1])) %>% ungroup() %>%
    mutate(abs_diff = abs(city_mean - gc_mean)) %>% 
    filter(!(name %in% c('Jefferson', 'Crittenden'))) %>% 
    arrange(abs_diff) %>%
    filter(name %in% head(unique(name), 30)) %>% select(name) %>% unique() %>% pull())
(jc = data %>% group_by(name) %>%  # keep 30 based on pre price mean values
    mutate(city_mean = mean(price[time %in% period1])) %>% ungroup() %>%
    mutate(abs_diff = abs(city_mean - jc_mean)) %>% 
    filter(!(name %in% c('Garland', 'Crittenden'))) %>% 
    arrange(abs_diff) %>%
    filter(name %in% head(unique(name), 30))%>% select(name) %>% unique() %>% pull())
(cc = data %>% group_by(name) %>%  # keep 30 based on pre price mean values
    mutate(city_mean = mean(price[time %in% period1])) %>% ungroup() %>%
    mutate(abs_diff = abs(city_mean - cc_mean)) %>% 
    filter(!(name %in% c('Jefferson', 'Garland'))) %>% 
    arrange(abs_diff) %>%
    filter(name %in% head(unique(name), 30)) %>% select(name) %>% unique() %>% pull())
(treat1 = data %>% filter(original_date==201904) %>% select(time) %>% unique() %>% pull())
(treat2 = data %>% filter(original_date==202010) %>% select(time) %>% unique() %>% pull())
(period1 = 0:(treat1-1))
(period2 = 0:(treat2-1))
county = function(sample, treated, casino, period, sig.ipop, placebo) {
  data %>% synthetic_control(outcome = price, 
                      unit = name, 
                      time = time, 
                      i_unit = treated, 
                      i_time = casino,
                      generate_placebos=placebo) %>%
    generate_predictor(time_window = period,
                       income = mean(log(income)),
                       unemployed = mean(unemployed),
                       edu = mean(edu),
                       old = mean(old),
                       young = mean(young),
                       lfp = mean(lfp),
                       black = mean(black),
                       hispanic = mean(hispanic)) %>%
    generate_predictor(time_window = period[length(period)], last_price = log(price)) %>% 
    generate_weights(optimization_window = period, sigf_ipop = sig.ipop, margin_ipop = 0.02, bound_ipop = 6) %>% 
    generate_control()
}
gc2 = county(gc, 'Garland', treat1, period1, 7, T)
jc2 = county(jc, 'Jefferson', treat2, period2, 7, T)
cc2 = county(cc, 'Crittenden', treat1, period1, 7, T)
(date_quarter = data %>% select(time, date_q) %>% unique())
result = function (data, time_window = NULL) {
  if (!(".meta" %in% colnames(data))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(data$.original_data[[1]][[time_index]])
  }
  data %>% grab_synthetic_control(placebo = FALSE) %>% filter(time_unit %in% time_window) %>%
    rename(Synthetic = synth_y,  treated = real_y, time = time_unit) %>% 
    mutate(effect = treated - Synthetic) %>% left_join(date_quarter, by = 'time')
}
(GC = result(gc2)); (JC = result(jc2)); (CC = result(cc2))
load('states_q.RDa'); states_q = states_q %>% filter(date_q >= as.yearqtr("2016 Q3"))
x_axis_start <- as.yearqtr("2016 Q3") 
x_axis_end <- as.yearqtr("2025 Q4")
(custom_breaks <- seq(x_axis_start, x_axis_end, by = 1))
plot_sy = function(df, city, casino){
  color_values = c( 'Synthetic Control' = 'darkgrey', 'Arkansas' = 'black')
  color_values[city] = 'black'
  ggplot(data = df) + ggtitle(city) +
    geom_line(data = states_q, linetype = 'dotted', aes(x = date_q, y = price, color = 'Arkansas'), linewidth = 1)+
    geom_line(aes(x=date_q, y = Synthetic, color = 'Synthetic Control'), linewidth = 1) + 
    geom_line(aes(x=date_q, y = treated, color = city), linewidth = 1) +
    scale_x_yearqtr(name = '',format = "%Y Q%q", n = 13, breaks = custom_breaks,
                    limits = c(x_axis_start, x_axis_end)) +
    geom_vline(xintercept = as.yearqtr(casino), color = "black") +
    scale_y_continuous(name = "", expand=c(0,0), limits = c(0, 374000)) + 
    theme_classic() + theme(legend.position = c(0.05, 0.99), 
                            legend.justification = c("left", "top"), 
                            legend.text = element_text(size = 16),
                            legend.background = element_rect(fill = "white", color = "white"),
                            plot.title = element_text(hjust = 0.5, size = 18))+
    guides(color=guide_legend(title="")) +
    scale_color_manual(values = color_values,
                       breaks = c(city, 'Synthetic Control', 'Arkansas'))
}; plot_sy(GC, 'Garland', "2019 Q2")
ggsave(plot = plot_sy(GC, 'Garland', "2019 Q2")/plot_sy(JC, 'Jefferson', "2020 Q4")/plot_sy(CC, 'Crittenden', "2019 Q2"),
       filename = 'plot_county_realtor.png', width = 9, height=12)
#
control = function(data){
  bind_rows(grab_unit_weights(data, placebo = FALSE) %>% 
              mutate(type = "Control Unit Weights (W)"), grab_predictor_weights(data,placebo = FALSE) %>% 
              rename(unit = variable)) %>% arrange(desc(weight)) %>% 
    mutate(unit = forcats::fct_reorder(unit, weight)) %>% filter(type!= "Variable Weights (V)") %>%
    select(-type) %>% filter(weight>0.001)
}; control(gc2)
(gc_control = control(gc2) %>% mutate(x = paste0(unit, ' (', round(weight, 3), ')')) %>% select(x) %>% 
    mutate(id = 1:n()))
(jc_control = control(jc2) %>% mutate(y = paste0(unit, ' (', round(weight, 3), ')')) %>% select(y) %>% 
    mutate(id = 1:n()))
(cc_control = control(cc2) %>% mutate(z = paste0(unit, ' (', round(weight, 3), ')')) %>% select(z) %>% 
    mutate(id = 1:n()))
(controls = gc_control %>% full_join(jc_control, by = 'id') %>% full_join(cc_control, by = 'id') %>% select(-id))
colnames(controls) = c('Synthetic<br>Garland', 'Synthetic<br>Jefferson', 'Synthetic<br>Crittenden')
controls
writeLines(htmlTable(controls, rnames = F), "controls_county_realtor.html")
