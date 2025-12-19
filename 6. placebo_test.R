hs1 %>% plot_mspe_ratio()
pb1 %>% plot_mspe_ratio()
wm1 %>% plot_mspe_ratio()
hs2 %>% plot_mspe_ratio()
pb2 %>% plot_mspe_ratio()
wm2 %>% plot_mspe_ratio()
gc1 %>% plot_mspe_ratio()
jc1 %>% plot_mspe_ratio()
cc1 %>% plot_mspe_ratio()
gc2 %>% plot_mspe_ratio()
jc2 %>% plot_mspe_ratio()
cc2 %>% plot_mspe_ratio()
ratio_custom = function (data, time_window = NULL) {
  data %>% grab_significance(time_window = time_window) %>% 
    dplyr::mutate(unit_name = forcats::fct_reorder(as.character(unit_name), mspe_ratio)) %>% 
    ggplot(aes(unit_name, mspe_ratio, fill = type)) + geom_col() + 
    coord_flip() + ggplot2::labs(y = "Post-Period MSPE / Pre-Period MSPE", 
                                 x = "", fill = "", color = "") + 
    scale_fill_manual(values = c("grey", "black")) + 
    scale_color_manual(values = c("grey", "black")) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.grid.major = element_line(colour = "gray80") ,
          panel.grid.minor = element_line(colour = "gray90"), 
          panel.background = element_blank(), axis.line.x = element_line(colour = "black"), 
          axis.line.y = ggplot2::element_line(colour = "black"), 
          axis.text.y = ggplot2::element_text(colour = "black"), 
          axis.text.x = ggplot2::element_text(colour = "black")) +
    theme_classic() +theme(legend.position = "none") 
}; (placebo1 = hs1 %>% ratio_custom())
(placebo2 = gc1 %>% ratio_custom())
ggsave(placebo1, file='placebo_hs_z.png', height=8, width=6)
ggsave(placebo2, file='placebo_gc_z.png', height=8, width=6)
### custom plot_placebo
hs1 %>% plot_placebos(prune=T) 
gc1 %>% plot_placebos(prune=T)
cc1 %>% plot_placebos(prune=T)
gc2 %>% plot_placebos(prune=T)
placebo_custom = function (data, time_window = NULL, prune = TRUE, casino) {
  if (!(".meta" %in% colnames(data))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(data$.original_data[[1]][[time_index]])
  }
  plot_data <- data %>% grab_synthetic_control(placebo = TRUE) %>% 
    dplyr::mutate(diff = real_y - synth_y) %>% dplyr::filter(time_unit %in% time_window) %>% 
    dplyr::mutate(type_text = ifelse(.placebo == 0, treatment_unit, "control units"),
                  type_text = factor(type_text, levels = c(treatment_unit, "control units")))
  caption <- ""
  if (prune) {
    sig_data = data %>% grab_significance(time_window = time_window)
    thres <- sig_data %>% dplyr::filter(type == "Treated") %>% 
      dplyr::pull(pre_mspe) %>% sqrt(.)
    retain_ <- sig_data %>% dplyr::select(unit_name, pre_mspe) %>% 
      dplyr::filter(sqrt(pre_mspe) <= thres * 2) %>% dplyr::pull(unit_name)
    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- "Pruned all placebo cases with a pre-period RMSPE exceeding two times the treated unit's pre-period RMSPE."
  }
  x_axis_start <- as.yearqtr("2012 Q1") 
  x_axis_end <- as.yearqtr("2025 Q4")
  (x_axis <- seq(x_axis_start, x_axis_end, by = 0.25))
  a = min(plot_data$time_unit)
  b = max(plot_data$time_unit)
  mk = seq(a, b, by = 1)
  df = data.frame(time_q = x_axis, time_unit = mk)
  data = plot_data %>% 
    left_join(df, by = 'time_unit') %>% 
    ggplot(aes(x = time_q, y = diff, group = .id, color = type_text, alpha = type_text, size = type_text)) + 
    scale_x_yearqtr(name = '', format = "%Y Q%q") +
    geom_hline(yintercept = 0, color = "black",linetype = 2) + 
    geom_vline(xintercept = as.yearqtr(casino, format = "%Y Q%q"), color = "black", linetype = 3) + 
    geom_line() + 
    scale_color_manual(values = c("black", "grey20")) + 
    scale_alpha_manual(values = c(1, 0.4)) + 
    scale_size_manual(values = c(1, 0.5)) + 
    labs(color = "", alpha = "", size = "", y = '', x = '', 
         title = paste0("Difference of each '", unit_index, "' in the donor pool"), caption = caption) + 
    theme_classic() + 
    theme(legend.position = "bottom")    
}
(gap1 = hs1 %>% placebo_custom(prune = T, casino = '2019 Q3'))
(gap2 = gc1 %>% placebo_custom(prune = T, casino = '2019 Q3'))
ggsave(gap1, file='gap_city_z.png', height=4, width=6)
ggsave(gap2, file='gap_county_z.png', height=4, width=6)
