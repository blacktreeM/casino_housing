library(ggplot2); library(dplyr); library(tidyr); library(patchwork); library(zoo)
load('city_data_z.RDa'); data = data_z
data = data %>% filter(date_q >= as.yearqtr("2012 Q1")) %>% arrange(name, date_q); head(data)# delete before 2013
city_z = data %>% filter(name %in% c('Hot Springs', 'Pine Bluff', 'West Memphis')) %>% select(name, date_q, zillow)
load('city_data_r.RDa'); data = data_r
city_r = data %>% filter(name %in% c('Hot Springs', 'Pine Bluff', 'West Memphis')) %>% select(name, date_q, price)
city = city_z %>% full_join(city_r, by = c('name', 'date_q')) 
load('county_data_z.RDa'); data = data_z
data = data %>% filter(date_q >= as.yearqtr("2012 Q1")) %>% arrange(name, date_q)
county_z = data %>% filter(name %in% c('Garland', 'Jefferson', 'Crittenden')) %>% select(name, date_q, zillow)
load('county_data_r.RDa'); data = data_r
county_r = data %>% filter(name %in% c('Garland', 'Jefferson', 'Crittenden')) %>% select(name, date_q, price); head(county)
county = county_z %>% full_join(county_r, by = c('name', 'date_q')) %>% mutate(name = paste(name, 'County'))
load('states_q.RDa')
ar = states_q %>% arrange(date_q)%>% filter(date_q >= as.yearqtr("2012 Q1")) 
data = city %>% bind_rows(county) #%>% bind_rows(ar)
data = data %>%
  mutate(date_q = as.yearqtr(date_q, format = "%Y Q%q")) %>%
  pivot_longer(
    cols = c(zillow, price), 
    names_to = "Metric",     
    values_to = "Value"      
  ) %>% filter(!is.na(name)); head(data); table(data$name)
x_axis_start <- as.yearqtr("2012 Q1") 
x_axis_end <- as.yearqtr("2025 Q4")
(custom_breaks <- seq(x_axis_start, x_axis_end, by = 1))
city_order <- c(
  "Hot Springs", 
  "Pine Bluff", 
  "West Memphis", 
  "Garland County", 
  "Jefferson County", 
  "Crittenden County"
)
data = data %>% mutate(name = factor(name, levels = city_order))
ggplot(data = data, aes(x = date_q, y = Value, group = interaction(name, Metric), color = Metric)) + 
  geom_line(size = 1) +
  facet_wrap(~ name, nrow = 2) + 
  scale_x_yearqtr(format = "%Y Q%q", name='') + ylab('')+
  scale_color_manual(
    name = "", 
    values = c("zillow" = "black", "price"  = "darkgrey"), 
    labels = c("zillow" = "Zillow Housing Index", 
      "price"  = "Realtor.com Median Listing Price")) +
  theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
  guides(color=guide_legend(title="")) 
ggsave('plot_price.png', height=6, width=8)