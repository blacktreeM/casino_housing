library(zoo);library(dplyr); library(tidyr)
load('cities.RDa'); head(cities); unique(cities$name)
subset(cities, name=='Arkansas')
subset(cities, name=='Hot Springs')
subset(cities, name=='Hot Springs National Park')
hs = cities %>% filter(name=='Hot Springs') %>% select(date, name, zillow); head(hs)
hsnp = cities %>% filter(name=='Hot Springs National Park') %>% select(date, price); head(hsnp)
HS = hs %>% full_join(hsnp, by = 'date'); head(HS)
cities = cities %>% filter(name!='Hot Springs' & name!='Hot Springs National Park') %>% bind_rows(HS)
load('acs_city.RDa'); head(acs); table(acs$year)
acs = acs %>% select(city, income, unemployed, edu, old, young, lfp, black, hispanic, pop) %>% 
  rename('name' = 'city') %>% 
  group_by(name) %>% summarize(
    income = mean(income, na.rm = TRUE),
    unemployed = mean(unemployed, na.rm = TRUE),
    edu = mean(edu, na.rm = TRUE),
    old = mean(old, na.rm = TRUE),
    young = mean(young, na.rm = TRUE),
    lfp = mean(lfp, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE)
  ) %>% ungroup(); head(acs)
city_data = cities %>% left_join(acs, by = 'name') %>% filter(!is.na(income))
head(city_data); tail(city_data); summary(city_data)
unique(city_data$name)
summary(city_data)
data_z = city_data %>% filter(!is.na(zillow)) %>% select(-price) %>%
  mutate(date_full = as.Date(paste0(date, "01"), format = "%Y%m%d")) %>%
  mutate(date_q = as.yearqtr(date_full)) %>% # quarterly data
  select(-date_full) %>%
  group_by(name, date_q) %>%
  summarize(across(.cols = -date, .fns = mean, na.rm = TRUE), 
            original_date = first(date), .groups = 'drop') %>%
  mutate(time_numeric = as.numeric(date_q),
         min_time = min(time_numeric, na.rm = TRUE),
         time = 4 * (time_numeric - min_time)) %>%
  select(-time_numeric, -min_time); head(data_z)
data_r = city_data %>% filter(!is.na(price)) %>% select(-zillow)  %>%
  mutate(date_full = as.Date(paste0(date, "01"), format = "%Y%m%d")) %>%
  mutate(date_q = as.yearqtr(date_full)) %>%
  select(-date_full) %>%
  group_by(name, date_q) %>%
  summarize(across(.cols = -date, .fns = mean, na.rm = TRUE),
            original_date = first(date), .groups = 'drop') %>%
  mutate(time_numeric = as.numeric(date_q),
         min_time = min(time_numeric, na.rm = TRUE),
         time = 4 * (time_numeric - min_time)) %>%
  select(-time_numeric, -min_time); head(data_r)
save(data_z, file = 'city_data_z.RDa')
save(data_r, file = 'city_data_r.RDa')

# county
load('counties.RDa'); table(counties$name)
load('acs_county.RDa'); head(acs); table(acs$year)
acs = acs %>% select(county, income, unemployed, edu, old, young, lfp, black, hispanic, pop) %>% 
  rename('name' = 'county') %>% mutate(name = gsub(' County', '', name)) %>% 
  group_by(name) %>% summarize(
    income = mean(income, na.rm = TRUE),
    unemployed = mean(unemployed, na.rm = TRUE),
    edu = mean(edu, na.rm = TRUE),
    old = mean(old, na.rm = TRUE),
    young = mean(young, na.rm = TRUE),
    lfp = mean(lfp, na.rm = TRUE),
    black = mean(black, na.rm = TRUE),
    hispanic = mean(hispanic, na.rm = TRUE),
    pop = mean(pop, na.rm = TRUE)
  ) %>% ungroup(); head(acs)
data = counties %>% left_join(acs, by = 'name') %>% filter(!is.na(income))
head(data); tail(data); summary(data)
# creating time var
data_z = data %>% filter(!is.na(zillow)) %>% select(-price) %>%
  mutate(date_full = as.Date(paste0(date, "01"), format = "%Y%m%d")) %>%
  mutate(date_q = as.yearqtr(date_full)) %>%
  select(-date_full) %>%
  group_by(name, date_q) %>%
  summarize(across(.cols = -date, .fns = mean, na.rm = TRUE),
            original_date = first(date), .groups = 'drop') %>%
  mutate(time_numeric = as.numeric(date_q),
         min_time = min(time_numeric, na.rm = TRUE),
         time = 4 * (time_numeric - min_time)) %>%
  select(-time_numeric, -min_time); head(data_z)
data_r = data %>% filter(!is.na(price)) %>% select(-zillow)  %>%
  mutate(date_full = as.Date(paste0(date, "01"), format = "%Y%m%d")) %>%
  mutate(date_q = as.yearqtr(date_full)) %>%
  select(-date_full) %>%
  group_by(name, date_q) %>%
  summarize(across(.cols = -date, .fns = mean, na.rm = TRUE),
            original_date = first(date), .groups = 'drop') %>%
  mutate(time_numeric = as.numeric(date_q),
         min_time = min(time_numeric, na.rm = TRUE),
         time = 4 * (time_numeric - min_time)) %>%
  select(-time_numeric, -min_time); head(data_r)
save(data_z, file = 'county_data_z.RDa')
save(data_r, file = 'county_data_r.RDa')
