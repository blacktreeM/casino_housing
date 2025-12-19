setwd('~/casino_housing')
# https://files.zillowstatic.com/research/public_csvs/zhvi/City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1753627718
# https://files.zillowstatic.com/research/public_csvs/zhvi/County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
# https://files.zillowstatic.com/research/public_csvs/zhvi/State_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv
city = 'City_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv'
county = 'County_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv'
# zillow city
data = read.csv(city); head(data[,1:9])
data = data %>% filter(State == 'AR') %>% select(1, 3, 9:ncol(.)); head(data[, 1:5])
z_city = data %>% pivot_longer(cols = starts_with("X"), 
                              names_to = "date", values_to = "price", names_prefix = "X") %>% 
  mutate(name = RegionName) %>% select(date, name, price) %>% 
  mutate(date = gsub('\\.', '', substring(date, 1, 7)),
         date = as.numeric(date)); head(z_city)
# zillow county
data = read.csv(county); head(data[,1:10])
z_county = data %>% filter(State == 'AR') %>% mutate(fips = StateCodeFIPS * 1000 + MunicipalCodeFIPS) %>%
  select(1, 3, 10:ncol(.)) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "price", names_prefix = "X") %>% 
  mutate(name = gsub(' County', '', RegionName), 
         date = gsub('\\.', '', substring(date, 1, 7)), date = as.numeric(date)) %>% 
  select(date, fips, name, price) %>%
  arrange(date, fips); head(z_county)
# https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_County_History.csv
# https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_Zip_History.csv
# https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_State_History.csv
library(stringr)
city = 'RDC_Inventory_Core_Metrics_Zip_History.csv'
county = 'RDC_Inventory_Core_Metrics_County_History.csv'
# realtor city
data = read.csv(city); head(data[,1:10])
data = data %>% select(1:4) %>% filter(postal_code > 70000 & postal_code < 80000); head(data)
colnames(data) = c('date', 'zip', 'name', 'price'); head(data)
r_city = data %>% filter(grepl(', ar', name)) %>% 
  mutate(name = str_to_title(gsub(', ar', '', name))) %>% 
  arrange(date, zip)
subset(r_city, zip == '72801')[1:10,]
r_city = r_city %>% arrange(date, zip)
# realtor county
data = read.csv(county); head(data[,1:5])
colnames(data) = c('date', 'fips', 'name', 'price'); head(data)
r_county = data %>% select(1:4) %>% filter(grepl(', ar', name)) %>% 
  mutate(name = str_to_title(gsub(', ar', '', name))) %>% arrange(date, fips)
subset(r_county, name=='Pope')[1:10,]
r_county = r_county %>% arrange(date, fips)
# merge county
head(z_county); head(r_county)
counties = z_county %>% rename('zillow' = 'price') %>% full_join(r_county, by = c('date', 'name', 'fips'))
head(counties); subset(counties, name=='Pope')
summary(counties); table(counties$date)
save(counties, file = 'counties.RDa')
# merge city
head(z_city); head(r_city)
length(unique(r_city$name));length(unique(r_city$name))
setdiff(unique(r_city$name), unique(z_city$name))
setdiff(unique(z_city$name), unique(r_city$name))
# "Hot Springs"          "Helena - West Helena"
subset(r_city, zip==72390 & date==202511)
subset(r_city, zip==72342 & date==202511)
subset(z_city, name=="Helena - West Helena" & date==202111)
subset(r_city, zip==71762 & date==202511)
subset(r_city, name=='Russellville' & date==202411)
subset(z_city, name=='Russellville' & date==202411)
subset(r_city, name=='Little Rock' & date==202411)
subset(z_city, name=='Little Rock' & date==202411)
subset(r_city, name=='Bentonville' & date==202411)
subset(z_city, name=='Bentonville' & date==202411)
subset(z_city, name=='Hot Springs' & date==202411)
subset(r_city, name=='Hot Springs National Park' & date==202411)
subset(r_city, grepl("Hot Springs", name) & date == 202411)
# average over multiple zip

r_city_av = r_city %>% group_by(date, name) %>% summarize(price = mean(price)) %>% ungroup(); head(r_city_av)
tail(subset(r_city_av, name=='Russellville'))
subset(r_city_av, name=='Little Rock' & date==202411)
subset(z_city, name=='Little Rock' & date==202411)
cities = z_city %>% rename('zillow' = 'price') %>% full_join(r_city_av, by = c('date', 'name')); head(cities)
tail(subset(cities, name=='Russellville'))
save(cities, file = 'cities.RDa')
# zillow state
state = 'State_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv'
data = read.csv(state); head(data[,1:10])
ar = data %>% filter(RegionName=='Arkansas') %>% 
  select(6:ncol(.)) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "price", names_prefix = "X") %>% 
  mutate(date = gsub('\\.', '', substring(date, 1, 7)),
         date = as.numeric(date)); head(ar)
# realtor state
state = 'RDC_Inventory_Core_Metrics_State_History.csv'
data = read.csv(state); head(data[,1:10])
data = data %>% select(1:2, 4) %>% filter(state=='Arkansas') 
colnames(data) = c('date', 'name', 'price'); data %>% arrange(date) %>% head()
states = ar %>% rename('zillow' = 'price') %>% full_join(data, by = 'date'); head(states)
save(states, file = 'states.RDa')
states_q = states %>%
  mutate(date_full = as.Date(paste0(date, "01"), format = "%Y%m%d")) %>%
  mutate(date_q = as.yearqtr(date_full)) %>%
  select(-date_full) %>%
  group_by(name, date_q) %>%
  summarize(across(.cols = -date, .fns = mean, na.rm = TRUE),
            original_date = first(date), .groups = 'drop') %>%
  mutate(time_numeric = as.numeric(date_q),
         min_time = min(time_numeric, na.rm = TRUE),
         time = 4 * (time_numeric - min_time)) %>%
  select(-time_numeric, -min_time); head(states_q)
save(states_q, file = 'states_q.RDa')
