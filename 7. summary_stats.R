library(htmlTable); library(dplyr); library(tidyr); library(forcats)
city_name = c('Predictors', 'Hot Springs', 'Synthetic<br>Hot Springs', 'Hot Spring<br>sample mean', 
              'Pine Bluff', 'Synthetic<br>Pine Bluff', 'Pine Bluff<br>sample mean', 
              'West Memphis', 'Synthetic<br>West Memphis', 'West Memphis<br>sample mean')
county_name = c('Predictors', 'Garland', 'Synthetic<br>Garland', 'Garland<br>sample mean', 
                'Jefferson', 'Synthetic<br>Jefferson', 'Jefferson<br>sample mean', 
                'Crittenden', 'Synthetic<br>Crittenden', 'Crittenden<br>sample mean')
desired_order = c('last_price', 'income', 'unemployed', 'lfp', 'edu',
                  'young', 'old', 'black', 'hispanic')
var_name = c('ln(Last median house price)', 'ln(Median household income)', 'Unemployment rate',
             'Labor force participation rate', 'Share college graduate',
             'Share young (<15)', 'Share old (>65)',  'Share Black people', 'Share Hispanic people')
## city zillor
hs_sum = hs1 %>% grab_balance_table() 
pb_sum = pb1 %>% grab_balance_table() 
wm_sum = wm1 %>% grab_balance_table() 
(sum_stats = hs_sum %>% left_join(pb_sum, by = 'variable') %>% left_join(wm_sum, by = 'variable'))
colnames(sum_stats) = city_name
sum_stats
(sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors), 
                                  Predictors = fct_relevel(Predictors, desired_order)) %>% 
    arrange(Predictors) %>% select(-Predictors))
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
# anti-log income and house price
#summary_table[1:2, -1] =exp(summary_table[1:2, -1]); summary_table
colnames(summary_table)[1] = 'Predictors'
summary_table[,-1] = lapply(summary_table[,-1], function(x) round(x, 1))
summary_table
writeLines(htmlTable(summary_table, rnames = F), "predictors_city_z.html")
# city realtor
hs_sum = hs2 %>% grab_balance_table() 
pb_sum = pb2 %>% grab_balance_table() 
wm_sum = wm2 %>% grab_balance_table() 
(sum_stats = hs_sum %>% left_join(pb_sum, by = 'variable') %>% left_join(wm_sum, by = 'variable'))
colnames(sum_stats) = city_name
sum_stats
(sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors),
                                  Predictors = fct_relevel(Predictors, desired_order)) %>% 
    arrange(Predictors) %>% select(-Predictors))
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
# anti-log income and house price
#summary_table[1:2, -1] =exp(summary_table[1:2, -1]); summary_table
colnames(summary_table)[1] = 'Predictors'
summary_table[,-1] = lapply(summary_table[,-1], function(x) round(x, 1))
summary_table
writeLines(htmlTable(summary_table, rnames = F), "predictors_city_r.html")

## county zillor
(gc_sum = gc1 %>% grab_balance_table())
(jc_sum = jc1 %>% grab_balance_table()) 
(cc_sum = cc1 %>% grab_balance_table()) 
(sum_stats = gc_sum %>% left_join(jc_sum, by = 'variable') %>% left_join(cc_sum, by = 'variable'))
colnames(sum_stats) = county_name
sum_stats
(sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors),
                                  Predictors = fct_relevel(Predictors, desired_order)) %>% 
    arrange(Predictors) %>% select(-Predictors))
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
# anti-log income and house price
#summary_table[1:2, -1] =exp(summary_table[1:2, -1]); summary_table
colnames(summary_table)[1] = 'Predictors'
summary_table[,-1] = lapply(summary_table[,-1], function(x) round(x, 1))
summary_table
writeLines(htmlTable(summary_table, rnames = F), "predictors_county_z.html")
# county realtor
(gc_sum = gc2 %>% grab_balance_table())
(jc_sum = jc2 %>% grab_balance_table()) 
(cc_sum = cc2 %>% grab_balance_table()) 
(sum_stats = hs_sum %>% left_join(pb_sum, by = 'variable') %>% left_join(wm_sum, by = 'variable'))
colnames(sum_stats) = county_name
sum_stats
(sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors),
                                  Predictors = fct_relevel(Predictors, desired_order)) %>% 
    arrange(Predictors) %>% select(-Predictors))
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
# anti-log income and house price
#summary_table[1:2, -1] =exp(summary_table[1:2, -1]); summary_table
colnames(summary_table)[1] = 'Predictors'
summary_table[,-1] = lapply(summary_table[,-1], function(x) round(x, 1))
summary_table
writeLines(htmlTable(summary_table, rnames = F), "predictors_county_r.html")

# combine tables
tables = paste0(c('controls_city_zillow', 'controls_city_realtor',
                  'controls_county_zillow', 'controls_county_realtor',
                  'predictors_city_z', 'predictors_city_r',
                  'predictors_county_z', 'predictors_county_r'), '.html'); tables
for (file in tables) {
  file_content = readLines(file)
  cat(file, file_content, file = 'Tables.html', append = TRUE, sep = "\n")
  cat("\n", file = 'Tables.html', append = T) 
}
