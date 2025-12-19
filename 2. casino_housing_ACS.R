# city level
key = '60afb8b8550abb1dc40fef1e46bf9c492b1dc98d'
library(tidycensus); library(dplyr); library(tidyr)
census_api_key(key, install=T, overwrite = T)
acs =  function(year){
  get_acs(geography = "place", state = "AR",
          variables = c(income = 'B19013_001', employed = 'B23025_004', unemployed = 'B23025_005',
                        pop = 'B03002_001', black = 'B03002_004', hispanic = 'B03002_012', 
                        male = 'B15002_002', ns1 = 'B15002_003',
                        ns2 = 'B15002_004', ns3 = 'B15002_005',
                        ns4 = 'B15002_006', ns5 = 'B15002_007', 
                        ns6 = 'B15002_008', ns7 = 'B15002_009',
                        ns8 = 'B15002_010', collegeM = 'B15002_015',
                        masterM = 'B15002_016', profM = 'B15002_017',
                        doctorM = 'B15002_018', 
                        female = 'B15002_019', nsf1 = 'B15002_020',
                        nsf2 = 'B15002_021', nsf3 = 'B15002_022',
                        nsf4 = 'B15002_023', nsf5 = 'B15002_024', 
                        nsf6 = 'B15002_025', nsf7 = 'B15002_026',
                        nsf8 = 'B15002_027', collegeF = 'B15002_032',
                        masterF = 'B15002_033', profF = 'B15002_034',
                        doctorF = 'B15002_035',
                        pop16 = 'DP03_0001',
                        lf = 'DP03_0003',
                        pop_foreign = 'DP02_0088',
                        foreign = 'DP02_0094',
                        men = 'DP02_0025',
                        men_marry = 'DP02_0027',
                        women = 'DP02_0031',
                        women_marry = 'DP02_0033',
                        age_pop = 'DP05_0001',
                        old = 'DP05_0024',
                        young = 'DP05_0019',
                        median_age = 'DP05_0018'),
          year = year)  %>% select(-moe) %>% 
    rename('fips' = 'GEOID', 'city' = 'NAME') %>% 
    pivot_wider(names_from = variable, values_from = estimate)%>% 
    mutate(fips = as.numeric(fips), 
           edu = 100*(collegeM + masterM + profM + doctorM+collegeF + masterF + profF +  doctorF)/(male+female),
           drop=100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8+nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/(male+female),
           edu_male = 100*(collegeM + masterM + profM + doctorM)/male,
           edu_female = 100*(collegeF + masterF + profF +  doctorF)/female,
           drop_male = 100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8)/male,
           drop_female = 100*(nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/female) %>% 
    mutate(men_married = 100*men_marry/men,
           women_married = 100*women_marry/women,
           young = 100*young/age_pop, old = 100*old/age_pop,
           lfp = 100*lf/pop16, 
           foreign = 100*foreign/pop_foreign,
           black = 100*black/pop, hispanic = 100*hispanic/pop,
           unemployed = 100*unemployed / (employed + unemployed),
           year = year)
}
acs16 = acs(2016)
acs17 = acs(2017)
acs18 = acs(2018)
acs = rbind(acs17, acs18) # 2016 acs different variables
acs = acs %>% mutate(city = gsub(', Arkansas', '', city), city = gsub(' city| town', '', city)); unique(acs$city)
acs %>% arrange(desc(pop)) %>% select(city, pop, year) %>% print(n=30)
save(acs, file='acs_city.RDa')
# County
acs =  function(year){
  get_acs(geography = "county", state = "AR",
          variables = c(income = 'B19013_001', employed = 'B23025_004', unemployed = 'B23025_005',
                        pop = 'B03002_001', black = 'B03002_004', hispanic = 'B03002_012', 
                        male = 'B15002_002', ns1 = 'B15002_003',
                        ns2 = 'B15002_004', ns3 = 'B15002_005',
                        ns4 = 'B15002_006', ns5 = 'B15002_007', 
                        ns6 = 'B15002_008', ns7 = 'B15002_009',
                        ns8 = 'B15002_010', collegeM = 'B15002_015',
                        masterM = 'B15002_016', profM = 'B15002_017',
                        doctorM = 'B15002_018', 
                        female = 'B15002_019', nsf1 = 'B15002_020',
                        nsf2 = 'B15002_021', nsf3 = 'B15002_022',
                        nsf4 = 'B15002_023', nsf5 = 'B15002_024', 
                        nsf6 = 'B15002_025', nsf7 = 'B15002_026',
                        nsf8 = 'B15002_027', collegeF = 'B15002_032',
                        masterF = 'B15002_033', profF = 'B15002_034',
                        doctorF = 'B15002_035',
                        pop16 = 'DP03_0001',
                        lf = 'DP03_0003',
                        pop_foreign = 'DP02_0088',
                        foreign = 'DP02_0094',
                        men = 'DP02_0025',
                        men_marry = 'DP02_0027',
                        women = 'DP02_0031',
                        women_marry = 'DP02_0033',
                        age_pop = 'DP05_0001',
                        old = 'DP05_0024',
                        young = 'DP05_0019',
                        median_age = 'DP05_0018'),
          year = year)  %>% select(-moe) %>% 
    rename('fips' = 'GEOID', 'county' = 'NAME') %>% 
    pivot_wider(names_from = variable, values_from = estimate)%>% 
    mutate(fips = as.numeric(fips), 
           edu = 100*(collegeM + masterM + profM + doctorM+collegeF + masterF + profF +  doctorF)/(male+female),
           drop=100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8+nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/(male+female),
           edu_male = 100*(collegeM + masterM + profM + doctorM)/male,
           edu_female = 100*(collegeF + masterF + profF +  doctorF)/female,
           drop_male = 100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8)/male,
           drop_female = 100*(nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/female) %>% 
    mutate(men_married = 100*men_marry/men,
           women_married = 100*women_marry/women,
           young = 100*young/age_pop, old = 100*old/age_pop,
           lfp = 100*lf/pop16, 
           foreign = 100*foreign/pop_foreign,
           black = 100*black/pop, hispanic = 100*hispanic/pop,
           unemployed = 100*unemployed / (employed + unemployed),
           year = year)
}
acs16 = acs(2016)
acs17 = acs(2017)
acs18 = acs(2018)
acs = rbind(acs17, acs18)
acs = acs %>% mutate(county = gsub(', Arkansas', '', county)); unique(acs$county)
acs %>% arrange(desc(pop)) %>% select(county, pop) %>% print(n=30)
save(acs, file = 'acs_county.RDa')