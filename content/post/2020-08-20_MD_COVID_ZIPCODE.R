library(tidyverse)
library(tibbletime)
rolling_mean <- rollify(mean, window = 5)
# https://data.imap.maryland.gov/datasets/5f459467ee7a4ffda968139011f06c46/data
md_zip_temporal <- read_csv('~/Downloads/MDCOVID19_MASTER_ZIP_CODE_CASES.csv')
md_zip_population <- read_csv('~/Downloads/Maryland_Census_Data_-_ZIP_Code_Tabulation_Areas__ZCTAs_.csv') %>% 
  select(ZCTA5CE10, POP100) %>% mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
colnames(md_zip_population) <- c('ZIP_CODE', 'Population')

# Zip codes with largest shifts from mid July to early July
# taking latest date - or / 7/1 with 7 day rolling mean
md_zip_temporal %>% 
  pivot_longer(cols = F4_11_2020:total07_20_2020) %>% 
  #filter(ZIP_CODE %in% c(20814, 202722, 20782,20783,20740,20742, 20912,20781)) %>% 
  mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
           lubridate::parse_date_time(orders = 'mdy')) %>% 
  mutate(Diff = value - lag(value)) %>% 
  filter(!is.na(Diff), Diff >= 0) %>% 
  mutate(ZIP_CODE = as.character(ZIP_CODE)) %>% 
  group_by(ZIP_CODE) %>% 
  mutate(rm = zoo::rollmean(x = value, 4, 
                            na.pad=TRUE, align="right")) %>% 
  filter(date == as.Date('2020-07-01') | date == as.Date('2020-07-20')) %>% 
  select(-name,  -OBJECTID, -value, -Diff) %>% 
  pivot_wider(values_from = 'rm', names_from = 'date') %>% 
  mutate(Ratio = `2020-07-20` / `2020-07-01`,
         Delta = `2020-07-20` - `2020-07-01`) %>% 
  arrange(-Delta)
  
# plotting cases per day (7 day rolling mean) by zip code
md_zip_temporal %>% 
  pivot_longer(cols = F4_11_2020:total07_20_2020) %>% 
  filter(ZIP_CODE %in% c(20814, 202722, 20782,20783,20740,20742, 20912,20781, 20906)) %>% 
  mutate(date = gsub('^F','',name) %>% gsub('^total','',.) %>% 
           lubridate::parse_date_time(orders = 'mdy')) %>% 
  mutate(Diff = value - lag(value)) %>% 
  filter(!is.na(Diff), Diff >= 0) %>% 
  mutate(ZIP_CODE = as.character(ZIP_CODE)) %>% 
  group_by(ZIP_CODE) %>% 
  mutate(rm = zoo::rollmean(x = Diff, 7, 
                            na.pad=TRUE, align="right")) %>% 
  left_join(md_zip_population) %>% 
  mutate(rmPop = (rm/Population)  * 100000) %>% 
  ggplot(aes(x=date,y=rmPop, color = ZIP_CODE)) + 
  geom_point() + 
  geom_line() + 
  cowplot::theme_cowplot() +
  ylab('7 day rolling mean of new cases/day per capita')
