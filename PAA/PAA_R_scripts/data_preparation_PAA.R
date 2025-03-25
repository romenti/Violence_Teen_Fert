rm(list=ls())
source('R-code/upload_libraries.R')

#### Fertility Data ####
fert_data <- read.csv("data/WPP2024_Fertility_by_Age5.csv")

#### Population by Age ####
pop_age <- read.csv("data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv")

# Countries to be excluded (GPI is not available)

exclude = c("","KSV","TWN","PRK")

#### Total Female Population ####

data("pop1dt")

pop_female_init <- pop1dt %>%
  select(Year=year,country_code,country=name,popF) %>%
  filter(Year>=2009,Year<=2023) %>%
  ungroup() %>%
  group_by(country_code,country) %>%
  summarise(pop_female_init=mean(popF))



#### Extract teen birth rates ####

teen_asfr <- fert_data %>%
  dplyr::select(iso3=ISO3_code,country_code=LocID,
                country=Location,Year=Time,Age=AgeGrpStart,Fx=ASFR,Births) %>%
  filter(!(iso3 %in% exclude)) %>%
  filter(Year>=2009,Year<=2023,Age %in% c(15)) %>%
  select(iso3,country_code,country,Year,Births,F15=Fx) 


#### Regional Groupings ####

Regional_UNDP <- read_excel("Data/Regional_UNDP.xlsx")

region_groups <- Regional_UNDP %>%
  select(iso3,sub_region=`Sub-region Name`,region=`Region Name`) %>%
  mutate(area=case_when(sub_region %in% c('Northern Europe','Western Europe',
                                          'Eastern Europe','Southern Europe',
                                          'Northern America') ~ "Europe and North America",
                        sub_region %in% c('Central Asia','Southern Asia') ~ 'Central and Southern Asia',
                        sub_region %in% c('Eastern Asia') ~ 'Eastern Asia',
                        sub_region %in% c('Latin America and the Caribbean') ~ 'Latin America and the Caribbean',
                        sub_region %in% c('South-eastern Asia') ~ 'South-Eastern Asia',
                        region %in% c('Oceania') ~ 'Oceania',
                        sub_region %in% c('Northern Africa','Western Asia') ~ 'Northern Africa and Western Asia',
                        sub_region %in% c('Sub-Saharan Africa') ~ 'Sub-Saharan Africa'))





#### Read data for gpi ####

data_gpi_overall = read_excel('Data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx',
                              sheet="Overall Scores",
                              skip=3) %>%
  pivot_longer(!c("Country","iso3c"),
               names_to="year",
               values_to="gpi_overall") %>%
  mutate(year=as.numeric(year)) %>%
  rename(iso3=iso3c,Year=year) %>%
  mutate(gpi_overall=case_when(iso3=="PSE" & gpi_overall==0 ~ 2.79,
                               iso3=="SSD" & gpi_overall==0 ~ 2.35,
                               TRUE~gpi_overall)) %>%
  dplyr::select(-Country)

data_gpi_safety = read_excel('Data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx',
                             sheet="Safety and Security",
                             skip=3) %>%
  pivot_longer(!c("Country","iso3c"),
               names_to="year",
               values_to="gpi_safety") %>%
  mutate(year=as.numeric(year)) %>%
  rename(iso3=iso3c,Year=year) %>%
  mutate(gpi_safety=case_when(iso3=="PSE" & gpi_safety==0 ~ 3.01,
                              iso3=="SSD" & gpi_safety==0 ~ 2.74,
                              TRUE~gpi_safety)) %>%
  dplyr::select(-Country)

data_gpi_safety = read_excel('Data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx',
                             sheet="Safety and Security",
                             skip=3) %>%
  pivot_longer(!c("Country","iso3c"),
               names_to="year",
               values_to="gpi_safety") %>%
  mutate(year=as.numeric(year)) %>%
  rename(iso3=iso3c,Year=year) %>%
  mutate(gpi_safety=case_when(iso3=="PSE" & gpi_safety==0 ~ 3.01,
                              iso3=="SSD" & gpi_safety==0 ~ 2.74,
                              TRUE~gpi_safety)) %>%
  dplyr::select(-Country)


data_gpi_militarisation = read_excel('Data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx',
                                     sheet="Militarisation",
                                     skip=3) %>%
  pivot_longer(!c("Country","iso3c"),
               names_to="year",
               values_to="gpi_militarisation") %>%
  mutate(year=as.numeric(year)) %>%
  rename(iso3=iso3c,Year=year) %>%
  mutate(gpi_militarisation=case_when(iso3=="PSE" & gpi_militarisation==0 ~ 1.97,
                                      iso3=="SSD" & gpi_militarisation==0 ~ 2.18,
                                      TRUE~gpi_militarisation)) %>%
  dplyr::select(-Country)


data_gpi_conflict = read_excel('Data/GPI-2022-overall-scores-and-domains-2008-2022.xlsx',
                               sheet="Ongoing Conflict",
                               skip=3) %>%
  pivot_longer(!c("Country","iso3c"),
               names_to="year",
               values_to="gpi_conflict") %>%
  mutate(year=as.numeric(year)) %>%
  rename(iso3=iso3c,Year=year) %>%
  mutate(gpi_conflict=case_when(iso3=="PSE" & gpi_conflict==0 ~ 3.32,
                                iso3=="SSD" & gpi_conflict==0 ~ 2.21,
                                TRUE~gpi_conflict)) %>%
  dplyr::select(-Country)


gpi_area = data_gpi_safety %>%
  left_join(data_gpi_conflict) %>%
  mutate(gpi=(gpi_safety+gpi_conflict)/2) %>%
  left_join(pop_exp) %>%
  left_join(region_group) %>%
  mutate(area=ifelse(area %in% c('Europe & Central Asia','North America'),
                     'Europe, Central Asia & North America',area),
         area=ifelse(area %in% c('South Asia','East Asia & Pacific'),'South-Eastern Asia & Pacific',area)) %>%
  group_by(area,Year) %>%
  summarise(gpi=weighted.mean(gpi,Exp,na.rm = T)) 





#### Read data for other development indicators ####

development_indicators = read.csv('data/development_indicators.txt')


# Gender Development Index

data_gdi = development_indicators  %>%
  dplyr::select(iso3,starts_with('gdi'),-gdi_group_2022) %>%
  pivot_longer(!c("iso3"),names_to="Year",values_to="gdi_lag") %>%
  mutate(Year=as.numeric(substr(Year,5,length(Year)))+1) %>%
  filter(Year %in% seq(2009,2023,1))

# Human Development Index

data_hdi = development_indicators  %>%
  dplyr::select(iso3,starts_with('hdi'),-hdicode,-hdi_rank_2022) %>%
  pivot_longer(!c("iso3"),names_to="Year",values_to="hdi_lag") %>%
  mutate(Year=as.numeric(substr(Year,5,length(Year)))+1) %>%
  filter(Year %in% seq(2009,2023,1))

# Gender Inequality Index

data_gii = development_indicators  %>%
  dplyr::select(iso3,starts_with('gii'),-gii_rank_2022) %>%
  pivot_longer(!c("iso3"),names_to="Year",values_to="gii_lag") %>%
  mutate(Year=as.numeric(substr(Year,5,length(Year)))+1) %>%
  filter(Year %in% seq(2009,2023,1))



#### Marriage Data ####

marriage_data = data.frame()
continents = c('africa','oceania','americas','europe','asia')
for(i in 1:5){
  data_temp = read.csv(paste0('data/marriage_data_',continents[i],'.csv'))
  data_temp = data_temp %>%
    filter(Time %in% seq(2008,2022,1),Sex=='Female',
           Age %in% c('15-19')) %>%
    mutate(Time=Time+1) %>%
    select(iso3=Iso3,Year=Time,marriage_prev_lag=Value)
  marriage_data = rbind(marriage_data,data_temp)
  
}

#### Contraceptive Data ####

contraceptive_data = data.frame()
continents = c('africa','oceania','americas','europe','asia')
for(i in 1:5){
  data_temp = read.csv(paste0('data/contraceptives_',continents[i],'.csv'))
  data_temp = data_temp %>%
    filter(Time %in% seq(2008,2022,1),Sex=='Female',
           Age %in% c('15-49')) %>%
    mutate(Time=Time+1) %>%
    select(iso3=Iso3,Year=Time,contraceptive_prev_lag=Value)
  contraceptive_data = rbind(contraceptive_data,data_temp)
  
}



#### Linked data set #####

data_analysis = teen_asfr %>%
  left_join(region_groups) %>%
  left_join(pop_female_init,by=c('country_code','country')) %>%
  left_join(data_gpi_conflict %>%
              mutate(Year=Year+1),by=c('iso3','Year')) %>%
  left_join(data_gpi_militarisation %>%
              mutate(Year=Year+1),by=c('iso3','Year')) %>%
  left_join(data_gpi_safety %>%
              mutate(Year=Year+1),by=c('iso3','Year')) %>%
  left_join(data_gpi_overall %>%
              mutate(Year=Year+1),by=c('iso3','Year')) %>%
  left_join(data_gdi,,by=c('iso3','Year')) %>%
  left_join(data_gii,by=c('iso3','Year')) %>%
  left_join(data_hdi,by=c('iso3','Year')) %>%
  left_join(marriage_data) %>%
  left_join(contraceptive_data) %>%
  filter(!is.na(gpi_overall)) %>%
  ungroup() %>%
  group_by(iso3) %>%
  mutate(gii_lag=case_when(iso3=="AGO" & is.na(gii_lag)~0.554,
                           iso3=="BTN" & is.na(gii_lag)~0.476,
                           iso3=="CIV" & is.na(gii_lag)~0.639,
                           iso3=="GIN" & is.na(gii_lag)~0.628,
                           iso3=="GMB" & is.na(gii_lag)~0.66,
                           iso3=="GNB" & is.na(gii_lag)~0.63,
                           iso3=="GUY" & is.na(gii_lag)~0.438,
                           iso3=="LBY" & is.na(gii_lag)~0.286,
                           iso3=="MMR" & is.na(gii_lag)~0.496,
                           iso3=="SOM" & is.na(gii_lag)~0.674,
                           iso3=="SWZ" & is.na(gii_lag)~0.543,
                           iso3=="TCD" & is.na(gii_lag)~0.698,
                           TRUE~gii_lag),
         hdi_lag=case_when(iso3=="BTN" & is.na(hdi_lag)~0.582,
                           iso3=="SSD" & is.na(hdi_lag)~0.406,
                           iso3=="SOM" & is.na(hdi_lag)~0.38,
                           TRUE~hdi_lag),
         gdi_lag=case_when(iso3=="BTN" & is.na(gdi_lag) ~ 0.948,
                           iso3=="CIV" & is.na(gdi_lag) ~ 0.864,
                           iso3=="DJI" & is.na(gdi_lag) ~ 0.797,
                           iso3=="ECU" & is.na(gdi_lag) ~ 0.982,
                           iso3=="GIN" & is.na(gdi_lag) ~ 0.825,
                           iso3=="GMB" & is.na(gdi_lag) ~ 0.85,
                           iso3=="GNB" & is.na(gdi_lag) ~ 0.856,
                           iso3=="GUY" & is.na(gdi_lag) ~ 0.984,
                           iso3=="LBY" & is.na(gdi_lag) ~ 0.975,
                           iso3=="MMR" & is.na(gdi_lag) ~ 0.958,
                           iso3=="SOM" & is.na(gdi_lag) ~ 0.769,
                           iso3=="SWZ" & is.na(gdi_lag) ~ 0.962,
                           iso3=="TCD" & is.na(gdi_lag) ~ 0.785,
                           iso3=="TLS" & is.na(gdi_lag) ~ 0.881,
                           TRUE ~ gdi_lag)) %>%
  group_by(iso3) %>%
  fill(gpi_conflict,gpi_safety, .direction = "downup") %>%
  ungroup()


# Impute the GDI based on the available HDI if missing


data_analysis_imp_gdi = select(data_analysis,gdi_lag,Year,hdi_lag,area)
imp_gdi <- mice(data_analysis_imp_gdi, method = "norm.nob", m = 1)
data_sto_gdi <- complete(imp_gdi) 

data_analysis$gdi_lag_imp = data_sto_gdi$gdi_lag*100 #multiple by 100 to rescale it




#### Save final data set for analyses ####

save(data_analysis,file='PAA-Data_files/final_data_set.RData')












