library(dplyr)
library(tidyr)

indicator_score = function(v, f, F, N) {
  score = numeric(length(v))
  for (i in 1:length(v)) {
    if (F == 0) {
      score[i] = 100 * v[i]/N
    } else {
      score[i] = 100 * (v[i] - 0.5*(F - f[i]))/N
    }
  }
  score
}

load_data = function() {
  cgrt = read.csv('OxCGRT_latest.csv')
  covid = read.csv('owid-covid-data.csv')
  
  cgrt$Date = as.Date(paste(cgrt$Date), format='%Y%m%d')
  covid$date = as.Date(covid$date, format='%Y-%m-%d')

  
  #
  # Average StringencyIndex vs DPM
  #
  countries.with_regions = (cgrt %>% 
                              filter(RegionName != '') %>% 
                              select(CountryName) %>% 
                              distinct)$CountryName 
  
  cgrt.no_region = cgrt %>% 
    filter(!CountryName %in% countries.with_regions)
  
  # join StringencyIndex with latest DPM
  l1 = cgrt.no_region %>% 
    select(CountryCode, CountryName, Date, StringencyIndex)
  l2 = covid %>% 
    select(continent, iso_code, date, total_deaths_per_million)
  l1l2 = inner_join(l1, l2, by=c('CountryCode' = 'iso_code', 'Date' = 'date'))

  smart_max = function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
  avg_si_vs_dpm = l1l2 %>% group_by(CountryCode) %>%
    summarise(
      continent=first(continent), 
      CountryName=first(CountryName),
      mean_si=mean(StringencyIndex, na.rm=T),
      dpm=smart_max(total_deaths_per_million)
    ) %>% 
    filter(!(is.na(mean_si) | is.na(dpm)))
  
  labels_from_breaks = function(breaks) {
    labels = list()
    for (b in 1:length(breaks)-1) {
      labels[b] = paste(breaks[b],' - ', breaks[b+1]-1)
    }
    labels
  }
  
  si_breaks = seq(0,100,by=10)
  dpm_breaks = seq(0,max(si_vs_dpm$dpm)+500, by=500)
  
  avg_si_vs_dpm$mean_si_group = cut(si_vs_dpm$mean_si, breaks=si_breaks, labels=labels_from_breaks(si_breaks), right=T)
  avg_si_vs_dpm$dpm_group = cut(si_vs_dpm$dpm, breaks=dpm_breaks, labels=labels_from_breaks(dpm_breaks), right=T)
  
  
  #
  # Containment policy scores aggregated by month
  #
  cgrt_monthly = cgrt %>% 
    mutate(
      month = cut(Date, 'months'),
      C1_score = indicator_score(cgrt$C1_School.closing, cgrt$C1_Flag, 1, 3),
      C2_score = indicator_score(cgrt$C2_Workplace.closing, cgrt$C2_Flag, 1, 3),
      C3_score = indicator_score(cgrt$C3_Cancel.public.events, cgrt$C3_Flag, 1, 2),
      C4_score = indicator_score(cgrt$C4_Restrictions.on.gatherings, cgrt$C4_Flag, 1, 4),
      C5_score = indicator_score(cgrt$C5_Close.public.transport, cgrt$C5_Flag, 1, 2),
      C6_score = indicator_score(cgrt$C6_Stay.at.home.requirements, cgrt$C6_Flag, 1, 3),
      C7_score = indicator_score(cgrt$C7_Restrictions.on.internal.movement, cgrt$C7_Flag, 1, 2),
      C8_score = indicator_score(cgrt$C8_International.travel.controls, NA, 0, 4),
    ) %>%
    group_by(CountryName, month) %>%
    summarise(
      month=first(month),
      CountryName = first(CountryName),
      C1 = mean(C1_score, na.rm=T),
      C2 = mean(C2_score, na.rm=T),
      C3 = mean(C3_score, na.rm=T),
      C4 = mean(C4_score, na.rm=T),
      C5 = mean(C5_score, na.rm=T),
      C6 = mean(C6_score, na.rm=T),
      C7 = mean(C7_score, na.rm=T),
      C8 = mean(C8_score, na.rm=T)
    ) %>%
    ungroup()
  
  cgrt_monthly[is.na(cgrt_monthly)] = 0
  
  list(
    cgrt = cgrt,
    covid = covid,
    avg_si_vs_dpm = avg_si_vs_dpm,
    cgrt_monthly = cgrt_monthly
  )
}

data = load_data()
