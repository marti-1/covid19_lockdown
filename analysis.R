library(dplyr)

cgrt = read.csv('OxCGRT_latest.csv')
covid = read.csv('owid-covid-data.csv')

cgrt$Date = as.Date(paste(cgrt$Date), format='%Y%m%d')
covid$date = as.Date(covid$date, format='%Y-%m-%d')

countries.with_regions = (cgrt %>% 
                            filter(RegionName != '') %>% 
                            select(CountryName) %>% 
                            distinct)$CountryName 

cgrt.no_region = cgrt %>% 
  filter(!CountryName %in% countries.with_regions)

# join StringencyIndex with total deaths
l1 = cgrt.no_region %>% 
  select(CountryCode, CountryName, Date, StringencyIndex)
l2 = covid %>% 
  select(continent, iso_code, date, total_deaths_per_million)
l1l2 = inner_join(l1, l2, by=c('CountryCode' = 'iso_code', 'Date' = 'date'))


smart_max = function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
si_vs_dpm = l1l2 %>% group_by(CountryCode) %>%
  summarise(
    continent=first(continent), 
    CountryName=first(CountryName),
    mean_si=mean(StringencyIndex, na.rm=T),
    dpm=smart_max(total_deaths_per_million)
  ) %>% 
  filter(!(is.na(mean_si) | is.na(dpm)))
si_vs_dpm

labels_from_breaks = function(breaks) {
  labels = list()
  for (b in 1:length(breaks)-1) {
    labels[b] = paste(breaks[b],' - ', breaks[b+1]-1)
  }
  labels
}

si_breaks = seq(0,100,by=10)
dpm_breaks = seq(0,max(si_vs_dpm$dpm)+500, by=500)

si_vs_dpm$mean_si_group = cut(si_vs_dpm$mean_si, breaks=si_breaks, labels=labels_from_breaks(si_breaks), right=T)
si_vs_dpm$dpm_group = cut(si_vs_dpm$dpm, breaks=dpm_breaks, labels=labels_from_breaks(dpm_breaks), right=T)

library(ggplot2)
library(ggmosaic)

stringency_vs_dpm_plot = function(data, title) {
  p = ggplot(data=data) +
    geom_mosaic(aes(x=product(dpm_group,mean_si_group),fill=dpm_group)) +
    scale_fill_brewer(palette = "RdYlGn", direction=-1, name='DPM') +
    xlab('Avg(StringencyIndex)') +
    ylab('DPM')+
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )
  if (!missing(title)) {
    p = p + labs(title = title)
  }
  p
}

stringency_vs_dpm_plot(si_vs_dpm, title='Avg(StringencyIndex) ~ DPM')

stringency_vs_dpm_plot(si_vs_dpm %>% filter(continent=='Europe'), title='Avg(StringencyIndex) ~ DPM | Europe')
stringency_vs_dpm_plot(si_vs_dpm %>% filter(continent=='Asia'), title='Avg(StringencyIndex) ~ DPM | Asia')
stringency_vs_dpm_plot(si_vs_dpm %>% filter(continent=='Africa'), title='Avg(StringencyIndex) ~ DPM | Africa')
stringency_vs_dpm_plot(si_vs_dpm %>% filter(continent=='North America'), title='Avg(StringencyIndex) ~ DPM | North America')
stringency_vs_dpm_plot(si_vs_dpm %>% filter(continent=='South America'), title='Avg(StringencyIndex) ~ DPM | South America')
