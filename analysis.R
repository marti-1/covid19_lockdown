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
library(patchwork)

stringency_vs_dpm_plot = function(data, title) {
  p = ggplot(data=data) +
    geom_mosaic(aes(x=product(dpm_group,mean_si_group),fill=dpm_group)) +
    scale_fill_brewer(palette = "RdYlGn", direction=-1, name='DPM') +
    xlab('Avg(StringencyIndex)') +
    ylab('DPM')+
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle=90)
    )
  if (!missing(title)) {
    p = p + labs(title = title)
  }
  p
}

stringency_vs_dpm_plot(si_vs_dpm, title='Avg(StringencyIndex) ~ DPM')

p1 = stringency_vs_dpm_plot(si_vs_dpm %>% 
                              filter(continent=='Europe'), title='Avg(StringencyIndex) ~ DPM | Europe')
p2 = stringency_vs_dpm_plot(si_vs_dpm %>% 
                              filter(continent=='Asia'), title='Avg(StringencyIndex) ~ DPM | Asia')
p3 = stringency_vs_dpm_plot(si_vs_dpm %>% 
                              filter(continent=='Africa'), title='Avg(StringencyIndex) ~ DPM | Africa')
p4 = stringency_vs_dpm_plot(si_vs_dpm %>% 
                              filter(continent=='North America'), title='Avg(StringencyIndex) ~ DPM | North America')
p5 = stringency_vs_dpm_plot(si_vs_dpm %>% 
                              filter(continent=='South America'), title='Avg(StringencyIndex) ~ DPM | South America')

combined <- p1 / (p2+p3) / (p4 + p5) & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# which countries in the world had least stringent <=45 quarantines
least_stringent = si_vs_dpm %>% filter(mean_si <= 45) %>% arrange(continent)
least_stringent

# heatmap of C_X vs date
indicator_score = function(v, f, F, N) {
  score = numeric(nrow(v))
  for (i in 1:nrow(v)) {
    if (F == 0) {
      score[i] = 100 * v[i]/N
    } else {
      score[i] = 100 * (v[i] - 0.5*(F - f[i]))/N
    }
  }
  score
}

cgrt = cgrt %>% mutate(
  C1_score = indicator_score(cgrt$C1_School.closing, cgrt$C1_Flag, 1, 3),
  C2_score = indicator_score(cgrt$C2_Workplace.closing, cgrt$C2_Flag, 1, 3),
  C3_score = indicator_score(cgrt$C3_Cancel.public.events, cgrt$C3_Flag, 1, 2),
  C4_score = indicator_score(cgrt$C4_Restrictions.on.gatherings, cgrt$C4_Flag, 1, 4),
  C5_score = indicator_score(cgrt$C5_Close.public.transport, cgrt$C5_Flag, 1, 2),
  C6_score = indicator_score(cgrt$C6_Stay.at.home.requirements, cgrt$C6_Flag, 1, 3),
  C7_score = indicator_score(cgrt$C7_Restrictions.on.internal.movement, cgrt$C7_Flag, 1, 2),
  C8_score = indicator_score(cgrt$C8_International.travel.controls, NA, 0, 4),
)

cgrt = cgrt %>% mutate(month = cut(Date, 'months'))

cgrt_monthly = cgrt %>% 
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

restrictions_heatmap = function(df, country) {
  df_monthly_long = df %>% 
    filter(CountryName == country) %>%
    select(!CountryName) %>%
    pivot_longer(!month, names_to = 'policy', values_to = 'stringency')
  
  ggplot(df_monthly_long, aes(x=policy, y=factor(month, levels=rev(levels(month))), fill=stringency)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdYlGn") +
    labs(title=country)+
    ylab('')
}

restrictions_heatmap(cgrt_monthly, 'Latvia') +
  scale_x_discrete(labels=c('Schools', 'Workplace', 'Pub. Events', 'Gatherings','Pub. Transport', 'Stay home','Int move', 'Ext move'))

# neighbours
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

neighbours <- ne_countries(scale = "medium", 
                           country=c('Lithuania', 'Latvia', 'Estonia', 'Belarus', 'Poland'), 
                           returnclass = "sf")

neighbours_si_vs_dpm = inner_join(neighbours, si_vs_dpm, by=c('name' = 'CountryName'))

p1 = ggplot(data = neighbours_si_vs_dpm) +
  geom_sf(aes(fill=dpm)) +
  labs(title='Deaths per million')

p2 = ggplot(data = neighbours_si_vs_dpm) +
  geom_sf(aes(fill=mean_si)) +
  labs(title='Avg(Stringency)')

p1 + p2
