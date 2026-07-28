# practicing pulling in and manipulating data
# Allison Noble
# July 7, 2026

# loading packages----
library(tidyverse)
library(readxl)

# ggplot settings
theme_set(theme_bw() +
          theme(panel.grid = element_blank(),
                axis.text = element_text(size = 14),
                axis.title = element_text(size= 18)))


# read in data
lab_bio<- read_xlsx(path = "odata/labbiodiversity.xlsx", sheet = 3) #excel
sal<- read.csv("odata/salinity_data.csv")

# check data
summary(lab_bio)
summary(sal)

table(lab_bio$site, lab_bio$tray)

# organizing with tidyverse
lab_bio2<- lab_bio %>%
  #filter(site = "LUMO3")
  #filter(site != "SL1")
  #filter(site %in% c("LUMO3", "LUMO6"))
  #filter(!site %in% c("LUMO3", "LUMO6"))
  filter(date.retrieved >= "2025-01-01") %>%
  mutate(dry.biomass = dry.weight - tin.weight,
         dry.biomass = ifelse(dry.biomass <= 0, 0.001, dry.biomass),
         site.type = case_when(
           site %in% c("LUMO3", "LUMO6") ~ "LUMCON sites",
           site %in% c("TB1", "BB1") ~ "Outer estuary sites",
           site %in% c("BB2", "BB3") ~ "Barataria sites",
           site == "SL1" ~ "failed site"),
         mean.biomass = mean(dry.biomass, na.rm = T)) %>%
  group_by(site, site.type, tray, date.retrieved, taxaID) %>%
  #mutate(mean.biomass2 = mean(dry.biomass, na.rm = T))
  summarize(mean.biomass2 = mean(dry.biomass, na.rm = T))

summary(lab_bio2$mean.biomass2)

unique(lab_bio2$site.type)

# 7/14/2026
lab_bio3<- lab_bio2 %>%
  pivot_wider(names_from = taxaID, values_from = mean.biomass2, values_fill = 0)

# looking for outliers
ggplot(lab_bio %>%
         filter(taxaID %in% c("shmp-1", "poly-1", "amp-iso-uni"))) + #creates graph object
  geom_boxplot(aes(y = abundance,  x = taxaID)) +
  facet_wrap(~taxaID, scales = "free")
  
  
# shmp-1 abundance at LUMO3 over time
ggplot(lab_bio %>%
         filter(site == "LUMO3") %>%
         filter(taxaID == "shmp-1")) +
  geom_boxplot(aes(x = date.retrieved, y = abundance, group = date.retrieved)) +
  geom_point(aes(x = date.retrieved, y = abundance))
  #geom_line(aes(x = date.retrieved, y = abundance, group = tray))

# joining two datasets - salinity data w/ biodiversity data 
colnames(lab_bio)
colnames(sal)
#salinity data on hourly level, biodiversity has one date per site
ggplot(data = sal) +
  geom_point(aes(x = date, y = PSU, group = site, color = site))

#make dataset from biodiversity data for site, start, and end date/times
deploy<- lab_bio %>%
  select(site, end = date.retrieved) %>%
  distinct() %>%
  mutate(start = end - days(x = 14),
         start = ymd_hms(paste(start, "18:00:00")), 
         end = ymd_hms(paste(end, "8:00:00")),
         date.int = interval(start, end),
         deploy = paste0(site, ".", row.names(.)))

# intervals for salinity data
sal2<- sal %>%
  separate(date, into = c("date", "time"), sep= " ") %>%
  mutate(time = ifelse(is.na(time), "00:00:00", time), 
         date.time = ymd_hms(paste(date, time))) %>%
  select(-date, -time)

sal2$deploy<- "x"

# cut salinity data to deployments
for(i in 1:nrow(sal2)){
  t1<- sal2$date.time[i]
  dep2<- filter(deploy, site == sal2$site[i])
  t2<- dep2$deploy[t1 %within% dep2$date.int]
  if(length(t2) > 0) {
    sal2$deploy[i]<- t2
  }
}

# summarize salinity data
sal3<- sal2 %>%
  filter(deploy != "x") %>%
  group_by(deploy) %>%
  summarize(mean.sal = mean(PSU, na.rm = T),
            median.sal = median(PSU, na.rm = T),
            sd.sal = sd(PSU, na.rm = T),
            min.sal = min(PSU, na.rm = T),
            max.sal = max(PSU, na.rm = T))

dep.sal<- left_join(deploy, sal3) %>%
  mutate(date.retrieved = ymd(paste(year(end), month(end), day(end)))) %>%
  select(-start, -date.int, -deploy, -end)

bio_wsal<- left_join(lab_bio, dep.sal)
  

