# practicing pulling in and manipulating data
# Allison Noble
# July 7, 2026

# loading packages----
library(tidyverse)
library(readxl)

# read in data
lab_bio<- read_xlsx(path = "odata/labbiodiversity.xlsx", sheet = 3) #excel
sal<- read.csv("odata/salinity_data.csv")

# check data
summary(lab_bio)
summary(sal)

table(lab_bio$site, lab_bio$tray)

# organizing with tidyverse
lab_bio2<-  lab_bio %>%
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
  group_by(site, tray, date.retrieved, taxaID) %>%
  #mutate(mean.biomass2 = mean(dry.biomass, na.rm = T))
  summarize(mean.biomass2 = mean(dry.biomass, na.rm = T))

summary(lab_bio2$dry.biomass)

unique(lab_bio2$site.type)


