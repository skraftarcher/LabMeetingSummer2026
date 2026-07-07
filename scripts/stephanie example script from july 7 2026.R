# script to learn how to pull in data and reorganize it

# lab meeting 7/7/2026

# load packages
library(tidyverse)
#install.packages("readxl")
library(readxl)

#read in data
biod<-read_xlsx(path="odata/labbiodiversity.xlsx",sheet=3)
sal<-read.csv(file = "odata/salinity_data.csv")


summary(biod)
table(biod$site,biod$tray)

# playing with organizing data
b2<-biod%>%
  # filter(site=="LUMO3")#equals
  # filter(site!="LUMO3")# does not equal
  # filter(site %in% c("LUMO3","TB1"))# site is in this list of values
  # filter(!site %in% c("LUMO3","TB1"))# site is not in this list
  # filter(abundance>10)# is greater than
  # filter(abundance<10) # is less than
  filter(date.retrieved >="2025-01-01")%>% # is greater than or equal to
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass),
         site.type=case_when(
           site %in% c("LUMO3","LUMO6")~"LUMCON sites",
           site %in% c("TB1","BB1")~"Outer estuary sites",
           site %in% c("BB2","BB3")~"Barataria sites",
           site =="SL1"~"failed site"),
         mean.biomass=mean(biomass,na.rm=T))%>%# we're going to create new variables 
  group_by(site,tray,date.retrieved,taxaID)%>%
  # mutate(mean.biomass2=mean(biomass,na.rm=T))
  summarise(mean.biomass2=mean(biomass,na.rm=T))
  




unique(b2$site.type)
table(b2$site,b2$site.type)
