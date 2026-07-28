# script to learn how to pull in data and reorganize it

# lab meeting 7/7/2026

# load packages
library(tidyverse)
#install.packages("readxl")
library(readxl)

theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.text = element_text(size=14),
                           axis.title = element_text(size=18)))

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
  

b3<-b2%>%
  pivot_wider(names_from=site,values_from=mean.biomass2,values_fill = 0)


# looking for outliers
ggplot(data=biod%>%
         filter(taxaID %in% c("shmp-1","poly-1","amp-iso-uni")))+#creates a graph object
  geom_boxplot(aes(y=abundance))+
  facet_wrap(~taxaID,scales="free")

# shrimp-1 abundance at LUMO3 over time

ggplot(data=biod%>%
         filter(site=="LUMO3")%>%
         filter(taxaID=="shmp-1"))+
  geom_boxplot(aes(x=date.retrieved,y=abundance,group=date.retrieved))+
  geom_point(aes(x=date.retrieved,y=abundance),color="red",size=6)+
  geom_line(aes(x=date.retrieved,y=abundance,group=tray))

# joining salinity data to biodiversity 
# first you have to organize the salinity data

# look at the structure of the salinity data
ggplot(data=sal)+
  geom_line(aes(x=date,y=PSU,group=site))+
  facet_wrap(~site)


# make a dataset from biodiversity data to get site, start, and end date/times
deploys<-biod%>%
  select(site,end=date.retrieved)%>%
  distinct()%>%
  mutate(start=end-days(x=14),
         start=ymd_hms(paste(start,"18:00:00")),
         end=ymd_hms(paste(end,"8:00:00")),
         date.int=interval(start,end),
         deploy=paste0(site,".",row.names(.)))


# find intervals the salinity date goes with
# first thing we have to do is turn date into a date/time object
sal2<-sal%>%
  separate(date,into=c("date","time"),sep=" ")%>%
  mutate(time=ifelse(is.na(time),"00:00:00",time),
         date.time=ymd_hms(paste(date,time)))%>%
  select(-date,-time)

sal2$deploy<-NA


# now figure out how to link the intervals in deploys
for(i in 1:nrow(sal2)){
  t1<-sal2$date.time[i]
  dep2<-filter(deploys,site==sal2$site[i])
  t2<-dep2$deploy[t1 %within% dep2$date.int]
  if(length(t2)>0){
    sal2$deploy[i]<-t2
  }
  print(i)
}


# summarize salinity data 
sal3<-sal2%>%
  filter(!is.na(deploy))%>%
  group_by(deploy)%>%
  summarize(mean.sal=mean(PSU,na.rm=T),
            median.sal=median(PSU,na.rm=T),
            sd.sal=sd(PSU,na.rm=T),
            min.sal=min(PSU,na.rm=T),
            max.sal=max(PSU,na.rm = T))

dep.sal<-left_join(deploys,sal3)%>%
  mutate(date.retrieved=ymd(paste(year(end),month(end),day(end))))%>%
  select(-start,-date.int,-deploy,-end)


biod.withenv<-left_join(biod,dep.sal)

# example ugly plot
ggplot(data=biod%>%mutate(x2=row.names(.)))+
  geom_point(aes(x=x2,y=wet.weight,color=site,alpha=date.retrieved),size=4)+
  scale_y_continuous(n.breaks=100)
