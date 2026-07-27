# Stephanie's ugly plot script

# load packages
library(tidyverse)
library(ggpattern)
library(readxl)

# read in data
biod<-read_xlsx(path="odata/labbiodiversity.xlsx",sheet=3)

# only keep amp/iso, biv, gast, fsh, crb, and poly

biod$grps<-NA

biod$grps[grep(x=biod$taxaID,pattern="amp")]<-"amp.iso"
biod$grps[grep(x=biod$taxaID,pattern="iso")]<-"amp.iso"
biod$grps[grep(x=biod$taxaID,pattern="gast")]<-"gast"
biod$grps[grep(x=biod$taxaID,pattern="biv")]<-"biv"
biod$grps[grep(x=biod$taxaID,pattern="fsh")]<-"fsh"
biod$grps[grep(x=biod$taxaID,pattern="crb")]<-"crb"
biod$grps[grep(x=biod$taxaID,pattern="poly")]<-"poly"


biod2<-biod%>%
  filter(abundance>0)%>%
  mutate(grps=ifelse(is.na(grps),"oth",grps))%>%
  group_by(date.retrieved,site,grps)%>%
  summarize(abund=sum(abundance,na.rm = T))%>%
  mutate(yr=year(date.retrieved),
         season=case_when(
           month(date.retrieved) %in% c(12,1,2)~"Winter",
           month(date.retrieved) %in% c(3,4,5)~"Spring",
           month(date.retrieved) %in% c(6,7,8)~"Summer",
           month(date.retrieved) %in% c(9,10,11)~"Fall"),
         dep=paste(yr,season),
         img_path=case_when(
           grps=="amp.iso"~"pics/ampiso.jpg",
           grps=="biv"~"pics/biv.jpg",
           grps=="crb"~"pics/crb.jpg",
           grps=="fsh"~"pics/fsh.jpg",
           grps=="gast"~"pics/gast.jpg",
           grps=="oth"~"pics/oth.jpg",
           grps=="poly"~"pics/poly.jpg"
         ))%>%
  ungroup()%>%
  group_by(dep,site)%>%
  mutate(tot.a=sum(abund),
         prop=100*(abund/tot.a))%>%
  select(site,dep,grps,prop,img_path)%>%
  distinct()

biod2$dep.chron<-factor(biod2$dep,c("2023 Winter",
                                    "2023 Spring",
                                    "2023 Summer",
                                    "2023 Fall",
                                    "2024 Winter",
                                    "2024 Spring",
                                    "2024 Summer",
                                    "2024 Fall",
                                    "2025 Winter",
                                    "2025 Spring",
                                    "2025 Summer",
                                    "2025 Fall",
                                    "2026 Winter",
                                    "2026 Spring",
                                    "2026 Summer"))

biod2$dep.seas<-factor(biod2$dep,c("2023 Winter",
                                   "2024 Winter",
                                   "2025 Winter",
                                   "2026 Winter",
                                   "2023 Spring",
                                   "2024 Spring",
                                   "2025 Spring",
                                   "2026 Spring",
                                   "2023 Summer",
                                   "2024 Summer",
                                   "2025 Summer",
                                   "2026 Summer",
                                    "2023 Fall",
                                    "2024 Fall",
                                    "2025 Fall"))


p<-ggplot(biod2%>%
         filter(site!="SL1"), aes(x = dep.chron, y = prop)) +
  geom_col_pattern(
    aes(pattern_filename = img_path),
    pattern = 'image',
    pattern_type="tile",
    pattern_scale = -1,
    fill = 'white',
    colour = 'black'
  ) +
  scale_pattern_filename_discrete(choices = unique(biod2$img_path),
                                  name="Taxa group",
                                  labels = c("amphipods and isopods",
                                             "bivalves",
                                             "crabs",
                                             "fish",
                                             "gastropods",
                                             "other",
                                             "polychaetes")) +
  theme_minimal()+
  facet_wrap(~site,scales="free")+
  ylab("Proportion of taxa collected")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45,color="purple",hjust = 1),
        axis.title = element_text(color="orange"),
        axis.text.y=element_text(color="red"))

ggsave("figures/uglyplot1_chronological.jpg",plot=p,width=10,height=7)

p2<-ggplot(biod2%>%
            filter(site!="SL1"), aes(x = dep.seas, y = prop)) +
  geom_col_pattern(
    aes(pattern_filename = img_path),
    pattern = 'image',
    pattern_type="tile",
    pattern_scale = -1,
    fill = 'white',
    colour = 'black'
  ) +
  scale_pattern_filename_discrete(choices = unique(biod2$img_path),
                                  name="Taxa group",
                                  labels = c("amphipods and isopods",
                                             "bivalves",
                                             "crabs",
                                             "fish",
                                             "gastropods",
                                             "other",
                                             "polychaetes")) +
  theme_minimal()+
  facet_wrap(~site,scales="free")+
  ylab("Proportion of taxa collected")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 45,color="purple",hjust = 1),
        axis.title = element_text(color="orange"),
        axis.text.y=element_text(color="red"))

ggsave("figures/uglyplot1_seasons.jpg",plot=p2,width=10,height=7)

# ggplot(data=biod2%>%
#          filter(site!="SL1"),aes(x=dep,y=prop,fill=grps))+
#   geom_bar(stat="identity")+
#   facet_wrap(~site,scales="free")
#   
# 
