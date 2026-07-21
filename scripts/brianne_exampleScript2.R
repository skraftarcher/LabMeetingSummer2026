# This script demonstrates different ways to organize and arrange
# the example data for summer lab meetings

# created by Brianne Du Clos -- 09 July 2026

# load packages----
library(tidyverse)
library(readxl)

# read in data files----
biod<-read_xlsx(path="odata/labbiodiversity.xlsx",sheet=3)
sal<-read.csv(file = "odata/salinity_data.csv")
site<-read_xlsx(path="odata/sitedeploy.xlsx",sheet=2)
deploy<-read_xlsx(path="odata/sitedeploy.xlsx",sheet=3)

# explore data files----
View(biod)
summary(biod)
# how much of this data has been qaqc'd?
# get a number of missing values from column qaqc
biod |>
  summarise(toCheck = sum(is.na(qaqc))) #724 rows need checking yet
# looking for help online, the examples all use |> instead of %>%
# what is |> and how does it differ from the pipe?? 
# both are pipes, but there's a baseR pipe now that they recommend instead
# for the deets: https://tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/
# also: http://r4ds.hadley.nz/data-transform.html#sec-the-pipe

View(sal)
summary(sal)

# clean and organize data----
b3<- biod |>
  select(-notes) |> # take out notes column; it's full of NAs
  filter(taxaID != "amp-iso-exp") |> # see below; these have funny numbers
  mutate(biomass=dry.weight-tin.weight, # calculate biomass
         biomass=ifelse(biomass<=0,0.001,biomass)) |> # remove negatives
  drop_na() #lastly, remove any rows with missing values (incl. toCheck)
  
# exploratory questions----
# how many of each taxa have been counted?
b3 |>
  group_by(taxaID) |>
  summarise(taxaCount = sum(abundance)) |>
  print(n=100)
# why is amp-iso-exp -495? -- addressed; see below (and above)!

# How many of each taxa have been found at each site?
table(b3$taxaID,b3$site)
# no weird numbers with amp-iso-exp...?
# numbers are also too low to correspond with taxaCount...
# this is ROWS in which these taxa occur, not abundance,
# which is why the numbers are lower (facepalm emoji here)
# reflects number of sampling events each taxa has been found in, NOT
# how many of them were counted at each site

# What site has the highest overall abundance?
b3 |>
  group_by(site) |>
  summarise(siteAb = sum(abundance))

#### exploring amp-iso-exp -- removed after exploration; see above
#bAmpIsoExp <- b3 |>
  #filter(taxaID == "amp-iso-exp") # abundance and weights are -99?

#biodAmpIsoExp <- biod |>
  #filter(taxaID == "amp-iso-exp") # unfiltered data is like that too
# has six more rows removed wih NA filter, all -99s
# I guess this is addressed with biomass filter, but there's no
# weight measures to calculate that with?

#b2AmpIsoExp <- b2 |>
  #filter(taxaID == "amp-iso-exp") #biomass only; 0.001...

# looking back at the summary for biod$abundance, it has a min.
# value of -99! The weight columns do too!
#### maybe take these out for now?

# seems like biomass is the variable of choice for analyses over
# abundance. Try to get at site biomass, unique number of taxaIDs
# (diversity), and salinity?

# lab meeting 7/14----
# how to get at taxa by site?
# first idea:
b4<-b2 |>
  pivot_wider(names_from=site,values_from=mean.biomass2)
# has rows for each unique combo of tray and date, not easy to read
# second idea:
b4<-b2 |>
  pivot_wider(names_from=taxaID,values_from=mean.biomass2)
# has rows...(try this later to confirm)
# fill the NAs with 0s
b4<-b2 |>
  pivot_wider(names_from=taxaID, values_from=mean.biomass2, values_fill=0)

# looking for outliers:
# first ggplot
ggplot(data = biod) + #creates a graph object
  geom_boxplot(aes(y=abundance, fill=taxaID)) #plots abundance for each taxaID
# try again
ggplot(data = biod)+
  geom_boxplot(aes(y=abundance))+
  facet_wrap(~taxaID)
# try again
ggplot(data = biod)+
  geom_boxplot(aes(y=abundance))+
  facet_wrap(~taxaID,scales="free")
# ggplot for specific taxa:
ggplot(data = biod |>
         filter(taxaID %in% c("amp-iso-uni", "shmp-1","poly1")))+
  geom_boxplot(aes(y=abundance))+
  facet_wrap(~taxaID,scales="free")

# shrimp-1 abundance at LUMO3 over time
# with theme to change base aesthetics
theme_set(theme_bw()+theme(panel.grid=element.blank(),
                           axis.text=element.text(size=18),
                           axis.title=element.text(size=18)))

ggplot(data=biod|>
         filter(site=="LUMO3")|>
         filter(taxaID=="shmp-1"))+
  #geom_point(aes(x=date.retrieved, y=abundance)) add this to boxplot later
  geom_boxplot(aes(x=date.retrieved,y=abundance,group=date.retrieved))+
  geom_point(aes(x=date.retrieved, y=abundance)) # can change size and colors of points
  #theme(panel.grid=element.blank(), this is addressed above w/theme_set
        #panel.background=element.blank(),
        #panel.border=element.rect(color="black"))
