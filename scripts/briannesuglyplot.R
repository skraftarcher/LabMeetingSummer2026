# Brianne's ugly plots -- 22 July 2026

# ideas:
# boxplot of abundance by site, filled with tray
# something with taxaID and weight?
# weight over time?
# incorporate salinity somehow?

# load in packages
library(tidyverse)
library(readxl)

# read in data
biod<-read_xlsx(path="odata/labbiodiversity.xlsx",sheet=3)
sal<-read.csv(file = "odata/salinity_data.csv")

# check out data files
summary(biod)
summary(sal)
head(sal)

# make some plots:
# boxplot of abundance by site
ggplot(data = biod)+
  geom_boxplot(aes(x=site, y=abundance))
# of course the lower bound is going to be 0; this is count data
# not very helpful

# barplot of abundance
ggplot(data = biod)+
  geom_bar(aes(y=abundance))
# I don't know what I was going for here; this is weird.
# Also I want a column plot...

# like this! column plot of abundance by site:
ggplot(data = biod)+
  geom_col(aes(x=site, y=abundance))

# the above plot, but with tray counts distinct colors across sites:
ggplot(data = biod)+
  geom_col(aes(x=site, y=abundance, fill = tray))

# boxplot of wet weight by taxa ID
ggplot(data = biod)+
  geom_boxplot(aes(x=taxaID, y=wet.weight))
# yikes. need the right weight measurement (dry-tin, right?), 
# and definitely subsets of taxa, for this to make any sense

#summarize weight over time by site?
# get biomass measurements, drop the -99s
biod2 <- biod |>
  filter(taxaID!="amp-iso-exp") |>
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))
summary(biod2)
# boxplot of biomass by taxa ID
ggplot(data = biod2)+
  geom_boxplot(aes(x=taxaID, y=biomass))
# a little better...

# group data by site, then get mean weight by site
biod3 <- biod2 |>
  filter(!is.na(biomass)) |>
  group_by(site) |>
  summarise(mean.wt = mean(biomass)) 
# this gets average weight over sites, all times!
# only 7 entries, one per site

# biomass by site...  
ggplot(data = biod2)+
  geom_violin(aes(x=site, y=biomass))

ggplot(data = biod2)+
  geom_histogram(aes(biomass, fill = site))

# group weights by taxaID?  
biod4 <- biod2 |>
  filter(!is.na(biomass)) |>
  group_by(taxaID) |>
  summarise(mean.wt = mean(biomass)) 

summary(biod4)

ggplot(data = biod4)+
  geom_bar(aes(y=mean.wt)) # too many entries btw 0 and 1; can't put down bars

# code modified slightly from https://r-graph-gallery.com/301-custom-lollipop-chart.html#horiz
ggplot(biod4, aes(x=taxaID, y=mean.wt))+
  geom_segment(aes(x=taxaID, xend=taxaID, y=mean.wt, yend=mean.wt), color="orange", linewidth = 8) +
  geom_point(color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) # dots show, but not lines, and I'm not sure why

# scatterplot of abundance and salinity?
# still playing with weight
ggplot(biod2)+
  geom_point(aes(x=abundance, y=biomass, shape=site))
ggplot(biod2)+
  geom_point(aes(x=abundance, y=biomass, color=site))

# but really, try to get salinity in???
