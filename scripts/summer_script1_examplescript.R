#summerz - created 6/30/26
#loading tidyverse package
library(tidyverse)
#example objects
x<-1
x1<-c(1,2,3,4,5)
x2<-c("1",2,3,4,5)
#making data frame
x3<-data.frame(var1=seq(1,10,1), var2=("a"),var3=c(1.2,3.4))

#Installing read package
#library(readxl)

biod<-read_xlsx(path = "odata/labbiodiversitY.XLSX",sheet=3)
sal<-read.csv(file="odata/salinity_data.csv")

#look at the data
summary(biod)
table(biod$site, biod$tray)

#playing with organizing data
b2<-biod%>%
  #filter(site=="LUMO3") #equals
  #filter(site!="LUMO3) #does not equal
  #filter(site%in% c("LUMO3","TB1")) #site is in this list of values
  #filter(!site %in% c("LUMO3","TB1")) #site is not in this list of values
  #filter(abundance>10) #is greater than
  #filter(abundance<10) #is less than
  filter(date.retrieved >="2025-01-01")%>% #is greater than or equal to
  mutate(biomass=dry.weight- tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass),
         site.type=case_when(
           site %in% c("LUMO3","LUMO6")~"LUMCON sites",
           site %in% c("TB1","BB1")~"Outer estuary sites",
           site %in% c("BB2","BB3")~"Barataria sites",
           site == "SL1"~"failed site"),
         mean.biomass=mean(biomass,na.rm=T))%>% #we're going to create new variables
  group_by(site,tray,date.retrieved,taxaID)%>%
  #mutate(mean.biomass2=mean(biomass,na.rm=T))
  summarise(mean.biomass2=mean(biomass,na.rm=T))


#looking for outliers
ggplot(data = biod%>%
         filter(taxaID %in% c("shmp-1","poly-1","amp-iso-uni")))+
  geom_boxplot(aes(y=abundance))+
  facet_wrap(~taxaID,scales="free")#creates a graph object

ggplot(data=biod%>%
         filter(site=="LUMO3")%>%
         filter(taxaID=="shmp-1"))+
  geom_boxplot(aes(x=date.retrieved,y=abundance,group = date.retrieved))+
  geom_point(aes(x=date.retrieved,y=abundance),color="red",size=1)+
  geom_line(aes(x=date.retrieved,y=abundance,group = tray))


#playing with shrimpies
shrimp <- biod %>%
  filter(taxaID == "shmp-1")%>%

  # Create a new column called 'month'
  # format to the abbreviated month (Jan, Feb, Mar)
  mutate(month = format(date.retrieved, "%b")) %>%
  
  # Group by site and month so we can summarize abundance for each
  group_by(site, month) %>%

  
  # add abundance across all trays for that site and month
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  
  # Ungroup to avoid accidental grouping later
  ungroup()

# Plot shrimp seasonal abundance
ggplot(shrimp, aes(x = month, y = total_abundance, color = site, group = site)) +
  geom_line() +        # Draw lines connecting months
  geom_point() +       # Add points for each month
  labs(title = "Seasonal Abundance of shmp-1 (Shrimp)",
       x = "Month",
       y = "Total Abundance")

#playing with crabs
crab <- biod %>%
  filter(taxaID == "crb-1")%>%
  
  # Create a new column called 'month'
  # format it to the abbreviated month (Jan, Feb, Mar)
  mutate(month = format(date.retrieved, "%b")) %>%
  
  # Group by site and month so we can summarize abundance for each
  group_by(site, month) %>%
  
  
  # add abundance across all trays for that site and month
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  
  # Ungroup to avoid accidental grouping later
  ungroup()

# Plot crab seasonal abundance
ggplot(crab, aes(x = month, y = total_abundance, color = site, group = site)) +
  geom_line() +        # Draw lines connecting months
  geom_point() +       # Add points for each month
  labs(title = "Seasonal Abundance of crb-1 (crab)",
       x = "Month",
       y = "Total Abundance")


# already created two tables:
# 'shrimp' = abundance per site + month for shmp-1
# 'crab'   = abundance per site + month for crb-1

# rename columns for after joining
shrimp_crab <- shrimp %>%
  
  # Rename shrimp abundance column so we know which species
  rename(shrimp_abundance = total_abundance) %>%
  
  # Join shrimp and crab tables together by site and month
  # aligns shrimp and crab abundance for the SAME site and SAME month
  inner_join(
    crab %>% rename(crab_abundance = total_abundance),
    by = c("site", "month")
  )

ggplot(shrimp_crab, aes(
  x = shrimp_abundance,     # shrimp abundance on x-axis
  y = crab_abundance,       # crab abundance on y-axis
  color = site              # color points by site
)) +
  
  geom_point() +            # show each site-month as a point
  
  geom_smooth(
    method = "lm",          # add a linear regression line
    se = FALSE              # turn off shading around the line
  ) +
  
  facet_wrap(~ site) +      # make a separate panel for each site
  labs(
    title = "Shrimp vs Crab Abundance Correlation",
    x = "Shrimp Abundance (shmp-1)",
    y = "Crab Abundance (crb-1)"
  )


shrimp_crab %>%
  
  # We analyze each site separately because combining sites
  
  group_by(site) %>%
  
  summarise(
    
    # -----------------------------
    # CORRELATION COEFFICIENT (r)
    # -----------------------------
    # cor() calculates how strongly shrimp and crab abundance
    # rise and fall together.
    #
    # r close to +1  → they rise together (positive correlation)
    # r close to -1  → one rises when the other falls (negative correlation)
    # r close to 0   → no relationship
    #
    # cor() will fail if a site has too few paired observations
    # (for example, only 1 or 2 months where BOTH shrimp and crabs were present).
    #
    # tryCatch() prevents the entire summarise() from crashing.
    # If cor() errors, we return NA instead.
    correlation = tryCatch(
      cor(
        shrimp_abundance,      # shrimp values for this site
        crab_abundance,        # crab values for this site
        method = "pearson"     # Pearson correlation
      ),
      error = function(e) NA_real_   # If error, return NA
    ),
    
    
    # -----------------------------
    # P-VALUE (significance test)
    # -----------------------------
    # cor.test() gives a p-value that tells you whether the correlation
    # is statistically significant.
    #
    # p < 0.05 → significant relationship
    # p > 0.05 → not significant
    #
    # cor.test() *requires* at least 3 paired observations.
    # If a site has too few data points, cor.test() will error.
    #
    # tryCatch() prevents crashing and returns NA instead.
    p_value = tryCatch(
      cor.test(
        shrimp_abundance,      # shrimp values for this site
        crab_abundance         # crab values for this site
      )$p.value,
      error = function(e) NA_real_   # If error, return NA
    )
  )

