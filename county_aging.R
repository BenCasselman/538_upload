##########################################################################################
#
# Quick look at 2014 Census population estimates to look at county-level age trends.
# Data released 6/25/15. Downloadable here: http://www.census.gov/popest/data/counties/asrh/2014/CC-EST2014-ALLDATA.html
# Codebook here: http://www.census.gov/popest/data/counties/asrh/2014/files/CC-EST2014-ALLDATA.pdf
# All analysis uses county-level data.
# This is just basic framework for analysis in this FiveThirtyEight story: http://fivethirtyeight.com/datalab/some-parts-of-america-are-aging-much-faster-than-others/
# Published maps built on different framework by Ritchie King.

require(readr)
require(dplyr)
require(magrittr)
require(tidyr)
require(ggplot2)
require(ggvis)

# Download data
newdata <- read_csv("http://www.census.gov/popest/data/counties/asrh/2014/files/CC-EST2014-ALLDATA.csv")
# Create fips code variable
newdata <- newdata %>%
  mutate(fips = paste0(STATE,sprintf("%03d",COUNTY)))

# Split these into more manageable data frames. We'll drop 1 and 2 in the YEAR variable (which are other 2010 estimates)
all <- newdata %>%
  select(STATE,COUNTY,STNAME,CTYNAME,YEAR,AGEGRP,TOT_POP,fips) %>%
  filter(YEAR>=3)
men <- newdata %>%
  select(STATE,COUNTY,STNAME,CTYNAME,YEAR,AGEGRP,TOT_MALE,fips) %>%
  filter(YEAR>=3)
women <- newdata %>%
  select(STATE,COUNTY,STNAME,CTYNAME,YEAR,AGEGRP,TOT_FEMALE,fips) %>%
  filter(YEAR>=3)

# Create variables for young people (under 20), seniors (65+), working-age (20-64) and prime age (25-54).
all <- all %>%
  spread(key=AGEGRP,value=TOT_POP) %>%
  rename(Total = `0`) %>% rowwise() %>%
  mutate(minor=sum(`1`,`2`,`3`,`4`)/Total,senior=sum(`14`,`15`,`16`,`17`,`18`)/Total,
         working=sum(`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`)/Total,prime=sum(`6`,`7`,`8`,`9`,`10`,`11`)/Total) %>%
  select(STATE,COUNTY,fips,STNAME,CTYNAME,YEAR,Total,minor,senior,working,prime)
men <- men %>%
  spread(key=AGEGRP,value=TOT_MALE) %>%
  rename(Total = `0`) %>% rowwise() %>%
  mutate(minor=sum(`1`,`2`,`3`,`4`)/Total,senior=sum(`14`,`15`,`16`,`17`,`18`)/Total,
         working=sum(`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`)/Total,prime=sum(`6`,`7`,`8`,`9`,`10`,`11`)/Total) %>%
  select(STATE,COUNTY,fips,STNAME,CTYNAME,YEAR,Total,minor,senior,working,prime)
women <- women %>%
  spread(key=AGEGRP,value=TOT_FEMALE) %>%
  rename(Total = `0`) %>% rowwise() %>%
  mutate(minor=sum(`1`,`2`,`3`,`4`)/Total,senior=sum(`14`,`15`,`16`,`17`,`18`)/Total,
         working=sum(`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`)/Total,prime=sum(`6`,`7`,`8`,`9`,`10`,`11`)/Total) %>%
  select(STATE,COUNTY,fips,STNAME,CTYNAME,YEAR,Total,minor,senior,working,prime)

# Create single-year files
allrace.2014 <- all %>%
  filter(YEAR==7)
allrace.2010 <- all %>%
  filter(YEAR==3)

# For simplicity, create a separate df for change since 2010
change <- allrace.2010 %>%
  select(STATE,COUNTY,Total,minor,senior,working,prime) %>%
  left_join(allrace.2014,.,by=c("STATE","COUNTY"))
change <- change %>%
  mutate(overall=Total.x/Total.y -1,minor=minor.x - minor.y,senior=senior.x-senior.y,working=working.x-working.y,prime=prime.x-prime.y) %>%
  select(STATE,COUNTY,fips,STNAME,CTYNAME,Total.x,overall,minor,senior,working,prime)

# Explore data with scatters -- change up variables
plot <- change %>%
  select(fips,senior,minor,working,prime) %>%
  inner_join(allrace.2014,.,by="fips")

tooltip <- function(x){
  if(is.null(x)) return(NULL)
  row <- change[change$fips==x$fips,]
  paste0(row$CTYNAME,", ",row$STNAME)
}

# Couple examples:

# Aging (change in share of pop that's 65+) vs "oldness" (share of pop that's 65+):
plot %>%
  ggvis(x=~senior.x,y=~senior.y,key:=~fips) %>%
  layer_points() %>%
  add_tooltip(tooltip,"hover") %>%
  add_axis("x",title="Share 65+") %>%
  add_axis("y",title="Change in share 65+")

# Age of pop (share 65+) vs total population:
plot %>%
  ggvis(x=~Total,y=~senior.x) %>%
  layer_points(key:=~fips) %>%
  scale_numeric("x",trans="log",expand=0) %>%
  add_tooltip(tooltip,"hover") %>%
  add_axis("x",title="County population (log scale)") %>%
  add_axis("y",title="Senior share of population")

# Explore with maps
# Here's an example for share of population that's 65+
data(county.fips)

map <- allrace.2014

# Construct buckets
buckets <- quantile(map$senior,probs=seq(0,1,by=0.2))
map$color.senior <- as.numeric(cut(map$senior,buckets,include.lowest=TRUE))
colors <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c") # http://colorbrewer2.org/?type=sequential&scheme=Blues&n=5

#This is how you match up the intervals and color schema to county FIPS codes
colorsmatched <- map$color.senior[match(county.fips$fips, map$fips)]

#We draw a map and use a Polyconic projection with a transparent background
map("county", col = colors[colorsmatched], resolution = 0,lty = 0, fill=TRUE, projection = "polyconic", bg = "transparent")

#Here we're putting on top another county map that's blank but has thin white lines
map("county", col="grey", fill=FALSE, add=TRUE, lty=1, lwd=.00000001, projection="polyconic", bg="transparent")
#Here we throw on a state map with white colored borders
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 2, projection="polyconic", bg = "transparent")
title("Share of Population Age 65+, 2013")
legend("bottomleft",legend = sprintf("%.1f %%", 100*buckets[2:6]), fill=colors, bty="n")

