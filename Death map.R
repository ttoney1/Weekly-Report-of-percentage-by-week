#Program to make Deaths for last MMWR week state map
#One can run this code on Weds when new death data is released. Check
#end date in End Date Column. End Date should be most recent Saturday.

# *********         DIRECTIONS TO RUN CODE: ***********

#One should run code on or after Wednesday when new data is released
#Download raw death data from below link. Note API version excludes weekly.
#https://data.cdc.gov/NCHS/Provisional-COVID-19-death-counts-rates-and-percen/mpx5-t7tu
#Save to raw data folder using same file naming convention
#as previous versions and use Wednesday for the dates.
#Update dates below to prior Wednesday, current Wednesday, and 
#most recent Saturday. 
#Run code.
#Map and table will both output to map folder.
#Text you need to enter on slide will be on same page as map.
#Snip map and legend only; type all text directly into slide.

#UPDATE DATES TO PRIOR WEDNESDAY, CURRENT WEDNESDAY, & MOST RECENT SATURDAY
prior.week<-'05-31-2023'
new.week<-'06-07-2023'
most.recent.Saturday<-('06-03-2023')

#************************************************************************
#BACKGROUND:

# In raw data file, COVID death counts of 1-9 are suppressed 
#as indicated by blank / missing values in the COVID Death column. 
#Counts of zero deaths are NOT suppressed since zero deaths indicate
#no one died in that jurisdiction, so there is no risk to anonymity.
#If deaths were suppressed one week on CDT for a jurisdiction, 
#e.g. California, but shows death counts greater than 9 in the new data,
#for that same week, then it indicates the death count changed.

#NOTE.The CDT file and the file used for this code (same file) groups 
#deaths by jurisdiction of residence, whereas the other surveillance file 
#(which gets updated daily on the NVSS COVID-19 page), linked here:
#https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-by-Week-Ending-D/r8kw-7aab
#is grouped by jurisdiction of occurrence.

#Link to NCHS death raw data file used in this code:
#https://data.cdc.gov/NCHS/Provisional-COVID-19-death-counts-rates-and-percen/mpx5-t7tu

#State-level map from COVID Data Tracker to use as guide / or use COVID Update:
#https://covid.cdc.gov/covid-data-tracker/#cases_percent-covid-deaths

#To find hexa color picker online. Upload your image at below link:
#https://imagecolorpicker.com/en

#Links to census Shape Files if they do not download. To read in shape files,
#download the zip files. Use function for example read_sf('data/county_5m.shp') 
#county: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#state: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

##################################################################

#LOAD PACKAGES

library(tigris) #loads census Shape files and shifts PR, HI, AK in map
#Below to cache census data instead of re-downloading if error
options(tigris_use_cache = TRUE)
library(ggrepel) # for map labels will not overlap
library(shadowtext) # for map label shadow
library(gifski)
library(dplyr) #to use rename
library(rstudioapi) #to use dirname
library(readr) #read_csv
library(usmap) #generates maps, but can also use ggplot2
library(data.table)
library(tidyverse)
library(ggtext)
library(readxl)
library(cowplot) #ggdraw and theme_nothing() 
library(grid) #grid.draw
library(scales) #use show_col
library(sf)  # if want to use 'coord_sf'

#-------------------------------------------------------

#SET UP DIRECTORIES

#set working directory to any current location:
current.dir <- dirname(getActiveDocumentContext()$path)
setwd(current.dir)
getwd()

#current date vector
today.date<-Sys.Date()
# convert from default format to no dashes
today.date<-format(today.date,'%Y%m%d')
#today.date<-format(today.date,'%Y%m%d','%b%d%Y')

user <- Sys.getenv('USERNAME')

#---------------------------------------------------------------
#LOAD SHAPE FILES FROM CENSUS WEBSITE USING TIGRIS PACKAGE:
options(tigris_use_cache = TRUE) #Census file server has hiccups. So use
# To show lakes, set cb=TRUE. Cache to not re-download.
#us_counties_shp <- counties(year = 2019, cb =TRUE, resolution = "500k")

#load State shape file
#Valid values are '500k', '5m', and '20m'.
# To show lakes, set cb=TRUE. Cache to not re-download. 
us_states_shp <- states(year = 2019, cb =TRUE, resolution = "500k")


#LOAD SHAPE FILES FROM DOC SHAREPOINT 

# load(paste0(shape.path,'/Shapefile RData format/state.RData'))
# load(paste0(shape.path,'/Shapefile RData format/county.RData'))


#-----------------------------------------------------------
#IMPORT DATA
#Note. API does not appear to include weekly, so using downloaded file
#renames state column to 'state' so map function will recognize it
#combine NY and NYC
deaths<-read_csv(paste0('./raw data',
  '/Provisional_COVID-19_death_counts__rates__and_percent_',
                new.week,'.csv')) %>%
  dplyr:: rename(state = Jurisdiction_Residence) %>% 
  subset(Group == "weekly") %>% 
  # subset(Jurisdiction_Residence != "United States")
  mutate(data_period_end = as.Date(`data_period_end`, "%m/%d/%Y"),
         state = 
        ifelse(state == 'New York', 'New York (excluding NYC)',
        ifelse(state == 'New York and New York City', 'New York',
                         state))
         ) %>% 
  subset(`data_period_end` %in% max(`data_period_end`, na.rm = TRUE)) %>%
  subset(state != "United States")%>%
  subset(! state %like% 'Region')


# Import same death data, but keep the prior week and US / National-level
#only for percent change
prior.deaths<-read_csv(paste0('./raw data',
          '/Provisional_COVID-19_death_counts__rates__and_percent_',
                        new.week,'.csv')) %>%
  subset(Group == "weekly") %>% 
  dplyr:: rename(state = Jurisdiction_Residence) %>%
  subset(state %in% 'United States') %>% 
  mutate(data_period_end = as.Date(`data_period_end`, "%m/%d/%Y")) %>% 
  subset(data_period_end >= max(data_period_end, na.rm = TRUE)-7
  )

#----------------------------------------------------------

#TRANSFORM SHAPE FILE TO LATER SHIFT PR, HI, AND AK BELOW 48 STATES

#Key for select	FIPS State Numeric Code
# 02   Alaska (AK)
# 15   Hawaii
# 60	American Samoa
# 64	Federated States of Micronesia
# 66	Guam
# 68	Marshall Islands
# 69	Commonwealth of the Northern Mariana Islands
# 70	Palau
# 72	Puerto Rico
# 74	U.S. Minor Outlying Islands
# 78	U.S. Virgin Islands

#display which state codes are in shape files
# table(us_counties_shp$STATEFP)
# class(us_counties_shp$STATEFP)
# table(us_states_shp$STATEFP); class(us_states_shp$STATEFP)

#Remove territories, and keep HI, AK, & PR

#counties 
# lower_48_co <- us_counties_shp %>% 
#   filter(!(STATEFP %in% c(
#     '60',	'64',	'66',	'68',	'69',	'70', '74',	'78'
#   )))

#states
lower_48_st <- us_states_shp %>% 
  filter(!(STATEFP %in% c(
    '60',	'64',	'66',	'68',	'69',	'70',	'74',	'78'
  )))

#Shift AK, HI, and PR below 48 states:

#counties 
# us_counties_shifted <- lower_48_co  %>% 
#   shift_geometry()

#states
us_states_shifted <- lower_48_st  %>% 
  shift_geometry()

#concatenate fips and state codes for later merge with CCL. 
#Convert to numeric:
# us_counties_shifted$fips_shp<-
#   as.numeric(paste0(us_counties_shifted$STATEFP,
#                     us_counties_shifted$COUNTYFP))

#-----------------------------------------------------------
#CATEGORIZE DEATH PERCENTAGES INTO LEVELS

#Note - zero deaths does NOT risk anonymity because it indicates no one died
#in that jurisdiction, so zeroes can be shown

deaths <- deaths %>% 
  mutate(COVID_pct_of_total_level =
                           ifelse(is.na(COVID_pct_of_total), "1-9 Deaths",
                           ifelse(COVID_pct_of_total >= 8.0, '>= 8.0%',
                           ifelse(COVID_pct_of_total >= 6.0, '6.0% to 7.9%',
                           ifelse(COVID_pct_of_total >= 4.0, '4.0% to 5.9%',
                           ifelse(COVID_pct_of_total >= 2.0, '2.0% to 3.9%',
                           ifelse(COVID_pct_of_total < 2.0, '< 2.0%',
                                  "No Data")))))))

# order character version of death percentages column to map if needed
table(deaths$COVID_pct_of_total_level) #display default order
deaths$COVID_pct_of_total_level= factor(deaths$COVID_pct_of_total_level,
                            levels = c( "1-9 Deaths",
                                        '< 2.0%',
                                '2.0% to 3.9%', 
                                '4.0% to 5.9%',
                                '6.0% to 7.9%', 
                                '>= 8.0%',
                                        'No Data'
                            ), ordered = TRUE)
#check
table(deaths$COVID_pct_of_total_level)#display new order and proper case
class(deaths$COVID_pct_of_total_level)

#-----------------------------------------------------------

# MERGE SHAPE FILE WITH DEATH DATA

deaths.w.shp<-full_join(us_states_shifted, deaths, by=c('NAME'='state'))

# Name state abbreviation column to 'state; 
#it must say 'state' to make map
deaths.w.shp$state<-deaths.w.shp$STUSPS

#-------------------------------------------------------------------

#MAKE PERCENT DIFFERENCE VECTORS 

avg.weekly.deaths <- 
  prior.deaths[prior.deaths$data_period_end ==
        max(prior.deaths$data_period_end, na.rm=T),]$COVID_pct_of_total

pct.diff<-
  prior.deaths[prior.deaths$data_period_end ==
          max(prior.deaths$data_period_end, na.rm=T),]$pct_diff_wk

#absolute value:
abs.pct.diff<-abs(as.numeric(pct.diff))

#Create vector for percent direction 
pct.dir <- ifelse(pct.diff == 0.0, "stable", 
                  ifelse(pct.diff < 0.0 , "down",
                         ifelse(pct.diff > 0.0 , "up")))

#################################################################


#     SET SOME NORTHEAST STATE MAP LABELS OUTSIDE OF MAP

# (states - DC, DE, MA, NH, NJ, RI)

usa_sf <- us_states_shifted %>%
  st_as_sf(usa_composite("laea")) %>%
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  ) %>%
  as_tibble() %>%
  st_as_sf()

usa_sf$nudge_x <- 0
usa_sf$nudge_y <- 0


x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))

#Note. x-range moves state label left - right. Y-range moves it up - down

ix <- usa_sf$STUSPS %in% c("NH")
usa_sf$nudge_x[ix] <- -.55 * 0.04 * x_range
usa_sf$nudge_y[ix] <- .7 * 0.10 * y_range

ix <- usa_sf$STUSPS %in% c("DC")
usa_sf$nudge_x[ix] <- .9 * 0.03 * x_range
usa_sf$nudge_y[ix] <- -.8 * 0.07 * y_range

ix <- usa_sf$STUSPS %in% c("MA")
usa_sf$nudge_x[ix] <- .25 * 0.27 * x_range
usa_sf$nudge_y[ix] <- -.12 * 0.02 * y_range

ix <- usa_sf$STUSPS %in% c("RI")
usa_sf$nudge_x[ix] <- .2 * 0.1 * x_range
usa_sf$nudge_y[ix] <- -.3 * 0.10 * y_range

ix <- usa_sf$STUSPS %in% c("NJ")
usa_sf$nudge_x[ix] <- .3 * 0.08 * x_range
usa_sf$nudge_y[ix] <- -.3 * 0.07 * y_range

ix <- usa_sf$STUSPS %in% c("DE")
usa_sf$nudge_x[ix] <- .4 * 0.09 * x_range
usa_sf$nudge_y[ix] <- -.3 * 0.07 * y_range



#######################################################################


# MAKE  MAP  - TAKES A FEW MINUTES TO RUN 


# map it
#rm(m)
m<-
  ggplot() +
  geom_sf(data= deaths.w.shp,
          aes(
            fill =  COVID_pct_of_total_level,
            geometry=geometry #if 'aes_string', then place geometry in quotes
          ),  
          color='grey' #,  size = 1.5,
  )+ 
  coord_sf(crs = st_crs('ESRI:102003')) +  # Albers
  scale_fill_manual(
    values=c(
              '1-9 Deaths' = '#e2f5f3', #light mint green
             '< 2.0%' = '#059d94', #dark green,
             '2.0% to 3.9%' = '#06cc98',  #medium green
             '4.0% to 5.9%' = '#effd96', #yellow
             '6.0% to 7.9%'= '#f1b774', #medium orange
             '>= 8.0%' = '#f89459', # dark orange,
             'No Data' = 'white'),
    drop = FALSE, #force ggplot to show all categories 
    na.value='white',
    # rearrange or hide legend values:
    breaks=c( '1-9 Deaths','< 2.0%', '2.0% to 3.9%',
              '4.0% to 5.9%', '6.0% to 7.9%',  '>= 8.0%',
      'No Data'),
    name = 'Percent Deaths\nAttributed to\nCOVID-19'
  )+
  labs(
    title = paste0('Pct. of All Deaths<br>',
                   'through<br>',
                   format(as.Date(max(deaths$data_period_end, na.rm=T),
                            '%Y-%m-%d'), '%m/%d/%Y')
    ) ,
    subtitle =  paste0( avg.weekly.deaths,
                        "% average weekly <br>",
                        "COVID 19 deaths as a pct.<br>",
                        "of all deaths in last 7 days, <br>",
                        pct.dir, " ", abs.pct.diff," ", 
                        "percentage points <br>",
                        "from previous 7 days") 
  )+
  theme_void() + #makes background white, removes axis
  theme(
    plot.margin = unit(c(0.5,10,0,0), "cm"), #moves map top, left, up, right
    # legend.position='right',
    legend.position = c(1,0.1), #manual position (left, down)
    legend.background = element_rect(fill = 'transparent',colour = NA),
    legend.justification = c(0.5, 0),
    # plot.title.position = 'plot',
 #positive hjust moves to right, negative vjust moves down
    plot.title =  element_textbox(
      size=16, hjust= 1.55, vjust = unit(5,"cm"), 
      face='bold',
      margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_textbox(
  #positive hjust moves to right, negative vjust moves down
      hjust=1.7, vjust = unit(2.5,"cm"),
      size =14, margin = margin(0, 0, 0, 0)),
    # plot.title element_text(face='bold'),
    legend.text = element_text(size=10),
    legend.title = element_text(size=9, face='bold')
  )+
  #guides(fill=guide_legend(nrow=2,byrow=TRUE) #make legend 2 rows
  # )+  
  
  #Add state border - #size controls thickness of lines
  geom_sf(data=us_states_shifted, aes(color='transparent', geometry=geometry),
          fill= 'transparent',
          color='black', size=0.5)+
  #coord_sf(crs = st_crs('ESRI:102003'),
  coord_sf(crs =  st_crs(2163),
           # Hide outlying areas in Aleutians West Census Area, Alaska to reduce space
           xlim = c(-2000000, 2500000), 
           ylim = c(-2700000, 730000)) + 
  #Add state labels -- with shadow
  geom_shadowtext(data=usa_sf,
                  aes(label = STUSPS,
                      geometry = geometry),
                  nudge_x = usa_sf$nudge_x,
                  nudge_y = usa_sf$nudge_y,
                  bg.color='white', size=3, color = 'black',
                  segment.color = "transparent",
                  stat = "sf_coordinates")


#display map
m

######################################################################
#                         EXPORT DEATH MAP  

ggsave(plot=m,
       paste0(
         'maps/deaths_', new.week,
         '_', today.date, "_", user, '.png'),
       # width=7, height=6, units='in')#, bg = 'transparent') #smaller size
       width=12, height=7, units='in', bg = 'white') #standard

