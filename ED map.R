#Program to make Emergency Department for last MMWR week state map
#One can run this code on Tues, Weds, or Thurs

# *********         DIRECTIONS TO RUN CODE: ***********

#No updates to code required, unless states with insufficient data need to be 
#updated near Line 25. ED Team POC (Karl Soetebier <fhd1@cdc.gov>) 
#will notify team with email if states change. If not, then just hit run. 
#Map will output to map folder.
#Text you need to enter on slide will be on same page as map.
#Snip map and legend only; type all text directly into slide.

#************************************************************************

#Link to raw ED data folder. ED Team places data here. Use most recent date:
#https://cdc.sharepoint.com/teams/DIMDataSurveillance-COVID19/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=bhk6P3&cid=f2ad7981%2D24da%2D4e4b%2Da9e2%2D83fcadbff1d3&FolderCTID=0x0120009E93F48EBAD70843A23B358980385899&id=%2Fteams%2FDIMDataSurveillance%2DCOVID19%2FShared%20Documents%2FData%20On%2DCall%20Team%2F1%2E%20Data%20Requests%2F00%5FFriday%20Indicators%20Reports%2FED%20Data%20Files&viewid=852f7f1a%2D4344%2D4bf3%2D9ef9%2D38f319addfd9

#State-level map from COVID Data Tracker to use as guide / or use COVID Update:
#https://covid.cdc.gov/covid-data-tracker/#cases_percent-covid-ed

#To find hexa color picker online. Upload your image at below link:
#https://imagecolorpicker.com/en

#Links to census Shape Files if they do not download. To read in shape files,
#download the zip files. Use function for example read_sf('data/county_5m.shp') 
#county: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#state: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

#UPDATE states that have insufficient data if needed (check ED map on CDT):
Insuf.States <- c('Minnesota', 'Oklahoma', 'Missouri', 'Puerto Rico')

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

#ED file directories 
 user <- Sys.getenv('USERNAME')
 ED.path <- paste0('C:/Users/', user, '/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/1. Data Requests/00_Friday Indicators Reports/ED Data Files')


#shape file directories 
# user <- Sys.getenv('USERNAME')
# shape.path <- paste0('C:/Users/', user, '/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/3. Data On Call Data Files/Shape Files')

#current date vector
today.date<-Sys.Date()
# convert from default format to no dashes
today.date<-format(today.date,'%Y%m%d')
#today.date<-format(today.date,'%Y%m%d','%b%d%Y')

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

#Below renames to 'state_name', removes NA's from state,
#removes rows beginning with 'Region' in state column,
#subsets to most recent date

#Note. ED data shows the week start on Sunday, but week ends on Saturday 
#Therefore, data goes through the most recent Saturday!

ED <- read_csv(paste0( ED.path,'/ED_COVID_DD_Weekly.csv')) %>%
   dplyr:: rename(state_name = Geography) %>% 
   subset(! state_name %in% NA) %>% 
   # subset(! state %in% grep('^Region',state))%>%
   subset(! state_name %like% 'Region')  %>%
    subset(Age_Group %in% 'all_ages') %>%
    subset(Time_Resolution %in% max(Time_Resolution, na.rm = TRUE)
    )
 
#Import same ED data, but keep prior week and US only for percent change
prior.ED <- read_csv(paste0( ED.path,'/ED_COVID_DD_Weekly.csv')) %>%
  dplyr:: rename(state_name = Geography) %>% 
  subset(state_name %in% 'United States') %>% 
  subset(Age_Group %in% 'all_ages') %>%
  subset(Time_Resolution >= max(Time_Resolution, na.rm = TRUE) -7
  )

#----------------------------------------------------------

#TRANSFROM SHAPE FILE TO LATER SHIFT PR, HI, AND AK BELOW 48 STATES

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
#CALCUALTE PERCENT OF COVID VISITS AMONG ALL ED VISITS & CATEGORIZE

#add a row for Puerto Rico
ED <- ED %>%
  add_row(state_name = 'Puerto Rico')

ED <- ED %>%
mutate(COVID_pct = Count_ED_Visits_Indicator /
         Count_ED_Visits_Total *100,
#categorize percents into levels:
  COVID_pct_level = ifelse(state_name %in% Insuf.States,'Insufficient Data', 
                    ifelse(COVID_pct >= 6.0, '>= 6.0%',
                    ifelse(COVID_pct >= 4.5, '4.5% to 5.9%',
                    ifelse(COVID_pct >= 3.0, '3.0% to 4.4%',
                    ifelse(COVID_pct >= 1.5, '1.5% to 2.9%',
                    ifelse(COVID_pct < 1.5, '< 1.5%',
                    ifelse(is.na(COVID_pct),'No Data',
                         'No Data')))))))
                  )

#create ED end date vector:
ED.end.date<-as.Date(max(ED$Time_Resolution, na.rm=T)) + 6

#create percent column for prior week
prior.ED <- prior.ED %>%
  mutate(COVID_pct = Count_ED_Visits_Indicator /
           Count_ED_Visits_Total *100)

# order character version of COVID ED visits column to map if needed
table(ED$COVID_pct_level) #display default order
ED$COVID_pct_level = factor(ED$COVID_pct_level,
        levels = c( 'Insufficient Data',
                '< 1.5%', '1.5% to 2.9%', '3.0% to 4.4%',
                    '4.5% to 5.9%', '>= 6.0%',
                    'No Data'
                    ), ordered = TRUE)
#check
table(ED$COVID_pct_level) #display new order and proper case
class(ED$COVID_pct_level)


#-----------------------------------------------------------

# MREGE SHAPE FILE WITH ED DATA

ED.w.shp<-left_join(us_states_shifted, ED, by=c('NAME'='state_name'))

# Name state abbreviation column to 'state; 
#it must say 'state' to make map
ED.w.shp$state<-ED.w.shp$STUSPS


##################################################################

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

#CALCULATE PERCENT POINT DIFFERENCE FROM PRIOR 7 DAYS COLUMNS:


# prior.ED$`Percentage Point difference from new week`<-NULL
#(formula to use: a-b)
prior.ED<-prior.ED %>%
  arrange(desc(Time_Resolution )) %>%
  mutate("Percentage Point difference from new week" =
           COVID_pct - lead(COVID_pct) 
  )
#format
prior.ED$Pct.Point.Diff <-
  format(round(as.numeric(prior.ED$'Percentage Point difference from new week'),
               1), nsmall=1, big.mark = ",")

#format percentage of COVID ED visits as percent format
prior.ED$COVID_pct.p <-
  format(round(as.numeric(prior.ED$COVID_pct),
               1), nsmall=1, big.mark = ",")

#Vectors:

#create vector for percent difference 
PCT.diff<-as.numeric(prior.ED[ prior.ED$Time_Resolution %in% 
       max(prior.ED$Time_Resolution, na.rm =T),]$Pct.Point.Diff)


abs.PCT.diff<-abs(PCT.diff)

#Create vector for percent of current national COVID ED visits:
natl.COVID.ED.pct<-
prior.ED[ prior.ED$Time_Resolution %in% 
            max(prior.ED$Time_Resolution, na.rm =T),]$COVID_pct.p
natl.COVID.ED.pct<-paste0(natl.COVID.ED.pct, "%")


#Create vector for percent direction 
PCT.dir <- ifelse(PCT.diff == 0.0, "stable", 
               ifelse(PCT.diff < 0.0 , "down",
                      ifelse(PCT.diff > 0.0 , "up")))

#check
abs.PCT.diff
natl.COVID.ED.pct
PCT.dir

###########################################################


# MAKE  MAP  - TAKES A FEW MINUTES TO RUN 


# map it
#rm(m)
m<-
  ggplot() +
  geom_sf(data= ED.w.shp,
          aes(
            fill =  COVID_pct_level,
          geometry=geometry #if 'aes_string', then place geometry in quotes
          ),  
          color='grey' #,  size = 1.5,
  )+ 
  coord_sf(crs = st_crs('ESRI:102003')) +  # Albers
  scale_fill_manual(
    values=c('Insufficient Data' = '#aeacad', #dark gray
'< 1.5%' = '#059d94', #dark green,
'1.5% to 2.9%' = '#06cc98',  #medium green
'3.0% to 4.4%' = '#effd96', #yellow
'4.5% to 5.9%'= '#f1b774', #medium orange
'>= 6.0%' = '#f89459', # dark orange,
'No Data' = 'white'),
  drop = FALSE, #force ggplot to show all categories 
    na.value='white',
# rearrange or hide legend values:
breaks=c('Insufficient Data', '< 1.5%','1.5% to 2.9%', '3.0% to 4.4%',
         '4.5% to 5.9%',  '>= 6.0%', 'No Data'),
name = 'Percent of ED\nVisits Diagnosed\nas COVID-19 in\nLast 7 Days'
)+
  labs(
    title = paste0('COVID-19 Emergency<br>',
       'Dept. Visits through<br>',
format(as.Date(max(ED$Time_Resolution, na.rm=T) +
                 6, '%Y-%m-%d'), '%m/%d/%Y')
) ,
subtitle = ifelse(PCT.diff != 0, paste0( natl.COVID.ED.pct,
                                                " of emergency department <br>",
                                                "visits are COVID-19 patients, <br>",
                                                PCT.dir, " ", abs.PCT.diff," ", 
                                                "percentage points <br>",
                                                "from previous 7 days"),
                  paste0( natl.COVID.ED.pct,
                          " of emergency department <br>",
                          "visits are COVID-19 patients, <br>",
                          PCT.dir," from previous 7 days"))
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
      size=16, hjust= 1.7, vjust = unit(5,"cm"), 
      face='bold',
      margin = margin(0, 0, 0, 0)),
    plot.subtitle = element_textbox(
      hjust=2.1, vjust = unit(2.8,"cm"),
            size =16, margin = margin(0, 0, 0, 0)),
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
#                         EXPORT ED MAP  

ggsave(plot=m,
       paste0(
         'maps/ED_', ED.end.date,
         '_', today.date, '.png'),
      # width=7, height=6, units='in')#, bg = 'transparent') #smaller size
        width=12, height=7, units='in') #standard

########################################################################
# TEXT TITLE TO ADD ON SLIDE:

 cat('COVID-19 Emergency\n',
      'Dept. Visits through\n',
      format(as.Date(max(ED$Time_Resolution, na.rm=T) +6, '%Y-%m-%d'), 
             '%m/%d/%Y')
)
# TEXT SUBTITLE TO ADD TO SLIDE:
 
cat( natl.COVID.ED.pct, 
    "of emergency department visits\n",
    "are COVID-19 patients,", PCT.dir,"\n",
      abs.PCT.diff, 
     "percentage points from\n",
      "previous 7 days")
