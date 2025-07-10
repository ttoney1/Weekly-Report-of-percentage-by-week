#Program to make NREVSS Lab / Test Positivity for last MMWR week state map
#and line chart.
#One can run this code on Thurs when new lab data is released. Check maximum
#end date in 'mmwrweek_end' Column. End Date should be most recent Saturday.
#Data is at HHS Region-level and is converted to State-level with 
#states belonging to the same HHS region coded as the same color in map.

# *********         DIRECTIONS TO RUN CODE: ***********

#One should run code on Thursday when new data is released
#Download raw NREVSS data from below link. 
#https://data.cdc.gov/Laboratory-Surveillance/Percent-Positivity-of-COVID-19-Nucleic-Acid-Amplif/gvsb-yw6g
#Save to raw data folder using same file naming convention
#as previous versions, and use Thursday for the dates.
#Update dates below to prior Thursday, current Thursday, and 
#most recent Saturday. 
#Run code.
#Map and chart will both output to map folder.
#Text you need to enter on slide will be on same page as map.
#Snip map and legend only; type all text directly into slide.
#Below are color codes to make text for percent change in slides:
#increase - red; decrease - green; stable - yellow

#UPDATE DATES TO PRIOR THURSDAY, CURRENT THURSDAY, & MOST RECENT SATURDAY
prior.week<-'06-01-2023'
new.week<-'06-08-2023'
most.recent.Saturday<-('06-03-2023')

#************************************************************************
#BACKGROUND:
#National Respiratory and Enteric Virus Surveillance System (NREVSS)
#On May 11, 2023 CDC discontinued utilizing the COVID electronic 
#laboratory reporting (CELR) platform as the primary laboratory source 
#of COVID-19 results, and therefore uses NREVSS.
#Percent positivity is calculated by dividing the number of positive NAATs
#by the total number of NAATs administered, then multiplying by 100
#[(# of positive NAAT tests / total NAAT tests) x 100].
#The data represent laboratory tests performed, not individual 
#(deduplicated) results in people. 

#Link to NREVSS lab raw data file used in this code:
#https://data.cdc.gov/Laboratory-Surveillance/Percent-Positivity-of-COVID-19-Nucleic-Acid-Amplif/gvsb-yw6g

#State-level map from COVID Data Tracker to use as guide / or use COVID Update:
#https://covid.cdc.gov/covid-data-tracker/#cases_positivity-week

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
library(fiftystater) # to make custom regions 

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

lab<-read_csv(paste0('./raw data',
  '/Percent_Positivity_of_COVID-19_Nucleic_Acid_Amplification_Tests_',
                new.week,'.csv')) %>%
  mutate(mmwrweek_end = as.Date(`mmwrweek_end`, "%m/%d/%Y")
         ) %>% 
  subset(`mmwrweek_end` %in% max(`mmwrweek_end`, na.rm = TRUE)) %>%
  subset(level != "National")


# Import same lab data, but keep the prior week and US / National-level
#only for percent change
prior.lab<-read_csv(paste0('./raw data',
          '/Percent_Positivity_of_COVID-19_Nucleic_Acid_Amplification_Tests_',
                        new.week,'.csv')) %>%
  subset(level %in% 'National') %>% 
  mutate(mmwrweek_end = as.Date(`mmwrweek_end`, "%m/%d/%Y")) %>% 
  subset(mmwrweek_end >= max(mmwrweek_end, na.rm = TRUE)-7
  )

#Import same lab data as above, but all weeks for trend chart at National
#level only

lab.wkly<-read_csv(paste0('./raw data',
                          '/Percent_Positivity_of_COVID-19_Nucleic_Acid_Amplification_Tests_',
                          new.week,'.csv')) %>%
  mutate(mmwrweek_end = as.Date(`mmwrweek_end`, "%m/%d/%Y")
  ) %>% 
  subset(level %in% "National")

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

#---------------------------------------------------------------------
#MAKE  HHS REGION BORDERS DATA

#Note. Must use same shape file that you used for states

hhs.region.1 <- us_states_shifted %>% 
  filter(STUSPS %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT')) %>% 
  summarise(STUSPS = 'hhs.region.1')

hhs.region.2 <- us_states_shifted %>% 
  #Excluding Puerto Rico so there is no border around PR
  filter(STUSPS %in% c('NJ', 'NY','VI')) %>% 
  summarise(STUSPS = 'hhs.region.2')

hhs.region.3 <- us_states_shifted %>% 
  filter(STUSPS %in% c('DE', 'DC', 'MD', 'PA', 'VA', 'WV')) %>% 
  summarise(STUSPS = 'hhs.region.3')

hhs.region.4 <- us_states_shifted %>% 
  filter(STUSPS %in% c('AL', 'FL', 'GA', 'KY','MS', 'NC','SC', 'TN')) %>% 
  summarise(STUSPS = 'hhs.region.4')

hhs.region.5 <- us_states_shifted %>% 
  filter(STUSPS %in% c('IL', 'IN', 'MI','MN', 'OH', 'WI')) %>% 
  summarise(STUSPS = 'hhs.region.5')

hhs.region.6 <- us_states_shifted %>% 
  filter(STUSPS %in% c('AR', 'LA','NM', 'OK' ,'TX')) %>% 
  summarise(STUSPS = 'hhs.region.6')


hhs.region.7 <- us_states_shifted %>% 
  filter(STUSPS %in% c('IA', 'KS', 'MO', 'NE')) %>% 
  summarise(STUSPS = 'hhs.region.7')

hhs.region.8 <- us_states_shifted %>% 
  filter(STUSPS %in% c('CO', 'MT','ND', 'SD', 'UT', 'WY')) %>% 
  summarise(STUSPS = 'hhs.region.8')

hhs.region.9 <- us_states_shifted %>% 
  filter(STUSPS %in% c('AZ', 'CA', 'HI', 'NV' )
         # 'AS', 'GU', 'MP')
  ) %>% 
  summarise(STUSPS = 'hhs.region.9')

hhs.region.10 <- us_states_shifted %>%  
  #excluding Alaska so no border is around Alaska
  filter(STUSPS %in% c('ID', 'OR', 'WA')) %>% 
  summarise(STUSPS = 'hhs.region.10')


#-----------------------------------------------------------------------

#CATEGORIZE LAB HHS REGIONS INTO STATES SO THAT MAP CAN BE LATER CREATED 

#Use R-bult -in data frame including Puerto Rico.Add PR, DC, VI:
#(below requires tidyverse package)
state.region <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC")) %>% 
  bind_rows(tibble(state = "Puerto Rico", abb = "PR")) %>%
  bind_rows(tibble(state = "Virgin Islands", abb = "VI"))

# Categorize states into regions first:

state.region <- state.region %>%
  mutate(HHS.Region = fcase(
    state %in% c('Connecticut','Maine','Massachusetts',
                'New Hampshire','Rhode Island','Vermont'),"Region 1",
    state %in% c('Puerto Rico', 'New Jersey', 'New York',
                 'Virgin Islands'
                 ),
                "Region 2",
    state %in% c('Delaware', 'District of Columbia', 'Maryland',
               'Pennsylvania', 'Virginia', 'West Virginia'),
                "Region 3",
    state %in% c('Alabama', 'Florida', 'Georgia', 'Kentucky',
          'Mississippi', 'North Carolina',
          'South Carolina', 'Tennessee'), "Region 4",
    state %in% c('Illinois', 'Indiana', 'Michigan',
                'Minnesota', 'Ohio', 'Wisconsin'), "Region 5",
    state %in% c('Arkansas', 'Louisiana',
            'New Mexico', 'Oklahoma' ,'Texas'),"Region 6",
    state %in% c('Iowa', 'Kansas', 'Missouri', 'Nebraska'), 
                  "Region 7",
    state %in% c('Colorado', 'Montana','North Dakota', 'South Dakota',
          'Utah', 'Wyoming'), "Region 8",
    state %in% c('Arizona', 'California', 'Hawaii', 'Nevada'),
             # 'American Samoa', 'Guam', 'Northern Mariana Islands',
              "Region 9",
    state %in% c('Alaska', 'Idaho', 'Oregon', 'Washington'),
          "Region 10"))

#Create a column for HHS Region map labels

state.region <- state.region %>%
  mutate(region.map.label = fcase(
    state %in% c('Maine'),"1",
    state %in% c('New York' ),"2",
    state %in% c('West Virginia'),"3",
    state %in% c('Georgia'), "4",
    state %in% c('Wisconsin'), "5",
    state %in% c('Texas'),"6",
    state %in% c('Kansas'), "7",
    state %in% c('Wyoming'), "8",
    state %in% c('Nevada'), "9",
    state %in% c('Oregon'),"10"))

#Join state data to lab data by matching on HHS Region
lab.state <-
left_join(state.region, lab, by = c("HHS.Region" = "level"))

#rename state columns so 'abb' becomes 'state'
lab.state<-lab.state %>%
  dplyr:: rename(state_full = state) %>% 
  dplyr:: rename(state = abb)

#-----------------------------------------------------------
#CATEGORIZE LAB PERCENTAGES INTO LEVELS

lab.state <- lab.state %>% 
  mutate(percent_pos_level =ifelse(is.na(percent_pos),'No Data',
                           ifelse(percent_pos >= 20.0, '>= 20.0%',
                           ifelse(percent_pos >= 15.0, '15.0% to 19.9%',
                           ifelse(percent_pos >= 10.0, '10.0% to 14.9%',
                           ifelse(percent_pos >= 5.0, '5.0% to 9.9%',
                           ifelse(percent_pos < 5.0, '< 5.0%',
                                  "Insufficient Data")))))))

# order character version of 'percent_pos_level' column to map if needed
table(lab.state$percent_pos_level) #display default order
lab.state$percent_pos_level= factor(lab.state$percent_pos_level,
                            levels = c( 'Insufficient Data',
                                        "< 5.0%",
                                        '5.0% to 9.9%',
                                '10.0% to 14.9%', 
                                '15.0% to 19.9%',
                                '>= 20.0%',
                                        'No Data'
                            ), ordered = TRUE)
#check
table(lab.state$percent_pos_level)#display new order and proper case
class(lab.state$percent_pos_level)

#-----------------------------------------------------------

# MERGE SHAPE FILE WITH LAB DATA

lab.w.shp<-full_join(lab.state, us_states_shifted, by=c('state'='STUSPS'))

#----------------------------------------------------------
#MAKE PERCENT DIFFERENCE VECTORS 

avg.weekly.pos <- 
  prior.lab[prior.lab$mmwrweek_end ==
        max(prior.lab$mmwrweek_end, na.rm=T),]$percent_pos


pct.diff<-
  prior.lab[prior.lab$mmwrweek_end ==
              max(prior.lab$mmwrweek_end, na.rm=T),]$perc_diff


#absolute value:
abs.pct.diff<-abs(as.numeric(pct.diff))

#Create vector for percent direction 
pct.dir <- ifelse(pct.diff < 0, "down", 
            ifelse(pct.diff = 0, "stable",     
                  "up")) 

#################################################################

#Commented- out below because not showing state labels
#     SET SOME NORTHEAST STATE MAP LABELS OUTSIDE OF MAP

# (states - DC, DE, MA, NH, NJ, RI)

# usa_sf <- us_states_shifted %>%
#   st_as_sf(usa_composite("laea")) %>%
#   mutate(
#     CENTROID = map(geometry, st_centroid),
#     COORDS = map(CENTROID, st_coordinates),
#     COORDS_X = map_dbl(COORDS, 1),
#     COORDS_Y = map_dbl(COORDS, 2)
#   ) %>%
#   as_tibble() %>%
#   st_as_sf()
# 
# usa_sf$nudge_x <- 0
# usa_sf$nudge_y <- 0
# 
# 
# x_range <- abs(Reduce("-", range(usa_sf$COORDS_X)))
# y_range <- abs(Reduce("-", range(usa_sf$COORDS_Y)))
# 
# #Note. x-range moves state label left - right. Y-range moves it up - down
# 
# ix <- usa_sf$STUSPS %in% c("NH")
# usa_sf$nudge_x[ix] <- -.55 * 0.04 * x_range
# usa_sf$nudge_y[ix] <- .7 * 0.10 * y_range
# 
# ix <- usa_sf$STUSPS %in% c("DC")
# usa_sf$nudge_x[ix] <- .9 * 0.03 * x_range
# usa_sf$nudge_y[ix] <- -.8 * 0.07 * y_range
# 
# ix <- usa_sf$STUSPS %in% c("MA")
# usa_sf$nudge_x[ix] <- .25 * 0.27 * x_range
# usa_sf$nudge_y[ix] <- -.12 * 0.02 * y_range
# 
# ix <- usa_sf$STUSPS %in% c("RI")
# usa_sf$nudge_x[ix] <- .2 * 0.1 * x_range
# usa_sf$nudge_y[ix] <- -.3 * 0.10 * y_range
# 
# ix <- usa_sf$STUSPS %in% c("NJ")
# usa_sf$nudge_x[ix] <- .3 * 0.08 * x_range
# usa_sf$nudge_y[ix] <- -.3 * 0.07 * y_range
# 
# ix <- usa_sf$STUSPS %in% c("DE")
# usa_sf$nudge_x[ix] <- .4 * 0.09 * x_range
# usa_sf$nudge_y[ix] <- -.3 * 0.07 * y_range
# 
# 

#######################################################################


# MAKE  MAP  - TAKES A FEW MINUTES TO RUN 

# map it
#rm(m)
m<-
  ggplot() +
  geom_sf(data= lab.w.shp,
          aes(
            fill = percent_pos_level,
        geometry=geometry #if 'aes_string', then place geometry in quotes
          ),  
          color='grey90' #,  size = 0.7,
  )+ 
  coord_sf(crs = st_crs('ESRI:102003')) +
    scale_fill_manual(
    values=c( 'Insufficient Data' = '#7d7d7d', #dark grey
              '< 5.0%' = '#0b9b98', #dark green
             '5.0% to 9.9%' = '#06cc98',  #medium green
             '10.0% to 14.9%' = '#f9fbb7', #yellow
             '15.0% to 19.9%'= '#f1b774', #medium orange
             '>= 20.0%' = '#f89459', # dark orange,
             'No Data' = 'white'),  
    drop = FALSE, #force ggplot to show all categories 
    na.value='white',
    # rearrange or hide legend values:
    breaks=c('Insufficient Data', '< 5.0%', '5.0% to 9.9%',
              '10.0% to 14.9%', '15.0% to 19.9%',  '>= 20.0%',
             'No Data'
      ),
    name = 'COVID-19 test positivity'
  )+
  labs(
    title = paste0('COVID-19 test positivity<br>',
                   'through ',
                   format(as.Date(max(lab.state$mmwrweek_end, na.rm=T),
                            '%Y-%m-%d'), '%m/%d/%Y')
    ) ,
    subtitle =  paste0( avg.weekly.pos, "% ",
                        "COVID-19 test positivity in <br>",
                        "past week, ", pct.dir,
                        " ", abs.pct.diff," percentage <br>",
                        "points from prior week") 
    )+
  theme_void() + #makes background white, removes axis
  theme(
    plot.margin = unit(c(1,10,1,0), "cm"), #moves plot top, left, up/small, right
    # legend.position='right',
    legend.position = c(0.97, 0.1), #manual position (left, down)
    legend.background = element_rect(fill = 'transparent',colour = NA),
    legend.justification = c(0.5, 0),
    # plot.title.position = 'plot',
 #positive hjust moves to right, negative vjust moves down
    plot.title =  element_textbox(
      size=16, hjust= 1.77, vjust = unit(5,"cm"),
      face='bold',
      margin = margin(15, 0, 0, 0), #1st number moves title up/down
      lineheight = 1.2 #line spacing
      ),
    plot.subtitle = element_textbox(
#positive hjust moves to right, negative vjust moves down
      hjust=1.89, vjust = unit(3.3,"cm"),
      size =14, 
      margin = margin(18, 0, 0, 0),
      lineheight = 1.0 #line spacing
      ),
    legend.text = element_text(size=12),
    legend.title = element_text(size=10, face='bold')
  )+ 
# Add region labels
  geom_sf_text(
                  data=lab.w.shp,
                  aes(label = region.map.label,
                      geometry = geometry),
                  # nudge_x = usa_sf$nudge_x,
                  # nudge_y = usa_sf$nudge_y,
                  bg.color='white', 
                  size=4, color = 'black',
                  segment.color = "white",
                  stat = "sf_coordinates") +
  geom_sf_label(
                  data = lab.w.shp,
                  aes(label = region.map.label,
                      geometry = geometry),
                   size = 4, 
                  #alpha = 0, #makes trasparent 
                   label.r = unit(0.5, "lines"), label.size = 1,
                    segment.color = "white", segment.size = 2
                   # seed = 1002
  )+
#Add HHS Region Borders
  geom_sf(data = hhs.region.1, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.2, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.3, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.4, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.5, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.6, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.7, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.8, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.9, col = "black", alpha = 0, size = 1.1)+
  geom_sf(data = hhs.region.10, col = "black", alpha = 0, size = 1.1)+
  coord_sf(
    #Hide outlying areas of Alaska to save space by moving map left
    #xlim - first number moves map left/right; ylim (excluded) moves up/down
    xlim = c(-2350000, 2500000)
  )
  
#display map
m

######################################################################
#                         EXPORT LAB MAP  

ggsave(plot=m,
       paste0(
         'maps/NREVSS_lab_', new.week,
         '_', today.date, "_", user, '.png'),
       # width=7, height=6, units='in', bg = 'white') #smaller size
       width=12, height=7, units='in',  bg = 'white') #standard

###############################################################

# MAKE LINE CHART

#UPDATE DATES BELOW IF NEEDED

#make line chart
lab.chart <- 
  ggplot()+
  geom_line(
    data = lab.wkly,
       aes(x=mmwrweek_end, y=percent_pos),
    color = '#edb07d',
    size = 2
        )+
  xlab("") +
  ylab("Weekly Test % Positivity") +
  # scale_x_date(date_labels = "%b %d, %y", 
  # limits = c(as.Date("2019-09-13"),
  #            max(lab.wkly$mmwrweek_end)
  #            ), 
  # breaks = "29 weeks", minor_breaks = "1 year",
  scale_x_date(
    date_labels = "%b %d, %y",
    limits = c(as.Date("2020-01-11"),max(lab.wkly$mmwrweek_end)), 
    breaks = c(as.Date("2020-01-11"),
                as.Date("2020-08-08"),as.Date("2021-03-06"),
               as.Date("2021-10-02"),as.Date("2022-04-30"),
               as.Date("2022-11-06"),max(lab.wkly$mmwrweek_end)) 
  ) +
  theme_classic()+
  theme(
        panel.grid.major.y = element_line(colour = "#cdd6dd"),
      text = element_text(size=22),
      axis.text.x = element_text(angle=45, hjust=1) 
  )

lab.chart #display chart
#--------------------------------------------------------------

#                         EXPORT LAB CHART  

ggsave(plot=lab.chart,
       paste0(
         'maps/NREVSS_lab_Chart_', new.week,
         '_', today.date, "_", user, '.png'),
        width=7, height=6, units='in', bg = 'white') #smaller size
      # width=12, height=7, units='in', bg = 'white') #standard