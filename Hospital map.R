#                         OBJECTIVE: 
#Program to make Hospital Admission Rate per 100k county map for
#the most recent complete MMWR week (Sun-Sat). Metrics will be and must
#be at HSA level.One can run this code on either Mon, Tues, Weds, or Thurs


#     ********   DIRECTIONS TO RUN CODE: *********

#Although hospital data refreshes daily, run on either Wednesday or Thursday
#to match the CDT as close as possible, which updates on Thursday. 
#Go to the contour linked below, click Update, and update the dates in
#the filter to most complete MMWR week (Sun-Sat)
#https://protect.hhs.gov/workspace/contour-app/ri.contour.main.analysis.4310c20d-e0a7-4292-a975-c09872aa539a/path/ri.contour.main.ref.d2700180-b7ba-444c-9068-f5a4e5945a45/board/545b8d4d-6bb1-3eac-9e2d-d7692930ed2c?viewMode=edit
#Export data and save to raw data folder using same file naming convention
#as previous versions (usually date is Weds or Thurs)
#Update dates below from prior and current week's file name, and Saturday. 
#Run code.
#Map and table will both output to map folder.
#Text you need to enter on slide will be on same page as map.
#Snip map and legend only; type all text directly into slide

#UPDATE DATE FROM LAST & CURRENT WEEK'S FILE NAME:
prior.week<-'06-01-2023'
new.week<-'06-08-2023'
most.recent.Saturday<-('06-03-2023')

#If needed, update Unallocated territory FIPS codes, excpet PR, if any other
#territories except for AS, GU. VI are in the hospital data:
unall.terr <-c(60000, 66000, 78000) #AS, GU, VI


#****************************************************************************

# ********************   BACKGROUND:     ****************

#How weekly county Admission Rate is calculated (estimated):
#1. Must use data collected at HHS ID-level as opposed to hospital-level
#to include more counties.
#2. Sum admissions of confirmed COVID-19 for both adult & pediatric for week
#3. Estimate admissions by summing admissions for available 
#counties within each Health Service Area (HSA)  
#4. Divide sum by the HSA population for each HSA, multiply by 100k
#5. Apply admissions total to each county in the HSA 


#County-level map from COVID Data Tracker to use as guide / or use COVID Update:
#https://covid.cdc.gov/covid-data-tracker/#cases_new-admissions-rate-county

#To find hexa color picker online. Upload your image at below link:
#https://imagecolorpicker.com/en

#Links to census Shape Files if they do not download. To read in shape files,
#download the zip files. Use function for example read_sf('data/county_5m.shp') 
#county: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#state: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

#########################################################################
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
library(sf)  # to use 'coord_sf'for map
#-------------------------------------------------------

#SET UP DIRECTORIES

#set working directory to any current location:
current.dir <- dirname(getActiveDocumentContext()$path)
setwd(current.dir)
getwd()


#shape file directories 
# user <- Sys.getenv('USERNAME')
# shape.path <- paste0('C:/Users/', user, '/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/3. Data On Call Data Files/Shape Files')

#county census file directory:
user <- Sys.getenv('USERNAME')
county.census.path <- paste0('C:/Users/', user, '/CDC/Data, Analytics, Visualization Task Force - Data On-Call Team/3. Data On Call Data Files/2019 Vintage Population/County')


#current date vector
today.date<-Sys.Date()
# convert from default format to no dashes
today.date<-format(today.date,'%Y%m%d')
#today.date<-format(today.date,'%Y%m%d','%b%d%Y')

#---------------------------------------------------------------
#LOAD COUNTY CENSUS POPULATIONS FROM SHAREPOINT 

#To date, we use 2019 data. Update year if needed.
#YEAR 12 = 2019; AGEGRP 0 = Total

co.census <-read_csv(paste0(county.census.path,
    "./cc-est2019-alldata.csv")) %>%
  subset(YEAR==12 &  AGEGRP==0) %>%
  select(STATE, COUNTY, STNAME, CTYNAME, YEAR, AGEGRP, TOT_POP) %>% 
  mutate(
#Ensure both COUNTY and SATE code columns import as character with 
#with leading zeroes, in order to concatenate FIPS code column correctly.   
    FIPS_code_char = paste0(STATE, COUNTY),
   FIPS_code = as.numeric( FIPS_code_char)
    )%>%
  relocate (FIPS_code, .before = 'STATE') 


#---------------------------------------------------------------
#LOAD SHAPE FILES FROM CENSUS WEBSITE USING TIGRIS PACKAGE:

#counties:
options(tigris_use_cache = TRUE) #Census file server has hiccups. So use
# To show lakes, set cb=TRUE. cache to not re-download. 
us_counties_shp <- counties(year = 2019, cb =TRUE, resolution = "500k")

#states
#Valid values are '500k', '5m', and '20m'.
# To show lakes, set cb=TRUE. cache to not re-download. 
us_states_shp <- states(year = 2019, cb =TRUE, resolution = "500k")


#LOAD SHAPE FILES FROM DOC SHAREPOINT IF NEEDED

# load(paste0(shape.path,'/Shapefile RData format/state.RData'))
# load(paste0(shape.path,'/Shapefile RData format/county.RData'))


#-----------------------------------------------------------
#IMPORT DATA

#Import HSA mapping data
HSA.mapping <-read_csv(paste0("./raw data/",
   "Health Service Area Mapping from CCL_2023.csv")) %>%
  mutate(county_fips = as.integer(county_fips))

#HSA file is missing Virgin Island HSA population - insert value
#year needed
HSA.mapping[HSA.mapping$county_fips==78000,]$health_service_area_population_2019<-
  104672        
          
#Import Hospital data
#Below renames to 'fips' and 'state_ab', removes NA's from fips, 
#makes fips numeric--fips must be numeric for map.
#add missing fips code for American Samoa
#standardize each territory FIPS by recoding as unallocated, except for PR

admis <- read_csv(paste0('./raw data',
 '/contour-export-Unified-HHS-ID-Hospital-Admissions-weekly-',
 new.week,'.csv')) %>%
  dplyr:: rename(fips = fips_code) %>% 
  dplyr:: rename(state_ab = state) %>% 
  mutate(fips = as.integer(fips),
  tot.admis = SUM_of_previous_day_admission_adult_covid_confirmed +
    SUM_of_previous_day_admission_pediatric_covid_confirmed,
  fips = 
  ifelse(state_ab == 'AS', 60000, 
  ifelse(state_ab == 'GU', 66000,
  ifelse(state_ab == 'MP', 69000,
  ifelse(state_ab == 'VI', 78000, fips)))) 
  )%>%
   subset(! fips %in% NA) 

#Import prior week hospital data
prior.admis <- read_csv(paste0('./raw data',
     '/contour-export-Unified-HHS-ID-Hospital-Admissions-weekly-',
              prior.week,'.csv')) %>%
  dplyr:: rename(fips = fips_code) %>% 
  dplyr:: rename(state_ab = state) %>% 
  mutate(fips = as.integer(fips),
         tot.admis = SUM_of_previous_day_admission_adult_covid_confirmed +
           SUM_of_previous_day_admission_pediatric_covid_confirmed,
         fips = 
           ifelse(state_ab == 'AS', 60000, 
                  ifelse(state_ab == 'GU', 66000,
                         ifelse(state_ab == 'MP', 69000,
                                ifelse(state_ab == 'VI', 78000, fips)))) 
  )%>%
  subset(! fips %in% NA)

#----------------------------------------------------------

#TRANSFROM SHAPE FILE TO LATER SHIFT PR, HI, AND AK BELOW 48 STATES

#Note.In year 2020, FIPS 2261 (Valdez-Cordova Census Area, Alaska) 
#split into 2063 (Chugach Census Area) and 2066 (Copper River Census Area)
#in 2020. But, to date, we still use 2019 FIPS, so code uses 2019 census data.

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
lower_48_co <- us_counties_shp %>%
  filter(!(STATEFP %in% c(
    '60',	'64',	'66',	'68',	'69',	'70', '74',	'78'
  )))

#states
lower_48_st <- us_states_shp %>% 
  filter(!(STATEFP %in% c(
    '60',	'64',	'66',	'68',	'69',	'70',	'74',	'78'
  )))

#Shift AK, HI, and PR below 48 states:

#counties 
us_counties_shifted <- lower_48_co  %>%
  shift_geometry()

#states
us_states_shifted <- lower_48_st  %>% 
  shift_geometry()

#concatenate fips and state codes for later merge with hosp data. 
#Converts to numeric:
us_counties_shifted$fips_shp<-
  as.numeric(paste0(us_counties_shifted$STATEFP,
                    us_counties_shifted$COUNTYFP))


##########################################################################

# CALCULTE ADMISSIONS COUNT & RATE PER 100K

#merge HSA codes with Hospital data so that EVERY FIPS codes gets an HSA
admis<-full_join(HSA.mapping, admis, 
                 by=c( "county_fips" = "fips"))


#merge county census population so that EVERY FIPS codes gets a population
admis<-full_join(admis, co.census, 
                 by=c( "county_fips" = "FIPS_code"))

#Special HSA with no FIPS code in the data - 
#Re-map to near HSA & HSA population 
#Re-code Audrain & Monroe County, MO (FIPS 29007 & 29137) into HSA 553
#Re-code Montgomery County, MO (FIPS 29139) into HSA 599
admis <- admis %>%
  mutate(
      health_service_area_population_2019 = 
      ifelse(county_fips %in% c(29007, 29137), max( 
admis[health_service_area_number == 553,]$health_service_area_population_2019,
  na.rm=T),          
   ifelse(county_fips %in% 29139, max(
  admis[health_service_area_number == 599,]$health_service_area_population_2019,
  na.rm = T),                
    health_service_area_population_2019)),
 health_service_area_number=
      ifelse(county_fips %in% c(29007, 29137), 553,
    ifelse(county_fips %in% 29139, 599,
                    health_service_area_number))
          )


#calculate admissions count & rate at HSA-level & apply to each county.
admis<- admis %>%
  group_by(health_service_area_number) %>%
  mutate(
    HSA.admis.NA.count = sum(is.na(tot.admis)), #count NA's 
    fips.count.in.HSA = length(county_fips), #count FIPS
#If an HSA has all NA FIPS admissions, then make NA; if not, then ignore NA:
    tot.admis_complete =
    ifelse(HSA.admis.NA.count == fips.count.in.HSA, NA,
    round(sum(tot.admis, na.rm = T))
  )
  ) %>%
# must use un-group to keep all rows and un-group data!  
  ungroup() %>% 
  mutate(
    admis.rate.per.100k = (tot.admis_complete /
                  health_service_area_population_2019) *100000,
    admis.rate.per.100k = round(admis.rate.per.100k,1),
    admis.level =ifelse(admis.rate.per.100k >= 20, 'High (>= 20)',
       ifelse(admis.rate.per.100k >= 10, 'Medium (10.0 to 19.9)',
        ifelse(admis.rate.per.100k < 10, 'Low (< 10.0)',
        ifelse(is.na(tot.admis_complete),  'Insufficient data',
               'Insufficient data') )))
  )

#REMOVE any duplicate FIPS county codes! 
  #remove duplicate territories created earlier. Each territory, except PR,
#should appear once and as un-allocated
admis<-  admis[!duplicated(admis$county_fips), ]

# order character version of COVID ED visits column to map if needed
table(admis$admis.level) #display default order
admis$admis.level = factor(admis$admis.level,
                           levels = c( 'High (>= 20)',
                                       'Medium (10.0 to 19.9)',
                                       'Low (< 10.0)',
                                       'Insufficient data'),
                           ordered = TRUE)
#check
table(admis$admis.level); 
class(admis$admis.level)

#Ceate table of percent diffrence 
Tot.admis<- admis %>%
  group_by(admis.level) %>%
  summarise(
    Total = round(n()),
    Percent = (Total / length(admis$county_fips)) *100
    )

  
#------------------------------------------------------------------
#Apply same analysis as above to prior week

#merge HSA codes with prior Hospital data so that EVERY FIPS codes gets an HSA
prior.admis<-full_join(HSA.mapping, prior.admis, 
                 by=c( "county_fips" = "fips"))

#merge county census population so that EVERY FIPS codes gets a population
prior.admis<-full_join(prior.admis, co.census, 
                 by=c( "county_fips" = "FIPS_code"))

#Special HSA with no FIPS code in the data - 
#Re-map to near HSA & HSA population 
#Re-code Audrain & Monroe County, MO (FIPS 29007 & 29137)into HSA 553
#Re-code Montgomery County, MO (FIPS 29139)into HSA 599
prior.admis <- prior.admis %>%
  mutate(
    health_service_area_population_2019 = 
      ifelse(county_fips %in% c(29007, 29137),  max(
prior.admis[health_service_area_number == 553,]$health_service_area_population_2019,
        na.rm =t),            
 ifelse(county_fips %in% 29139, max( 
        prior.admis[health_service_area_number == 599,]$health_service_area_population_2019,
        na.rm =T),          
          health_service_area_population_2019)),
    health_service_area_number=
      ifelse(county_fips %in% c(29007, 29137), 553,
             ifelse(county_fips %in% 29139, 599,
                    health_service_area_number))
  )


#calculate admissions count & rate at HSA-level & apply to each county:
prior.admis<- prior.admis %>%
  group_by(health_service_area_number) %>%
  mutate(
    HSA.admis.NA.count = sum(is.na(tot.admis)), #count NA's 
    fips.count.in.HSA = length(county_fips), #count FIPS
    #If an HSA has all NA FIPS admissions, then make NA; if not, then ignore NA:
    tot.admis_complete =
      ifelse(HSA.admis.NA.count == fips.count.in.HSA, NA,
             round(sum(tot.admis, na.rm = T))
      )
  ) %>%
  # must use un-group to keep all rows and un-group data!  
  ungroup() %>% 
  mutate(
    admis.rate.per.100k = (tot.admis_complete /
                             health_service_area_population_2019) *100000,
    admis.rate.per.100k = round(admis.rate.per.100k,1),
    admis.level =ifelse(admis.rate.per.100k >= 20, 'High (>= 20)',
          ifelse(admis.rate.per.100k >= 10, 'Medium (10.0 to 19.9)',
             ifelse(admis.rate.per.100k < 10, 'Low (< 10.0)',
            ifelse(is.na(tot.admis_complete),  'Insufficient data',
                                             'Insufficient data') )))
  )

#REMOVE any duplicate FIPS county codes! 
#remove duplicate territories created earlier. Each territory, except PR,
#should appear once and as un-allocated
prior.admis<-  prior.admis[!duplicated(prior.admis$county_fips), ]


# order character version of COVID ED visits column to map if needed
table(prior.admis$admis.level) #display default order
prior.admis$admis.level = factor(prior.admis$admis.level,
                           levels = c( 'High (>= 20)',
                                       'Medium (10.0 to 19.9)',
                                       'Low (< 10.0)',
                                       'Insufficient data'),
                           ordered = TRUE)
#check
table(prior.admis$admis.level) 
class(prior.admis$admis.level)


#create TOTAL table for prior week

prior.Tot.admis<- prior.admis %>%
  group_by(admis.level) %>%
  dplyr:: rename(prior.admis.level = admis.level) %>% 
  summarise(
    prior.Total = round(n()),
    prior.Percent = (prior.Total / length(prior.admis$county_fips)) *100
  ) 

#-----------------------------------------------------------

# Make percent difference table:

#combine tables
Natl.admis<-cbind(Tot.admis, prior.Tot.admis)

#make table
Natl.admis2 <-Natl.admis %>%
  mutate(
        `% Change` =
          round(Natl.admis$Percent - Natl.admis$prior.Percent,2),
            Percent = round(Percent,2),
        Symbol = '%',
        `% Change`= paste0(`% Change`, Symbol),
       Percent =  paste0(Percent, Symbol)
        ) %>%
select (admis.level, Total, Percent, `% Change`) %>%   #keeps columns
subset(! admis.level %in% NA) %>% #drops row
dplyr::rename(Level = admis.level) 



#-----------------------------------------------------------

# MREGE COUNTY CENSUS SHAPE FILE WITH HOSP DATA

admis.w.shp<-full_join(us_counties_shifted, admis, by=c("fips_shp"=
                                                          "county_fips"))

# Add back in fips header column name; it must be lower case and numeric:
admis.w.shp$fips<-admis.w.shp$fips_shp


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


# MAKE  MAP  - TAKES A FEW MINUTES TO RUN 


# map it
#rm(m)
m<-
  ggplot() +
  geom_sf(data= admis.w.shp,
          aes(
            fill =  admis.level,
          geometry=geometry #if 'aes_string', then place geometry in quotes
          ),  
          color="#468C76" ,  size = 0.4, #dark green county borders
  )+ 
  coord_sf(crs = st_crs('ESRI:102003')) +  # Albers
  scale_fill_manual(
    values=c('Insufficient Data' = 'white', 
             'High (>= 20)' = '#f89459', # dark orange
             'Medium (10.0 to 19.9)' = '#effd96', #yellow
             'Low (< 10.0)' = '#06cc98'  #medium green 
             ),
  drop = FALSE, #force ggplot to show all categories 
    na.value='white',
# rearrange or hide legend values:
breaks=c('Low (< 10.0)','Medium (10.0 to 19.9)',
         'High (>= 20)',
         'Insufficient Data'),
name = 'COVID-19\nAdmissions\nper 100k'
)+
  labs(
    title = paste0('Confirmed COVID-19 Admissions through ', most.recent.Saturday)
#) ,
# #non-HSA count:
#  subtitle =  paste0( comma(sum(admis$tot.admis, na.rm=T)), 
# " from the MMWR week (Sun-Sat) ending ",
# format(as.Date(max(most.recent.Saturday), '%m-%d-%Y'), '%m/%d/%Y')
# ) 
)+
theme_void() + #makes background white, removes axis
theme(
  #plot.margin = unit(c(0.5,10,0,0), "cm"), #moves map top, left, up, right
  plot.margin = unit(c(.8,.8,.8,.8), "cm"),
   # legend.position='right',
    legend.position = c(1,0.1), #manual position (left, down)
      legend.background = element_rect(fill = 'transparent',colour = NA),
      legend.justification = c(0.5, 0),
     # plot.title.position = 'plot',
    #positive hjust moves to right, negative vjust moves down
    plot.title =  element_textbox(
    #  size=16, hjust= 1.7, vjust = unit(5,"cm"), 
      size=16, hjust= 0.5,
      face='bold',
      margin = margin(0, 0, 0, 0)),
    #plot.subtitle = element_textbox(
     # hjust=2.1, vjust = unit(2.8,"cm"),
      #      size =16, margin = margin(0, 0, 0, 0)),
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
#                         EXPORT HOSPITAL MAP  

#Note - if slashes are in dates, then file will not export. Make sure
#date uses dashes and not slashes!
ggsave(plot=m,
       paste0(
         'maps/Hospital_', most.recent.Saturday,
         '_', today.date,"_", user,'.png'),
      # width=7, height=6, units='in')#, bg = 'transparent') #smaller size
        width=12, height=7, units='in', bg = 'white') #standard

########################################################################
# TABLE TO ENTER ON SLIDE: EXPORT

#Export / Save
write.csv(Natl.admis2,
          paste0(getwd(),"/",
            "maps/hospital admssions Table_", most.recent.Saturday,
            "_", today.date,"_", user, ".csv"),
          row.names = FALSE, na = "")

#Note. Differences between CDT and this table are likely due to different 
#data pull times of the Unified Hospital data.