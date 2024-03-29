# Copyright 2019 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


#install.packages("bcdata")
#library("devtools")
#install_github("bcgov/envreportutils")


## Load libraries
library(readr) #load data from BC Data Catalogue
library(readxl) #load xlsx files
library(dplyr) # data munging
library(envreportutils)
library(tidyr)
library(stringr)
library(lubridate)
library(gridExtra)
library(bcdata)
library(sf)
library(mapview)
library(bcmaps)

wfile <- file.path("data",
                   "MASTER_Metrics for Publicly Available PGOWN Validated Data July2021.xlsx")

#wfile <- file.path(
#  soe_path("Operations ORCS/Special Projects/Water Program/Groundwater Wells Reporting/Data"),
#  "MASTER_Metrics for Publicly Available PGOWN Validated Data.xlsx"
#)

# Read in the data from Excel sheet using feb 2019 as base
wdata_0219 <- read_excel(wfile, sheet = "Feb 2019", range = "A2:J228",
                         col_names = c("Region", "Data_graded", "Well_ID", "Location",
                                       "Date_Validated", "Months_since_val", "foo","initial_cost","foo1", "comment"),
                         col_types = c("text", "text", "text","text", "date", "text",
                                       "text", "text", "text","text")) %>%
  select(-c("foo", "foo1")) %>%
  mutate(Region = ifelse(str_detect(Region, "%"),NA , Region),
         Region = ifelse(str_detect(Region, "Total"),NA , Region),
         initial_cost = as.numeric(initial_cost),
         Well_ID = as.integer(gsub("^#", "", Well_ID))) %>%
  fill(Region) %>%
  filter_at(.vars = vars(Data_graded, Well_ID), .vars_predicate = any_vars(!is.na(.))) %>%
  mutate(report_date = "2019-02-01",
         dateCheck = round(interval(ymd(Date_Validated),
                                    ymd("2019-02-01"))/ months(1), 0))

# create a well_key for each time slice
well_key <- read_excel(wfile, sheet = "Regions",
           col_names = c("Region", "Well_ID", "Location"),
           col_types = c("text", "text", "text")) %>%
  mutate(Well_ID = as.integer(gsub("^#", "", Well_ID)))

#well_key <- wdata_0219 %>%
#  select(Region, Well_ID, Location)

# function 1 to read in Feb 2020 sheets and beyond.

get_well_data_2020 = function(sheet, range, report_date) {
  tdata <- read_excel(wfile, sheet = sheet, range = range,
                      col_names = c("Data_graded", "FTE","inactive","Well_ID", "Location",
                                    "Date_Validated", "Months_since_val", "foo","initial_cost",
                                    "foo1", "comment"),
                      col_types = c("text", "text", "text", "text","text", "date", "text",
                                    "text", "text", "text","text")) %>%
    select(c(Data_graded, FTE, inactive, Well_ID, Location, Date_Validated, Months_since_val,
             initial_cost, comment)) %>%
    mutate(FTE = ifelse(str_detect(FTE, "-"), NA , FTE)) %>%
    fill(FTE) %>%
    filter(!is.na(Well_ID)) %>%
    mutate(initial_cost = as.numeric(initial_cost),
           Well_ID = as.integer(gsub("^#", "", Well_ID))) %>%
    filter_at(.vars = vars(Data_graded, Well_ID), .vars_predicate = any_vars(!is.na(.))) %>%
    mutate(report_date = report_date,
           dateCheck = round(interval(ymd(Date_Validated),
                                      ymd(report_date))/ months(1), 0)) %>%
    left_join(well_key, by = "Well_ID") %>% select(-c("Location.y")) %>%
    rename("Location" = "Location.x") %>% unique() #something weird is going on with the left_join I can't figure it out
}


wdata_0220 <- get_well_data_2020(sheet = "Feb 2020" , range = "E2:O238", report_date = "2020-02-01")
wdata_0720 <- get_well_data_2020(sheet = "July 2020" , range = "E2:O241", report_date = "2020-07-01")

#* added jkrogh Feb 2021
wdata_0221 <- get_well_data_2020(sheet = "Feb 2021" , range = "E2:O244" , report_date = "2021-02-01")

#* Added jkrogh July 2021
wdata_0721 <- get_well_data_2020(sheet = "July 2021" , range = "E2:O248" , report_date = "2021-07-01")
#*

# functions to format datasets (July 2018- July 2019)

get_well_data_graded = function(sheet, range, report_date) {
      tdata <- read_excel(wfile, sheet = sheet, range = range,
         col_names = c("Data_graded", "Well_ID", "Location",
                "Date_Validated", "Months_since_val", "foo","initial_cost","foo1", "comment"),
          col_types = c("text", "text","text", "date", "text",
                "text", "text", "text","text")) %>%
    select(c(Data_graded, Well_ID, Location, Date_Validated, Months_since_val,
                     initial_cost, comment))%>%
    filter(!is.na(Well_ID)) %>%
    mutate(initial_cost = as.numeric(initial_cost),
           Well_ID = as.integer(gsub("^#", "", Well_ID))) %>%
    filter_at(.vars = vars(Data_graded, Well_ID), .vars_predicate = any_vars(!is.na(.))) %>%
    mutate(report_date = report_date,
           dateCheck = round(interval(ymd(Date_Validated),
                                      ymd(report_date))/ months(1), 0)) %>%
    left_join(well_key)
}

wdata_0718 <- get_well_data_graded(sheet = "July 2018", range = "B2:J219", report_date = "2018-07-01")
wdata_0719 <- get_well_data_graded(sheet = "July 2019 ", range = "E2:M236", report_date = "2019-07-01")

wdata <- bind_rows(wdata_0219, wdata_0718, wdata_0719, wdata_0220, wdata_0720, wdata_0221, wdata_0721)

# functions to format datasets (Feb 2015 - Feb 2018)

get_well_data = function(sheet, range, report_date) {
  tdata <- read_excel(wfile, sheet = sheet, range = range,
                      col_names = c( "Well_ID", "Location",
                                    "Date_Validated", "Months_since_val", "foo","initial_cost","foo1", "comment"),
                      col_types = c("text","text", "date", "text",
                                    "text", "text", "text","text")) %>%
    select(c(Well_ID, Location, Date_Validated,Months_since_val,
             initial_cost, comment))%>%
    filter(!is.na(Well_ID)) %>%
    mutate(initial_cost = as.numeric(initial_cost),
           Well_ID = as.integer(gsub("^#", "", Well_ID))) %>%
    filter_at(.vars = vars(Well_ID), .vars_predicate = any_vars(!is.na(.))) %>%
    mutate(report_date = report_date,
           dateCheck = round(interval(ymd(Date_Validated),
                                      ymd(report_date))/ months(1), 0)) %>%
    left_join(well_key)
}


wdata_0218 <- get_well_data("Feb 2018", "B2:I214", "2018-02-01" )
wdata_0717 <- get_well_data("July 2017", "B2:I207", "2017-07-01" )
wdata_0217 <- get_well_data("Feb 2017", "B2:I198", "2017-02-01" )
wdata_0716 <- get_well_data("July 2016", "B2:I192", "2016-07-01" )
wdata_0316 <- get_well_data("March 2016", "B2:I193", "2016-03-01" )
wdata_0715 <- get_well_data("July 2015", "B2:I193", "2015-07-01" )

wdata_0215 <- get_well_data("Feb 2015", "B2:I168", "2015-02-01" )

wdata_0215 <- wdata_0215 %>%
  mutate(Months_since_val = round(as.numeric(Months_since_val)*12,0))


wdata <- bind_rows(wdata, wdata_0218, wdata_0717,wdata_0217,
                   wdata_0716, wdata_0316,  wdata_0715)

wdata <- wdata %>%
  mutate(Months_since_val = as.numeric(Months_since_val))

wdata <- bind_rows(wdata, wdata_0215)

# check
data_check <- wdata %>%
  group_by(report_date, Region) %>%
  summarise(n = n())


rm(wdata_0219, wdata_0218, wdata_0717,wdata_0217,
   wdata_0716, wdata_0316,wdata_0719,wdata_0718, wdata_0215, wdata_0715)

# update missing "region" values

region_table <- tribble(
  ~ Location, ~ Region2,
  "Merrit", "Thompson/Cariboo",
  "Joe Rich", "Okanagan/Kootenay",
  "Pemberton", "Lower Mainland",
  "Deroche", "Lower Mainland",
  "Dewdney", "Lower Mainland",
  "NG-Charlie Lake", "Ominca/Peace",
  "Canoe Creek", "Thompson/Cariboo",
  "Farmington", "Ominca/Peace",
  "Junction Sheep", "Thompson/Cariboo",
  "Shuswamp Lake Park Deep", "Thompson/Cariboo",
  "Salmon Arm", "Thompson/Cariboo",
  "Ellison", "Okanagan/Kootenay",
  "Cordova Bay", "Vancouver Island",
  "Fanny Bay", "Vancouver Island",
  "Ft Langley", "Lower Mainland",
  "Mt Newton", "Vancouver Island",
  "Ootischenia", "Okanagan/Kootenay",
  "Oyster River", "Vancouver Island",
  "Shuswap","Thompson/Cariboo",
  "Yarrow", "Lower Mainland",
  "Fort Nelson", "Ominca/Peace",
  "Cowichan Station (Koksilah)", "Vancouver Island",
  "Cowichan Station (Uphill Rd)","Vancouver Island"
)

wdata <- wdata %>%
  left_join(region_table) %>%
  mutate(Region = ifelse(is.na(Region), Region2, Region)) %>%
  dplyr::select(- Region2) %>%
  group_by(Location) %>%
  mutate(Region = ifelse(is.na(Region), first(Region), Region)) %>%
  ungroup() %>%
  mutate(inactive = ifelse(is.na(Date_Validated), "Y","N"),
         graded = ifelse(Data_graded %in% c("G","Y"), 1,
                         ifelse(Data_graded %in% c("NG","N","sort of"), 0,
                                ifelse(Data_graded %in% c("not available","-"), NA, Data_graded )))) %>%
  dplyr::select(-(Data_graded))

## data checks
#inactive <- wdata %>% filter(inactive == "Y")
#with.region <- wdata %>% filter(!is.na(Region))
#no.region <- wdata %>% filter(is.na(Region))

#data_check <- wdata %>%
#  group_by(report_date, Region) %>%
#  summarise(n = n())

# Update the names of the Regions
wdata <- wdata %>%
 mutate(Region = ifelse(Region == "Lower Mainland", "South Coast",
                        ifelse(Region == "Vancouver Island" , "West Coast", Region)))

# fix weird regions
wdata <- wdata %>% mutate(Region = case_when(Region == "Ominca/Peace" ~ "Omineca/Peace" ,
                                             Region == "Cariboo/Thompson" ~ "Thompson/Cariboo",
                                             TRUE ~ Region))

## get wells column names
##  https://catalogue.data.gov.bc.ca/dataset/e4731a85-ffca-4112-8caf-cb0a96905778


#bcdc_describe_feature("e4731a85-ffca-4112-8caf-cb0a96905778")

# Get the wells which have an OBSERVATION_WELL_NUMBER (and thus are part of PGOWN)
wells <- bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
  #filter() %>% collect()
  filter(!is.na(OBSERVATION_WELL_NUMBER) | WELL_TAG_NUMBER == 61559) %>%
  filter(!(WELL_TAG_NUMBER == 93712)) %>%
  select(OBSERVATION_WELL_NUMBER, #MINISTRY_OBSERVATION_WELL_STAT,
         WELL_DETAILS_URL) %>%
  collect() %>%
  mutate(wells_no = as.numeric(as.character(OBSERVATION_WELL_NUMBER)))

# Manually add obs well 494 which isn't yet in the geo warehouse
wells$OBSERVATION_WELL_NUMBER[wells$GW_WW_SYSID == 61405] = 494
wells$wells_no[wells$GW_WW_SYSID == 61405] = 494

# start of temp fix -------------------------------------------------------
# note : temporary using a fix with a csv with lat longs while wms is down.

#wells <- read.csv(file.path("data","Obswell_Locations_List_Updated_Active_clean.csv")) %>%
#  mutate(OBSERVATION_WELL_NUMBER = obswellcode)%>%
#  select(x, y, OBSERVATION_WELL_NUMBER) %>%
#  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
#  st_transform(., crs = 3005)

# end of temp fix ---------------------------------------------------------


wells_joined <- right_join(wells, wdata ,
                          by = c("wells_no" = "Well_ID"))


##Note the well # 365 Shuswamp Lake Park Deep has two GPS locations in the file.
##As the map is a course scale - selecting the first

duplicate <- wells_joined %>%
  group_by(wells_no, report_date) %>%
  summarise(count = n()) %>%
  filter(count >1)

# this has uncovered more problems with the data as to error in names and duplicate
# well id values
#wells_joined <- wells_joined %>%
#  filter(!OBJECTID == 45143748) #<- this id number changed?

#wells_joined <- full_join(wells, wdata ,
#                           by = c("wells_no" = "Well_ID"))
bc <- bcmaps::bc_bound()


# Create regions based on voronoi tessalation around well locations, grouped by
# 'Region' attribute and merged
wells_regions <- st_union(wells_joined) %>%
  st_voronoi() %>%
  st_sf() %>%
  st_collection_extract("POLYGON") %>%
  st_join(wells_joined[, "Region"]) %>%
  group_by(Region) %>%
  summarise() %>%
  st_intersection(bc) %>%
  mutate()

wells_regions <- wells_regions %>%
  mutate(Region = gsub("/","_", Region),
         Region = ifelse(Region == "Lower Mainland", "South Coast",
                         ifelse(Region == "Vancouver Island", "West Coast",
                                ifelse(Region == "Ominca_Peace" , "Omineca_Peace", Region ))))


## The map was getting the graphs and regions wrong this extra bit of code makes the regions into 6 polygons instead of the 163 (due to islands) that is in wells_regions
SK <- wells_regions %>% filter(Region == "Skeena") %>% st_union() %>% st_sf()
OK_KO <- wells_regions %>% filter(Region == "Okanagan_Kootenay") %>% st_union()%>% st_sf()
OM_PE <- wells_regions %>% filter(Region == "Omineca_Peace") %>% st_union()%>% st_sf()
SC <- wells_regions %>% filter(Region == "South Coast") %>% st_union()%>% st_sf()
TC <- wells_regions %>% filter(Region == "Thompson_Cariboo") %>% st_union()%>% st_sf()
WC <- wells_regions %>% filter(Region == "West Coast") %>% st_buffer(dist = 0) %>% st_union()%>% st_sf()

wells_regions <- rbind(OK_KO,OM_PE,SK,SC,TC,WC)

wells_regions$Region <- c("Okanagan_Kootenay",
                          "Omineca_Peace",
                          "Skeena",
                          "South Coast",
                          "Thompson_Cariboo",
                          "West Coast")

wells_regions <- cbind(wells_regions[,2], wells_regions[,1])
wells_regions <- wells_regions[,c(1,2)]

mapview(wells_regions, zcol = "Region") +
  mapview(wells_joined, zcol = "Region", legend = FALSE)

# export the R objects.

if (!dir.exists("tmp")) dir.create("tmp")
save(list = ls(), file = "tmp/welldata.RData")

