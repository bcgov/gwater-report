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

## Load libraries
library(readr) #load data from BC Data Catalogue
library(readxl) #load xlsx files
library(dplyr) # data munging
library(envreportutils)
library(tidyr)
library(stringr)
library(lubridate)


if (!exists("wells_regions")) load("tmp/welldata.RData")

# update regional name changes

wells_joined <- wells_joined %>%
  mutate(Region = gsub("/","_", Region),
         Region = ifelse(Region == "Lower Mainland", "South Coast",
            ifelse(Region == "Vancouver Island", "West Coast",
              ifelse(Region == "Ominca_Peace" , "Omineca_Peace", Region )))) %>%
  select(-c(GW_WW_SYSID, WELL_DETAILS_URL))


wells.df <- data.frame(wells_joined)

# number of wells per regions over time.
well.stats  <- wells.df %>%
  group_by(Region, report_date) %>%
  filter(!inactive == "Y") %>%
  summarise(no.active.wells = length(unique(OBSERVATION_WELL_NUMBER)),
            no.gth.7 = round(sum(Months_since_val > 7, na.rm = TRUE), 1),
            mth.ave = round(mean(Months_since_val, na.rm = TRUE), 1),
            mth.sd = round(sd(Months_since_val, na.rm = TRUE), 1),
            mth.total = round(sum(Months_since_val, na.rm = TRUE), 1),
            no.grad = round(sum(as.numeric(graded), na.rm = TRUE))) %>%
  mutate(pc.gth.7 = round(no.gth.7 / no.active.wells * 100, 1),
         pc.grad = round(no.grad / no.active.wells * 100, 1),
         report_date = ymd(report_date)) %>%
  ungroup()


well.stats$Region = factor(well.stats$Region, ordered = TRUE,
                           levels = c("Skeena", "Omineca_Peace", "Okanagan_Kootenay","Thompson_Cariboo",
                                      "South Coast", "West Coast"))


#write.csv(well.stats, file.path("out", "raw_table.csv"))


# format table - most recent year
#well.table.recent <- well.stats %>%
#  filter(report_date == max(report_date)) %>%
#  select(c(Region, report_date, no.active.wells, no.gth.7, pc.gth.7,
#           mth.ave, mth.sd, no.grad, pc.grad ))


reporting_date = max(well.stats$report_date)

# format table - all years

well.table <- well.stats %>%
  select(c(report_date, no.active.wells, no.gth.7, pc.gth.7, mth.ave, no.grad, pc.grad )) %>%
  group_by(report_date) %>%
  summarise(no.active.wells = sum(no.active.wells),
            no.gth.7 = round(sum(no.gth.7), 1),
            mth.ave = round(mean(mth.ave, na.rm = TRUE), 1),
            #mth.sd = round(sd(mth.ave, na.rm = TRUE), 1),
            pc.grad = round(mean(pc.grad, na.rm = TRUE), 0)) %>%
  mutate(pc.gth.7 = round(no.gth.7/no.active.wells*100, 0))

# add colour code for most recent data well location

wells_joined  <- wells_joined %>%
  group_by(OBSERVATION_WELL_NUMBER) %>%
  filter(report_date == reporting_date & inactive == "N") %>%
  mutate(map_colour = ifelse(Months_since_val > 7, "red", "cyan")) %>%
  ungroup()

save(list = ls(), file = "tmp/wellsum.RData")
