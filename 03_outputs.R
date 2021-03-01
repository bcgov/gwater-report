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

library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
load("tmp/wellsum.RData")
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# Create and overall plot

# plot with % wells validated
#ggplot(well.stats, aes(report_date, 100 - pc.gth.7,color = Region, fill = Region, shape = Region)) +
#  geom_point() + geom_line()+
#  ylim(0,100) +
#  labs(title = "% Wells validated within 7 months",
#       x = "", y = "Percentage of active wells")


# Create Popup plots for report ------------------------------------------


# Regional Plots  ---------------------------------------------------------

# Create list of regions
reg_list <- unique(well.stats$Region)

# Create list for plots
reg_plot_list <- vector(length = length(reg_list), mode = "list")
names(reg_plot_list) <- reg_list

# Create plotting function for regions

temp_plots <- function(reg.data) {
  p1 <- ggplot(reg.data, aes(report_date, 100 - pc.gth.7)) +
    geom_bar(stat = "identity") +
    ylim(0,100) +
    scale_x_date(date_breaks = "year", date_labels = "%Y") +
   # geom_text(aes(label=no.active.wells), vjust = -1) +
    labs(title = "% Active Well Data Validated Within 7 months",
         x = "time", y = "Percentage of wells")
  p1

}


# Create ggplot graph loop
plots <- for (n in reg_list) {
  print(n)
  reg.data <- well.stats %>% filter(Region == n)
  p <- temp_plots(reg.data)
  #name = gsub("/","_",n )
  ggsave(p, file = paste0("output/plots/",n, ".png"))
  reg_plot_list [[n]] <- p
}


## Plot 2 : create overall summary with all data and years.

p1 <- ggplot(well.table, aes(report_date, 100 - pc.gth.7)) +
  geom_bar(stat = "identity") +
  ylim(0,100) +
  geom_text(aes(label=no.active.wells), vjust = -1) +
  labs(title = "% Wells validated within 7 months",
       x = "", y = "Percentage of active wells")

p2 <-ggplot(well.table,  aes(report_date, mth.ave)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label= round(mth.ave, 0), vjust = -1))+
  ylim(0, max(well.table$mth.ave + 0.1* max(well.table$mth.ave))) +
  labs(title = "Average time since validation ",
       x = "", y = "No. of months") +
  geom_hline(yintercept=7, linetype="dashed", color = "red")



saveRDS(reg_plot_list, "reg_plot_list.rds")


