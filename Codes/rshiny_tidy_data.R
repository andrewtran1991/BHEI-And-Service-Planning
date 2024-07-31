# Purpose: service planning tool and report
# To generate the raw data that sources this app run the "00. Execute_v1" script
# in this folder: S:\Pop\Data\Disparities_Report\Disparities_Analysis\most_recent_analysis\02_Scripts\acs_census_2020

# Turn this off when launching to shinyapps
#setwd("/Users/KateWork/Desktop/CASRC/Rshiny")
rm(list = ls())

# Define UI for application 
library(stringr)
library(plotly)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(DT)
library(formattable)
library(data.table)
library(sf) # spatial data
library(tigris) # geojoin
library(tidyr)
library(dplyr)
library(tibble)
library(shinythemes)
#library(topsis)
library(bslib)
library(shinySelect)
library(shinyjs)
library(knitr)
library(shinyBS)
library(shinymanager)
library(shinycssloaders)
#library(bookdown)
library(readxl)
options(tigris_use_cache = TRUE)


# Load in the service+geo dataset to test app locally
#df <- read_sf("S://Pop//Data//Disparities_Report//Rshiny//sdoh_interpolation_geo.shp") 
#df <- df %>%
#  mutate(GEOID = str_to_title(GEOID))
#no_geo <- readRDS("S://Pop//Data//Disparities_Report//Rshiny//sdoh_interpolation_no_geo.Rda") 
#service_rmd <- readRDS("S:/Pop/Data/Disparities_Report/Rshiny/service_rmd.Rda") 
#service_rmd_short <- readRDS("S:/Pop/Data/Disparities_Report/Rshiny/service_rmd_short.Rda") 

# Load in the service+geo dataset to launch shinyapps.io
df <- read_sf("sdoh_interpolation_geo.shp") 
no_geo <- readRDS("sdoh_interpolation_no_geo.Rda") %>%
  filter(type != "county" & outcome != "total_population_census") %>%
  mutate(sprs_name = ifelse(is.na(sprs_name), "", sprs_name))

service_rmd <- readRDS("service_rmd.Rda") 
service_rmd_short <- readRDS("service_rmd_short.Rda") 

# Merge geos
map_app <- left_join(no_geo, df) %>%
  filter(!is.na(smst_nm))  
map_app <- st_as_sf(map_app)

# Create dataset for report comparators
map_app_type <- map_app %>%
  st_drop_geometry() %>%
  filter(outcome == "under_100_fpl") %>%
  dplyr::select(type, GEOID) %>%
  unique() %>%
  group_by(type) %>%
  group_split() 

# Add technical notes data table
technical_data_table <- service_rmd_short %>%
  filter(outcome != "total_population_census") %>%
  mutate(definition = ifelse(!is.na(acronyms_defined), paste0(definition, acronyms_defined), definition)) %>%
  dplyr::select(source_filter, pop_1, tbl_l_1, definition, source) %>%
  arrange(source_filter, pop_1) %>%
  mutate(definition = str_replace(definition, "HHSA Region|SRA|Zip Code|Census Tract", "area")) %>%
  unique() %>%
  rename("Data Type" = source_filter, "Outcome" = pop_1 , "Category" = tbl_l_1, "Definition" = definition, "Source" = source) 

tech_notes_title <- paste("Variable notes")

# Generate data table
tech_notes <- datatable(technical_data_table, 
                        rownames = FALSE,
                        filter = "top",
                        extensions = 'Buttons',
                        options = list(dom = 'frtipB',
                                       buttons = c('copy', 'excel', 'pdf'),
                                       scrollX = TRUE,
                                       pageLength = -1,  # Show all rows by default
                                       lengthMenu = list(c(15,35,50,-1),
                                                         c(15,35,50,"All"))))


# Save file for QC:
#qc_acs_2022 <- no_geo %>%
#  filter(grepl("Community", source_filter))
#write_xlsx(qc_acs_2022, "qc_acs_2022.xlsx")

