
# Setup -------------------------------------------------------------------

packages <- c("tidyverse", "here", "RtoSQLServer", "readxl", "haven", "spatstat", "data.table", "plotly", "writexl", "ReGenesees")
lapply(packages, require, character.only = TRUE)

# update for each year

source(here("Setup", "config.R"))
source(here("Processing", "ghg_inventory_process.R")) # saves RDA to ghg_inventory_export folder 
# # run scripts
# 
# source(here("Processing", "functions.R"))

# Load data
#load(paste0(ghg_inventory_export, "ghg_data.Rda"))

library(tidyverse)
library(ReGenesees)
library(here)
library(janitor)


# Table 1 - agriculture vs total ------------------------------------------

# Filter the data for 'Total' and 'Agriculture'


filtered_data <- national_total %>%
  filter(`TES Sector` %in% c("Total", "Agriculture"))

# Rename 'Total' to 'Total emissions'
filtered_data$`TES Sector` <- ifelse(filtered_data$`TES Sector` == "Total", "Total emissions", filtered_data$`TES Sector`)

# Extend the years sequence to include 2034
years <- seq(min(filtered_data$Year), 2030)
industries <- unique(filtered_data$`TES Sector`)
complete_data <- expand.grid(Year = years, `TES Sector` = industries)

# Merge with filtered data to ensure all years are represented, filling missing values with NA
complete_data <- complete_data %>%
  left_join(filtered_data, by = c("Year", "TES Sector"))


# remove rows with NAs

complete_data <- complete_data %>% 
  na.omit() %>% 
  pivot_wider(names_from=Year, values_from=Value) %>% 
  mutate(`TES Sector` = if_else(`TES Sector` == "Total emissions", "Total", `TES Sector`))

# pivot wider


table_1_df<-tibble(complete_data)




# Table 2 - agriculture by subsector --------------------------------------
subsector_levels <- c("Arable",
                    "Dairy",
                    "Dairy beef",
                    "Sheep",
                    "Suckler beef",
                    "Other",
                    "Total")


subsector<-subsector_total %>% 
  pivot_wider(names_from=Year, values_from=Value) %>% 
  mutate(Subsector = factor(Subsector, levels = subsector_levels)) %>%
  arrange(Subsector)

table_2_df<-tibble(subsector)



# Table 3 - subsector by source -------------------------------------------

source <- subsector_source %>% 
  mutate('Total agriculture' = rowSums(across(2:7))) %>% 
  bind_rows(
    summarise(
      .,
      across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      across(where(~ !is.numeric(.x)), ~ "Total emissions")
    )
  )

table_3_df<-tibble(source) %>% 
  mutate(across(where(is.numeric), ~ round_half_up(.x, 3)))

