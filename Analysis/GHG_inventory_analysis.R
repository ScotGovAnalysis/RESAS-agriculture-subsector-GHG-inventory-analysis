library(highcharter)
library(dplyr)
library(tidyr)
library(here)
library (openxlsx)

source(here("Setup", "config.R"))
source(here("Processing", "functions.R"))


# load publication tables
source(here("Publication", "publication tables.R"))


# calculate estimates --------------------------------      
      
# calculate changes in agriculture

agri_change <- table_1_df |>
  calculate_latest_change() |>
  calculate_90s_change() |> 
  select(`TES Sector`, contains(c("percentage_change", "amount")))




# calculate subsector emission proportions out of total agri emissions

years <- c(1990, ghg_inventory_year-1, ghg_inventory_year)

subsector_share <- table_2_df |> 
  calculate_subsector_share(years) |> 
  select(Subsector, `1990`, !!as.character(ghg_inventory_year-1), !!as.character(ghg_inventory_year), contains(c("share")))

# check 

subsector_share <- subsector_share |>
  bind_rows(
    subsector_share |>
      summarise(
        across(where(is.numeric), \(x) sum(x, na.rm = TRUE))
      )
  )

# calculate subsector changes from prev year and 1990
subsector_trend <- table_2_df |> 
  calculate_latest_change() |> 
  calculate_90s_change() |> 
  select(Subsector, `1990`, !!as.character(ghg_inventory_year-1), !!as.character(ghg_inventory_year),
         contains(c("percentage_change", "amount")))



# calculate latest year source share of emissions by subsector

subsector_source_share <- source |>
  filter(Source != "Total emissions") |> 
  select(-`Total agriculture`) |> 
  mutate(
    total = rowSums(across(where(is.numeric)), na.rm = TRUE)
  ) |>
  mutate(
    across(
      where(is.numeric) & !total,
      ~ 100 * .x / total
    )
  ) |>
  select(-total) |> 
  mutate(
  total = rowSums(across(where(is.numeric)), na.rm = TRUE))

# calculate latest year source share of emissions

source_share <- source |>
  select(Source, `Total agriculture`) |>
  mutate(
    `share of emissions` =
      (`Total agriculture` /
         `Total agriculture`[Source == "Total emissions"]) * 100
  )

# calculate agri share of TES sector emissions 

agri_sector_share <- national_total |>
  group_by(Year) |>
  mutate(
    total_emissions = Value[`TES Sector` == "Total"][1],
    emissions_share = (Value / total_emissions)*100
  ) |>
  ungroup() |> 
  filter(`TES Sector` != "Total") |> 
  arrange(Year, desc(emissions_share))


# calculate source share of agri emissions

agri_source_share_timeseries <- source_breakdown |>
  group_by(Year) |> 
  reframe(Category = Category, Value = Value, year_total = sum(Value)) |> 
  mutate(source_share = (Value/year_total)*100)

# calculate change (amount and percentage for emission source)  
agri_source_change <- source_breakdown|> 
  pivot_wider(names_from = Year, values_from = Value)

agri_source_change <- agri_source_change|>  
  calculate_latest_change() |>
  calculate_90s_change()  


# export to workbook -------------------------
# data for storyboarding
# #export xlsx
data <- list("agri_timeseries" = table_1_df,
                   "agri_trend" = agri_change,
                   "agri_source_emissions" = source,
                   "agri_source_share" = source_share,
             "agri_source_trends" = agri_source_change,
                   "subsector_share" = subsector_share,
                   "subsector_trend" = subsector_trend,
                   "subsector_source" = subsector_source,
                   "subsector_source_trends" = subsector_source_share,
                   "subsector_timeseries" = subsector_total,
                   "subsector_source_timeseries" = subsector_source_timeseries,
             "TES_sector emissions" = agri_sector_share
             )
#
# Create a new workbook
wb <- createWorkbook()

# Iterate through the list and add each data frame to a worksheet
for (name in names(data)) {
  addWorksheet(wb, name)                      # Add a worksheet with the name of the list item
  writeData(wb, name, data[[name]])    # Write the data frame to the worksheet
  
  # Set all column widths to 20
  setColWidths(wb, name, cols = 1:ncol(data[[name]]), widths = 20)
}

# Save the workbook to a file
saveWorkbook(wb, paste0(ghg_inventory_export, "ghgi_storyboard_data.xlsx"), overwrite = TRUE)
