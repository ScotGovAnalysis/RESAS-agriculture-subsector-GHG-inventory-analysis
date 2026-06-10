
# Setup -------------------------------------------------------------------

packages <- c("tidyverse", "here", "RtoSQLServer", "readxl", "haven", "spatstat", "data.table", "plotly", "writexl", "ReGenesees")
lapply(packages, require, character.only = TRUE)

# update for each year

source(here("Setup", "config.R"))

# run import
source(here("import", "import_ghg_inventory.R"))



# manure breakdown ---------------------------------------------------------
# SRUC analysis uses old IPCC categories which included direct NOx manure emissions.
# GHGI moved to Common Reporting Table (CRT) categories in 2025, which didn't include direct Nox (rather direct non-methane).
# Need to calculate by taking difference between total manure and direct methane manure emissions.
#NB: why not just take values for rows where LegacyIPCCCode column contains "Manure_Management_Non-methane_"??


agri_inventory <- agri_inventory |> filter(ConvertTo == "GWP CO2_AR5" 
) |> mutate(Emission = as.numeric(Emission))


manure <- agri_inventory |> filter(SourceName %in% c("Excreta",
                                                     "Managed Manure",
                                                     "Digestate")) |> # future query: why are we adding digestate?
  group_by(EmissionYear) 


# add together to get total direct manure emissions
direct_methane_manure <- summarise(manure, direct_methane_manure = sum(Emission))

# get NOx direct manure
# first get total manure emissions 
total_manure <-  agri_inventory |> filter(CRT_Category %in% c("3B1a", 
                                                                "3B1b",
                                                                "3B2",
                                                                "3B3",
                                                                "3B4c",
                                                                "3B4d",
                                                                "3B4e",
                                                                "3B4g")) |> group_by(EmissionYear) |> 
  summarise(total_manure = sum(Emission))

# join and subtract direct methane manure from manure total
direct_nox_manure <- full_join(total_manure, direct_methane_manure, by = "EmissionYear") |> 
  mutate(direct_nox_manure = total_manure - direct_methane_manure)


# format to join inventory data

direct_nox_manure <- direct_nox_manure |> pivot_longer(
  cols = -EmissionYear, names_to = "SourceName", values_to = "Emission"
) |> # add SRUC Name
  mutate(`SRUC Name` = case_when (SourceName == "direct_methane_manure" ~"Manure management - methane",
                                  SourceName == "direct_nox_manure" ~ "Manure management - nitrous oxide, direct",
                                  .default =NA_character_),
         Category = "Manure management") |> 
  select(-SourceName)



# Map CRT to SRUC (Legacy IPCC) --------------------------------------------------------------
agri_inventory <- left_join(agri_inventory, inventory_data[["Source categories in report"]], by = "CRT_Category_Description")

# replace methane and direct NOx manure with calculated
manure_fix <- agri_inventory |> filter (!CRT_Category %in% c("3B1a",
                                                        "3B1b",
                                                        "3B2",
                                                        "3B3",
                                                        "3B4c",
                                                        "3B4d",
                                                        "3B4e",
                                                        "3B4g") ) |> 
  bind_rows(direct_nox_manure) 

# 
# # sum rows in publication categories
# pub_categories <- manure_fix |> group_by(Category, EmissionYear) |> 
#   summarise(EmissionktCO2e= sum(Emission, na.rm = T))

# sum rows in sruc categories
sruc_categories <- manure_fix |> group_by(Category,`SRUC Name`, EmissionYear) |> 
  summarise(EmissionktCO2e= sum(Emission, na.rm = T)) |> 
  # remove total manure - will double count
  filter(!is.na(`SRUC Name`)) |> 
  rename(`SRUC name` = `SRUC Name`) |> 
  mutate(EmissionMtCO2e= EmissionktCO2e/1000)

# add proportions for subsectors ----------------------------------------------------

# filter out total from sruc analysis table
sruc_name <- inventory_data[["SRUC analysis - long"]]
sruc_name <- sruc_name |> filter(`SRUC name` != "Total")
sruc_categories <- left_join(sruc_categories, sruc_name, by = ("SRUC name"), relationship = "many-to-many") |> 
  mutate(subsector_emissions = EmissionMtCO2e*Proportion)# allocate proportions


# check if it adds to source total emissions

total_check <- sruc_categories |> group_by(`SRUC name`, EmissionYear) |> 
  summarise(total_source_emissions = first(EmissionMtCO2e)) 


total_sub__check <- sruc_categories |> group_by(`SRUC name`, EmissionYear) |> 
  summarise(total_sub_emissions = sum(subsector_emissions)) 

check <- left_join(total_check, total_sub__check, by = c("EmissionYear", "SRUC name")
)  
check <- check |>   mutate(diff = abs(total_source_emissions - total_sub_emissions)
  )                   

# produce  subsector breakdown using categories in report ----------------------------------------------------------
# add subsector

sruc_categories <- sruc_categories |> 
  rename(`SRUC subsector` = Subsector) |> 
  left_join(inventory_data[["Subsector categories in report"]], by = "SRUC subsector" )

# agri emissions by subsector and source
subsector_source_breakdown <- sruc_categories |> 
  group_by(EmissionYear, Category, `Subsector in report`) |> 
  summarise(EmissionMtCO2e = sum(subsector_emissions))

# agri emissions by subsector
subsector_breakdown <- sruc_categories |> 
  group_by(EmissionYear, `Subsector in report`) |> 
  summarise(EmissionMtCO2e = sum(subsector_emissions))

# agri emssions by source
source_breakdown <- agri_inventory |>
  group_by(EmissionYear, Category) |> 
  summarise(total_source_emissionsMtCO2e = sum(Emission)/1000)
  
  

# check against total agriculture ----------------------
total_agri_timeseries <- agri_inventory |> group_by(EmissionYear) |> 
  summarise(total_agri_emissionsMtCO2e = sum(Emission)/1000)

total_check <- subsector_breakdown |> group_by(EmissionYear) |> 
  summarise(total = sum(EmissionMtCO2e))

total_check <- left_join(total_check, total_agri_timeseries, by = "EmissionYear") |> 
  mutate(diff =total_agri_emissionsMtCO2e - total ) 

ifelse(total_check$diff >0.0000000001, print(paste("Total agriculture emissions is not equal to sum of total agri subsector emissions for year",
                                                   total_check$EmissionYear)), paste("Check passed - total agriculture matches total sum of subsector emissions for year",
                                                                                     total_check$EmissionYear))
# produce TES Sector results -------------------------------
all_inventory <- all_inventory|> filter(ConvertTo == "GWP CO2_AR5" 
) |> mutate(Emission = as.numeric(Emission))

TES_sector <- all_inventory |> 
  group_by(EmissionYear,TES_Sector) |> 
  summarise(total_emissionsMtCO2e = sum(Emission/1000))

# produce total Scotland result---------------------------------
scotland_total<- all_inventory |> group_by(EmissionYear) |> 
  summarise(total_emissionsMtCO2e = sum(Emission/1000)) |> 
  mutate(TES_Sector = "Total")

# add to TES Sector results
TES_sector <- rbind(TES_sector, scotland_total)

# export for tables and charts ----------------------

# total emissions by TES Sector
national_total <- TES_sector |> rename(Year = EmissionYear,
                                           `TES Sector` = TES_Sector,
                                           Value = total_emissionsMtCO2e) |> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year))

# Latest year subsector emissions by source
subsector_source <- subsector_source_breakdown |>  filter(EmissionYear == ghg_inventory_year) |># just current year 
  ungroup() |> 
  select(-EmissionYear) |>  pivot_wider(names_from = `Subsector in report`, values_from = EmissionMtCO2e) |> 
  mutate(Source = case_when(Category == "Other" ~ "Other emission source",
         .default =as.character(Category))) |> 
  select(Source, everything()) |> 
  select(-Category)

# timeseries subsector emissions by source
subsector_source_timeseries <- subsector_source_breakdown |> 
  select(-EmissionYear) |>  pivot_wider(names_from = `Subsector in report`, values_from = EmissionMtCO2e) |> 
  mutate(Source = case_when(Category == "Other" ~ "Other emission source",
                            .default =as.character(Category)),
         Year = EmissionYear) |> 
  select(Year, Source, everything()) |> 
  ungroup() |> 
  select(-Category, -EmissionYear) |> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year))


# timeseries subsector emissions
subsector_total <- subsector_breakdown |> 
  rename(Year = EmissionYear,
         Subsector = `Subsector in report`,
         Value = EmissionMtCO2e)|> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year))

# agri gas timeseries (emissions by pollutant)
agri_gas <- agri_inventory |>  group_by(EmissionYear, Pollutant) |> 
  summarise(Value = sum(Emission/1000)) |> 
  mutate(Gas = case_when(Pollutant == "N2O" ~ "Nitrous oxide",
                         Pollutant == "CO2" ~ "Carbon dioxide",
                         Pollutant == "CH4" ~ "Methane")) |> 
  rename(Year = EmissionYear) |> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year))
# add total:  
gas_total <- total_agri_timeseries |> 
  rename(Year = EmissionYear,
         Value =total_agri_emissionsMtCO2e) |> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year),
         Gas = "Total")

agri_gas <- rbind(agri_gas, gas_total) |> 
  select(Gas, Year, Value) |> 
  arrange(Gas)

# agri emissions by source timeseries 
source_breakdown <- source_breakdown |> 
  rename(Year = EmissionYear,
         Value = total_source_emissionsMtCO2e)|> 
  filter(Year != "BaseYear") |> 
  mutate(Year = as.numeric(Year)) |> 
  mutate(Category= case_when(Category == "Other" ~ "Other emission source",
                            .default =as.character(Category))) 


# save data to ADM ----------
# datasets used in Ag Stats Hub too

save(agri_gas, national_total, subsector_source, subsector_total, subsector_source_timeseries, source_breakdown, file = paste0(ghg_inventory_export, "ghg_data.RDa"))
# save to ag stats hub folder
# save(agri_gas, national_total, subsector_source, subsector_total, subsector_source_timeseries, source_breakdown, 
#      file = paste0("//s0196a/ADM-Rural and Environmental Science-Farming Statistics/Agriculture/Source/GHG Inventory/GHGI ", ghg_inventory_year, "/ghg_data.RDa"))
