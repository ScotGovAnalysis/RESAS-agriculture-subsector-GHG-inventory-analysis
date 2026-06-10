# Setup -------------------------------------------------------------------

packages <- c("tidyverse", "here", "RtoSQLServer", "readxl", "haven", "spatstat", "data.table", "plotly", "writexl")
lapply(packages, require, character.only = TRUE)

# update for each year

source(here("Setup", "config.R"))


# import-------------------------------------------------------------------
sheet_list <- c("Year",
                "Source categories in report",
                "Subsector categories in report",
               # "Scotland By Source", # pivot table of "by Source"
                "By Source data", # all uk inventory data
               # "Manure breakdown",
                "SRUC analysis - long",
                "table 1 - methodology",
                "table 2 - methodology")


inventory_data <- map(
  sheet_list,
  ~ read_xlsx(ghg_inventory_import, sheet = .x)
)


names(inventory_data) <- sheet_list

# get scottish data ---------------------------------------------------------

# set sensible header row 

header_row <- which(apply(inventory_data[["By Source data"]], 1, function(x)
  any(str_detect(as.character(x), "EmissionYear"))
))

inventory_data[["By Source data"]] <- inventory_data[["By Source data"]]|>
  slice((header_row + 1):n()) |>
  setNames(unlist(inventory_data[["By Source data"]][header_row, ]))


all_inventory <- inventory_data[["By Source data"]] |> filter(RegionName == "Scotland")

agri_inventory <- all_inventory |> filter(TES_Sector == "Agriculture")

