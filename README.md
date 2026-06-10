# RESAS-agriculture-subsector-GHG-inventory-analysis

This project contains scripts that will perform agriculture subsector analysis on GHG Inventory data.
This analysis is reported in: https://www.gov.scot/collections/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use/ 

Steps:

1. Prepare excel workbook from which you will import GHG inventory data, the SRUC subsector proportions and source categories used in the publication

2. Update config script.R with file paths to where you will import data from and export data to (default is the "Data" folder in the project.  Also update latest analysis year:  here(setup, config.R)

3. Run ghg_inventory_analysis.R (here(Analysis, ghg_inventory_analysis.R). The output is an excel workbook (ghgi_storyboard_data.xlsx)  with various tables of data used in the publication.  
4. This script will also run: 
    - publication tables.R  which also runs:
    - ghg_inventory_process.R : here(Processing, ghg_inventory_process.R). This prepares and outputs datasets used for ag stats hub and publication tables (ghg_data.Rda)
    - ghg_inventory import.R: here(Import, import_ghg_inventory.R). This script reads in relevant workbook sheets and reformats data 

For more information please contact agric.stats:gov.scot  
