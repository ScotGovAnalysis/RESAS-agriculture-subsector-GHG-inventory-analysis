#load data

library(aftables)
library(openxlsx)

source("tables/publication tables.R")
source("tables/get-cell-style.R")
source("tables/overwrite-num-cols.R")
current_year <- "2024-25"

cover_list <- list(
  "Title" = paste0("Scottish agriculture greenhouse gas emissions and nitrogen use: ", 
                   current_year),
  "Description" = c(paste0("Data in this workbook relate to the Scottish agriculture greenhouse gas emissions and nitrogen use: 2024-25 ",
                           "statistical release. These data are designated as official statistics in development. They are newly ",
                           "developed statistics undergoing testing."),
                    paste("This workbook contains data on total greenhouse gas emissions and emissions from agriculture and agricultural subsectors.",
                          "The data source is the 'Scottish Greenhouse Gas Statistics 2024'. Data covers the period 1990 to 2024."),
                    paste("This workbook also contains final estimates of average farm greenhouse gas emissions and nitrogen balance and use efficiency from the Farm Business Survey in Scotland.",
                          "Data covers the period from 2019 to the 2024 crop year, more specifically the 2019-20 to the 2024-25 financial year."),
                    paste("Estimates of absolute gross emissions (tCO\u2082e per ha) and emissions intensity (kgCO\u2082e per kg of output) are based on farm production activities and calculated using the carbon footprint calculator Agrecalc.",
                          "The tool uses the latest IPCC Tier I and Tier II in its calculations and is PAS2050 certified. There are different methods by which emissions can be estimated and care should be taken in comparing to outputs of different methodologies."),
                    paste("Methodology improvements have been made for emission estimates for the average farm from the year 2021-22. Previous years will not be revised as not all data are available.",
                          "Average farm emission results for the year 2021-22 onwards are not directly comparable to previous years."),
                    paste("Estimates of nitrogen balance and use efficiency are based on standard estimates of nitrogen content in all farm inputs and outputs where possible. ",
                          "Nitrogen estimates are not made for organic farms, which means a small proportion of the sample are excluded."),
                    paste("Some tables refer to notes. When notes are mentioned the note marker is presented in square brackets. The note text can be found in the Notes worksheet")
                    
                    
  ),
  #                   
  # "Reporting period" = c("Data is reported for accounting years, relating to each crop year.",
  #                        paste0("Accounting years are presented in YYYY-YY format. For example, 1 April 2023 to 31 March 2024 is presented as " , "'", current_year, "'.")),
  # "Rounding" = c("Current financial figures are rounded to the nearest £1. Inflated financial figures are rounded to the nearest pence.",
  #                "Distribution percentages in tables 4 and 5 are rounded to the nearest percent.",
  #                "Totals and percentages may not sum due to rounding."
  #                ),
  "Date of publication" = "9 June 2026",
  "Glossary" = "[Scottish agriculture greenhouse gas emissions and nitrogen use: 2024-25: Glossary](https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-2024-25/pages/glossary)",
  "Useful links" = c("[Scottish Greenhouse Gas Statistics 2024](https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2024/)",
                     "[Scottish agriculture greenhouse gas emissions and nitrogen use: 2024-25](https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-2024-25/)",
                     "[Scottish agriculture greenhouse gas emissions and nitrogen use: methodology](https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-methodology)"),
  "Contact" = "[agric.stats@gov.scot](mailto:agric.stats@gov.scot)"
)

contents_df <- data.frame(
  "Sheet name" = c("Notes", "Table_1", "Table_2", "Table_3", 
                   "Table_4", "Table_5", "Table_6", "Table_7", "Table_8", "Table_9", "Table_10"),
  "Sheet title" = c(
    "Notes",
    "Agricultural and total greenhouse gas emissions (MtCO\u2082e in Scotland, 1990 - 2024",
    "Scottish agriculture greenhouse gas emissions (MtCO\u2082e) by  subsector, 1990 - 2024",
    "Scottish agriculture greenhouse gas emissions (MtCO\u2082e) by  subsector and by source, 2024",
    "Absolute emissions (tCO\u2082e/ha) by farm type, 2019-20 to 2024-25",
    "Cattle livestock farm beef emission intensities (kgCO\u2082e/kg dwt) by farm type, 2023-24 and 2024-25",
    "Sheep livestock farm sheep emission intensities (kgCO\u2082e/kg dwt) by farm type, 2023-24 and 2024-25",
    "Dairy farm milk emission intensities (kgCO\u2082e/kg fat and protein corrected (FPC) milk), 2022-23 to 2024-25",
    "Arable farm cereal emission intensities (kgCO\u2082e/tonne crop) by farm type, 2022-23 to 2024-25",
    "Nitrogen balance (kg N surplus/ha) by farm type, 2019-20 to 2024-25",
    "Nitrogen use efficiency (% nitrogen outputs / nitrogen inputs) by farm type, 2019-20 to 2024-25"
  ),
  check.names = FALSE
)

notes_df <- data.frame(
  "Note number" = paste0("[Note ", 1:5, "]"),
  "Note text" = c("Methodology improvements to data used to estimate Scotland’s share of UK enteric fermentation and manure management emissions have been applied to the entire timeseries (1990 to 2024). As a result, agriculture emissions are slightly higher (<0.1 MtCO2e)  than previously reported for all years, except for 2022 where the revised estimate is slightly lower (<0.01 MtCO2e). Comparisons should only be made with estimates in the latest release.",
                  "Total GHG emissions are allocated to agricultural subsectors using a methodology developed by SRUC: https://www.gov.scot/publications/disaggregating-headline-smart-inventory-figures/",
                  "IPCC emission source categories have been allocated to source categories used in the the Scottish agriculture GHG emissions and nitrogen use publication. More details are available here: https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-methodology",
                  "Due to methodology improvements, data from 2019-20 and 2020-21 are not directly comparable with those from 2021-22 onwards. Previous years will not be revised as not all data are available.",
                  "Nitrogen data is not available for all farms in the farm business survey. Specifically, organic farms are excluded. Data are not directly comparable with data elsewhere which may include all farms in the farm business survey sample."
               
  ),
  check.names = FALSE
)

# agriculture ghgs
table_1_df <- table_1_df 



# subsector
table_2_df <- table_2_df 


# subsector and source
table_3_df <- table_3_df

# ghg emissions

table_4_df <- table_4_df 

# nitrogen balance
table_5_df <- table_5_df
# nitrogen use efficiency
table_6_df <- table_6_df 

# enterprise tables

table_7_df<-table_7_df %>% 
  select(-`2022-23`) %>% 
  filter(`Farm type` != "LFA sheep") %>% 
  filter(Measure == "Average (median)" | Measure =="Sample size")

table_8_df<-table_8_df %>% 
  select(-`2022-23`) %>% 
  filter(`Farm type` != "LFA cattle") %>% 
  filter(Measure == "Average (median)" | Measure =="Sample size")

table_9_df<-table_9_df  %>% 
  filter(Measure == "Average (median)" | Measure =="Sample size")

table_10_df<-table_10_df %>% 
  filter(Measure == "Average (median)" | Measure =="Sample size")

tables <- list(cover_list, contents_df, notes_df, table_1_df, table_2_df, table_3_df, table_4_df,
               table_7_df, table_8_df, table_9_df, table_10_df, table_5_df, table_6_df)


# create aftable

ghg_workbook <- aftables::create_aftable(
  tab_titles = c("Cover", "Contents", "Notes", "Table 1", "Table_2", "Table_3", "Table_4", "Table_5","Table_6", "Table_7", "Table_8", "Table_9", "Table_10"),
  sheet_types = c("cover", "contents", "notes", "tables", "tables", "tables", "tables", "tables", "tables", "tables", "tables", "tables", "tables"),
  sheet_titles = c(
    "Cover",
    "Table of contents",
    "Notes",
    "Agricultural and total greenhouse gas emissions (MtCO\u2082e) in Scotland, 1990 - 2024 [Note 1]",
    "Scottish agriculture greenhouse gas emissions (MtCO\u2082e) by  subsector, 1990 - 2024 [Note 1 and 2]",
    "Scottish agriculture greenhouse gas emissions (MtCO\u2082e) by  subsector and by source, 2024 [Notes 1, 2 and 3]",
    "Absolute emissions (tCO\u2082e/ha) by farm type, 2019-20 to 2024-25 [Note 4]",
    "Cattle livestock farm beef emission intensities (kgCO\u2082e/kg dwt) by farm type, 2023-24 and 2024-25",
    "Sheep livestock farm sheep emission intensities (kgCO\u2082e/kg dwt) by farm type, 2023-24 and 2024-25",
    "Dairy farm milk emission intensities (kgCO\u2082e/kg fat and protein corrected (FPC) milk), 2022-23 to 2024-25",
    "Arable farm cereal emission intensities (kgCO\u2082e/tonne crop) by farm type, 2022-23 to 2024-25",
    "Nitrogen balance (kg N surplus/ha) by farm type, 2019-20 to 2024-25 [Note 5]",
    "Nitrogen use efficiency (% nitrogen outputs / nitrogen inputs) by farm type, 2019-20 to 2024-25 [Note 5]"
  ),
  # blank_cells = c(
  #   rep(NA_character_,4),
  #   "Blank cells indicate that there's no note in that row.",
  #   rep(NA_character_,8)
  #   
  # ),
  # custom_rows = list(
  #   rep(NA_character_, 13)
  # ),
  sources = list(
    rep(NA_character_, 3),
    "[Scottish Greenhouse Gas Statistics 2024](https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2024/)",
    rep("[Scottish agriculture greenhouse gas emissions and nitrogen use: 2024-25](https://www.gov.scot/publications/scottish-agriculture-greenhouse-gas-emissions-and-nitrogen-use-2024-25/)",9) 
  ),
  tables = tables
)

#convert to work book

excel_wb<- aftables::generate_workbook(ghg_workbook)


# edit formatting
invisible(lapply(sheets(excel_wb), function(x) {
  showGridLines(excel_wb, x, showGridLines = FALSE) }))

### Cover sheet ####
cover_column_a_width <- 150

# Set width of column A
setColWidths(excel_wb, sheet = 1, cols = 1, widths = cover_column_a_width)

# Set row heights

setRowHeights(excel_wb, sheet = 1, rows = 6, heights = 35)
setRowHeights(excel_wb, sheet = 1, rows = 7, heights = 35)
setRowHeights(excel_wb, sheet = 1, rows = 8, heights = 50)
setRowHeights(excel_wb, sheet = 1, rows = 9, heights = 35)
setRowHeights(excel_wb, sheet = 1, rows = 10, heights = 35)
setRowHeights(excel_wb, sheet = 1, rows = 11, heights = 18)


### Contents sheet ####
contents_column_b_width <- 120
contents_last_row <- nrow(contents_df) + 3
# Set width of column B
setColWidths(excel_wb, sheet = 2, cols = 2, widths = contents_column_b_width)
# Set row heights
setRowHeights(excel_wb, sheet = 2, rows = 3:contents_last_row, heights = 20)
# Make the sheet name column link to the sheets
invisible(lapply(4:contents_last_row, function(i) {
  writeFormula(excel_wb, sheet = 2, startRow = i, startCol = 1,
               x = paste0('=HYPERLINK("#', contents_df$`Sheet name`[i-3],
                          '!A1", "', contents_df$`Sheet name`[i-3], '")')) }))



### Notes sheet ####
notes_column_b_width <- 130
notes_last_row <- nrow(notes_df) + 3
notes_2line_rows <- c(4)
# Set width of column B
setColWidths(excel_wb, sheet = 3, cols = 2, widths = notes_column_b_width)
# Set row heights
setRowHeights(excel_wb, sheet = 3, rows = 3:notes_last_row, heights = 35)
# Set column A of the table to Center Align
addStyle(excel_wb, sheet = 3, cols = 1, rows = 4:notes_last_row,
         gridExpand = TRUE, style = createStyle(valign = "center"))

### Tables ####
invisible(lapply(4:(nrow(contents_df) + 2), function(x) {
  table <- x - 3
  df <- ghg_workbook$table[[x]]
  last_col <- ncol(df)
  text_cols <- unlist(if_else(table == 1|table==2 | table==3, list(1), list(1:2)))
  num_cols <- (last(text_cols) + 1):last_col
  col_widths <- unlist(case_when(
    table == 1 ~ list(c(16, rep(10, length(num_cols)))),
    table == 2 ~ list(c(16, rep(10, length(num_cols)))),
    table == 3 ~ list(c(25, rep(10, length(num_cols)))),
    table == 4 ~ list(c(25, 20, rep(10, length(num_cols)))),
    table == 5 ~ list(c(25, 25, rep(10, length(num_cols)))),
    table == 6 ~ list(c(25, 25, rep(10, length(num_cols)))),
    table == 7 ~ list(c(25, 25, rep(10, length(num_cols)))),
    table == 8 ~ list(c(25, 25, rep(10, length(num_cols)))),
    table == 9 ~ list(c(25, 20, rep(10, length(num_cols)))),
    table == 10 ~ list(c(25, 20,rep(10, length(num_cols))))))
  
  heading_row <- unlist(case_when(
    table == 1 ~ list(4),
    table == 2 ~ list(4),
    table == 3 ~ list(4),
    table == 4 ~ list(4),
    table == 5 ~ list(4),
    table == 6 ~ list(4),
    table == 7 ~ list(4),
    table == 8 ~ list(4),
    table == 9 ~ list(4),
    table == 10 ~ list(4)))
  last_row <- nrow(df) + heading_row
  body_rows <- (heading_row + 1):last_row

  # Set the widths of all the columns
  setColWidths(excel_wb, sheet = x, cols = 1:last_col, widths = col_widths)
  # Set the heights of all the rows
  setRowHeights(excel_wb, sheet = x, rows = 4:last_row, heights = 16)

  # Set the header row to bold, Align Left for columns A&B, Align Right for
  # the rest of the columns, and have top and bottom borders
  addStyle(excel_wb, sheet = x, cols = text_cols, rows = heading_row,
           gridExpand = TRUE, style = get_cell_style("text", "heading"))
  addStyle(excel_wb, sheet = x, cols = num_cols, rows = heading_row,
           gridExpand = TRUE, style = get_cell_style("number", "heading"))
  # # Fix numbers stored as text
   #overwrite_num_cols(excel_wb, sheet = x, cols = num_cols, rows = body_rows, df)
  # # Set the body rows to Align Left for the text columns and Align Right
  # # for the numeric columns
  addStyle(excel_wb, sheet = x, cols = text_cols, rows = body_rows,
            gridExpand = TRUE, style = get_cell_style("text", "body"))
  addStyle(excel_wb, sheet = x, cols = num_cols, rows = body_rows,
           gridExpand = TRUE, style = get_cell_style("number", "body"))

  # # Set the last body rows to Align Left for columns A&B, Align Right for the
  # #rest of the columns, have a bottom border, and bold if required
  addStyle(excel_wb, sheet = x, cols = text_cols, rows = last_row,
           gridExpand = TRUE, style = get_cell_style("text", "lastrow"))
  addStyle(excel_wb, sheet = x, cols = num_cols, rows = last_row,
           gridExpand = TRUE, style = get_cell_style( "number",
                                                      "lastrow"))

  bold_text_cols <- unlist(case_when(
    table == 1 ~ list(1),
    table == 2 ~ list(1),
    table == 3 ~ list(1),
    table == 4 ~ list(1:2),
    table == 9 ~ list(1:2),
    table == 10 ~ list(1:2)))

  bold_text_rows <- unlist(case_when(
    table == 1 ~ list(5:6),
    table == 2 ~ list(5:11),
    table == 3 ~ list(5:12),
    table == 4 ~ list(5:9),
    table == 9 ~ list(5:9),
    table == 10 ~ list(5:9)))
  # Write data with header style defined above
  addStyle(excel_wb, sheet = x, cols = bold_text_cols, rows = bold_text_rows,
           gridExpand = TRUE, style = get_cell_style("text", "body", total=T))
 
  bold_num_cols <- unlist(case_when(
    table == 4 ~ list(3:7),
    table == 9 ~ list(3:7),
    table == 10 ~ list(3:7)))
  
  bold_num_rows <- unlist(case_when(
    table == 4 ~ list(5:9),
    table == 9 ~ list(5:9),
    table == 10 ~ list(5:9)))
  # Write data with header style defined above
  addStyle(excel_wb, sheet = x, cols = bold_num_cols, rows = bold_num_rows,
           gridExpand = TRUE, style = get_cell_style("number", "body", total=T))
  # make total row bold - this needs fixed
  
  addStyle(excel_wb, sheet = 1, cols = 1, rows = 6,
           gridExpand = TRUE, style = get_cell_style("text", "lastrow", total=T))
  addStyle(excel_wb, sheet = 2, cols = 1, rows = 11,
           gridExpand = TRUE, style = get_cell_style("text", "lastrow", total=T))
  addStyle(excel_wb, sheet = 3, cols = 1, rows = 12,
           gridExpand = TRUE, style = get_cell_style("text", "lastrow", total=T))
}))


excel_filepath <- "scottish_agriculture_ghg_n_use_data_tables.xlsx"
openxlsx::saveWorkbook(excel_wb, excel_filepath,
                       overwrite = T)
