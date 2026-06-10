
library(janitor)
# Defining a function which rounds in an expected way (rather than, eg,  rounding 53.2500 to 53.2).
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# functions -------------------------------------------
# year on change

calculate_latest_change <- function(x) {
  prev_year <- as.character(ghg_inventory_year - 1)
  
  x |>
    mutate(
      percentage_change_latest =
        ((.data[[as.character(ghg_inventory_year)]] /
            .data[[prev_year]]) - 1) * 100,
      amount_change_latest =
        .data[[as.character(ghg_inventory_year)]] -
        .data[[prev_year]]
    )
}

# change since 90s

calculate_90s_change <- function(x) {
  x |>
    mutate(
      percentage_change_90s =
        ((.data[[as.character(ghg_inventory_year)]] / .data[["1990"]]) - 1) * 100,
      amount_change_90s =
        .data[[as.character(ghg_inventory_year)]] -
        .data[["1990"]]
    )
}


# calculate subsector share
calculate_subsector_share <- function(x, years) {
  for (yr in years) {
    yr_chr <- as.character(yr)
    share_col <- paste0("share_", yr_chr)
    
    x <- x |>
      mutate(
        !!share_col :=
          (.data[[yr_chr]] / sum(.data[[yr_chr]]))*100, na.rm = TRUE)
    
  }
  x
}


