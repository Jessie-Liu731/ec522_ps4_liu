library(tidyverse)
library(fpp3)
library(BVAR)

### 2.1 The data

### 2.1a read in the data
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

sales_plot <- liquor |>
  autoplot(sales) +
  labs(
    title = "Monthly US liquor sales",
    x = "Year", y = "Sales (millions of dollars)"
  )

sales_plot

sales_seasonal_plot <- liquor |>
  gg_season(sales) +
  labs(
    title = "Seasonal plot of monthly US liquor sales",
    x = "Month", y = "Sales (millions of dollars)"
  )

sales_seasonal_plot

###There's clear trend and seasonal pattern in the series. 
###The series is overall trending upward each year.
###It has got an annual pattern in which the sales are highest in December and lowest in January.
###Since the series is trending upward and the seasonal swings do grow with the level of the series, we should use log transformation to stabilize the variance.





