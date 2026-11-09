library(tidyverse)
library(fpp3)
library(BVAR)
library(knitr)

## 2.1 The data

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

### 2.1b plot the data

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

## 2.2 Fit Both Models

### 2.2a fit ARIMA and ETS models

liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

###The ARIMA model is ARIMA(1,0,2)(2,1,1) with a seasonal period of 12.
###The ETS model is ETS(A,A,A), an additive Holt-Winters model.
###The ARIMA model gets one seasonal difference and no regular difference.
###The ETS model has an additive error component, an additive trend component and an additive seasonality component.

### 2.2b ljung-box test for residuals in ARIMA model

liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 24, dof = 1+2+2+1)

###The residuals do somewhat look like white noise, but by observing the ACF we could see that the series can be significantly correlated  with some seasonal lags. 
###The Ljung-Box test also shows that the p-value is 0.001, which is less than 0.05, indicating that the residuals are not white noise.

## 2.3 The Horse Race

###2.3a expanding-window TS-CV

liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

#liquor_cv_fit <- liquor_cv |>
#  model(snaive = SNAIVE(sales),
#        ets    = ETS(log(sales)),
#        arima  = ARIMA(log(sales)))

#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")

###There will be 216 forecast origins in total. The first forecast origin will be December 2001, and the last forecast origin will be November 2019. 

###2.3b produce one-month-ahead forecasts and compute accuracy

liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) 

###The first forecast will be January 2002, and the last forecast will be December 2019. 

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE)) |>
  knitr::kable(digits = 2)

###The ARIMA model has the lowest RMSE and MAE, indicating that it forecasts most accurately among the three models. 
###The two metrics agree.

### 2.4 Are the Differences Significant?

###ARIMA v. ETS
liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

###ARIMA v. SNAIVE
forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

###For the comparison between ARIMA and ETS, the DM statistic is -2.91 with its absolute value larger than 1.96. Thee p-value is 0.004, which is less than 0.05.
###For the comparison between ARIMA and SNAIVE, the DM statistic is -10.76 with its absolute value larger than 1.96. The p-value is less than 2.2e-16, which is far less than 0.05.
###Both DM tests' results indicate that the difference in forecast accuracy between the two models is statistically significant. In other words, the ARIMA model is significantly more accurate than the ETS and SNAIVE models.
###If I were the liquor store chain, I will use the ARIMA model to forecast the monthly liquor sales in the future.





