library(tidyverse)
library(fpp3)
library(BVAR)
library(knitr)

## Part 2 - A Business Forecasting Horse Race: ARIMA vs. ETS

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
  gg_season(log(sales)) +
  labs(
    title = "Seasonal plot of monthly US liquor sales",
    x = "Month", y = "Log Sales"
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
###The Ljung-Box test also shows that the p-value is essentially, which is less than 0.05, indicating that the residuals are not white noise.

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

## 2.4 Are the Differences Significant?

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

## Part 3 - Forecast Combination

###3a


########################

## Part 4 - A Medium-Scale VAR vs. a BVAR

###4.1a read in data

qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

###4.1b 

growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

###4.1c set up the TS-CV

N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

###We will be producing 120 forecasts for each variable.
###The date of the first forecast will be 1990 Q1 (shown as 1990-03-01) and the date of the last forecast will be 2019 Q4 (shown as 2019-12-01).

###4.2. OLS VARs

var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}


rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

###The VAR(1) has a smaller RMSE for 7 out of 8 variables, meaning that it forecasts better than VAR(4).
###The VAR(1) is basically a form of VAR(4) when the coefficient for all the latter three lags equals to 0 (an extreme form of the shrinkage we talked about in class). In this sense, the difference between the two models are the excessive lag parameters.
###The problem is that the redundant parameters in the VAR(4) could bring more in-sample fitting while risking overfitting the model, bringing noise into the model. Thus, the out-of-sample forecast could perform worse with the estimation errors and noise carried along.

###4.3. A BVAR

# bvar_cv <- function(lambda) {
#   pr <- bv_priors(hyper = "lambda",
#                   mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
#                                                 min = 0.999 * lambda,
#                                                 max = 1.001 * lambda),
#                              b = 0))
#   fc <- NULL
#   for (T in origins) {
#     fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
#                   n_draw = 2000, n_burn = 1000, verbose = FALSE)
#     x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
#     fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
#   }
#   fc
# }
# 
# set.seed(422)
# fc_bvar_03 <- bvar_cv(0.3)
# saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")
# 
# set.seed(422)
# fc_bvar_005 <- bvar_cv(0.05)
# 
# var_results <- tibble(
#   Variable = colnames(y),
#   `VAR(1)` = rmse_by_var(fc_var1),
#   `VAR(4)` = rmse_by_var(fc_var4),
#   `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
#   `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
# )
# 
# saveRDS(var_results, "results/var_results.rds")
# 
# var_results |> knitr::kable(digits = 3)

readRDS("results/var_results.rds") 

###The BVAR(4) with lamba equals to 0.3 has smaller RMSE for all the variables than OLS VAR(4). 
###This means that with the shrinkage, the BVAR with same number of variables and lags as an OLS VAR can tackle the overfitting problem to some extent.
###The BVAR(4) WITH lamba equals to 0.3 has smaller RMSE for 3 out of 8 variables than OLS VAR(1) model. This means that the smaller number of lags included in the VAR(1) can also be helpful to tackle the overfitting problem. 
###When lambda equals to 0.05, our prior becomes much tighter - we are more confident in that guess. Thus, the shrinkage becomes heavier with every lag coefficient getting pulled close to its prior mean of zero, which would be close to a simple benchmark mean model.
###It would not be a fair estimate of how accurately it will forecast in the future. A fair estimate would require testing the chosen model on some new pseudo out-of-sample data and we also need a dm-test to confirm the statistical significance of the difference between the models. 


###4.3c.

e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

e_var4 <- actual[, "unrate"] - fc_var4[, 6]
e_bvar <- actual[, "unrate"] - fc_bvar_03[, 6]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

###Both dm-test results suggest that, for GDP growth and unemployment rate, forecast produced by the BVAR(4) with lambda equals to 0.3 is performing better than the OLS VAR(4) model at either the 5% or 10% level.  
###This suggests that shrinkage does help to tackle the problem brought by adding parameters. However, at the mean time, there are some uncertainty. Since many of the differences are small and not necessarily statitically significant, reading the rakings in that table should be more cautious.






