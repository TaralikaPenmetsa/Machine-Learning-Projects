###Loading necessary packages
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)

library(gridExtra)
library(docstring)
library(readr)


###Loading data
data_master <- read.csv("data_master_1.csv",header =T,sep=",")

###creating a time series data object
sp_500 <- ts(data_master$sp_500, start=c(1995, 1), freq=12)
sp_500
plot.ts(sp_500)

#we will be splitting the data to remove 2015 to use it as test set
sp500_training <- ts(sp_500, start=c(1995, 1), end=c(2014, 12), freq=12)

###Plotting Time series
#to make inferences about important components of the time-series data, such as trend, seasonality and stationarity

plot.ts(sp500_training)

###Test for Statinarity
#Ljung-Box test
Box.test(sp500_training, lag = 20, type = 'Ljung-Box')

#Augmented Dickey-Fuller Test
adf.test(sp500_training)
#we can see that p-value is relatively high, so data is non stationary

###Decomposing time series
sp500_t_decomp <- decompose(sp500_training)
# here we have breaken down our time-series into its seasonal component, trend, and random
plot(sp500_t_decomp)


### DIAGNOSING ACF AND PACF PLOTS
acf(sp500_training)
pacf(sp500_training)

###Transforming our data to adjust for non-stationary
tsDiff <- diff(sp500_training)
# ploting transformed time-series
plot.ts(tsDiff)

###Testing for Stationarity
###Test for Statinarity
#Ljung-Box test
Box.test(tsDiff, lag = 20, type = 'Ljung-Box')

#Augmented Dickey-Fuller Test
adf.test(tsDiff)
# we can see that the result yields a small p-value which makes us reject the null hypothesis suggesting stationarity.

### DIAGNOSING ACF AND PACF PLOTS
acf(tsDiff)
pacf(tsDiff)
#From the above plots we deduce that an Moving Average 1 best fits our data because the ACF cuts off at one significant lag and the PACF shows geometric decay
#we are examining the differenced time-series so we have to use the ARIMA (Autoregressive integrated moving average) model
#thus our model so far is ARIMA(0, 1, 1)

####Building the ARIMA model####
#from exploratory analysis phase suggest that model ARIMA(0, 1, 1) might be best fit
# implementing auto.arima()
fit1 =auto.arima(sp500_training)
summary(fit1)
#The auto.arima() method yields the best model for a time-series based on (AIC) and on running auto.arima() method on dataset we got ARIMA(0, 1, 1) as best fit model

# ARIMA(0, 1, 1)
fit <- Arima(sp500_training, order = c(0,1,1),include.drift = TRUE)
summary(fit)


####Forecasting#####
#forecasting the next 1 years
for_sp500_all <- forecast(fit, h = 12)
for_sp500_all
plot(for_sp500_all)

#forecasting next 3 years
for_sp500_all <- forecast(fit, h = 36)
plot(for_sp500_all)



