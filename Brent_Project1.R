# Installing package to read excel file
install.packages("readxl")
library(readxl)

# whole data set of WTI & Brent oil
WTI_and_BRENT_prices_Econometrics_Project <- read_excel("C:/Users/SUPER/Desktop/WTI and BRENT prices-Econometrics Project.xlsx")
View(WTI_and_BRENT_prices_Econometrics_Project)

# extracting the raw data of Brent
brent <- WTI_and_BRENT_prices_Econometrics_Project[,3]

#Installing time-series package 
install.packages("tseries")
library(tseries)

#converting Brent raw data to time-series data
brent_ts <- ts(brent, start = c(1987,5), frequency =12)

#plotting data set
plot (brent_ts)
brent_ts
View(brent_ts)
summary(brent_ts)

#extracting first 390 rows of brent raw data (helpful in forecasting)
brent_data <- WTI_and_BRENT_prices_Econometrics_Project[1:390,3]

#converting the above extracted raw data into time-series
brent_data_ts <- ts(brent_data, start =c(1987,5), frequency =12)

#stationary test(Using Augmented Dickey-Fuller test & plotting)
adf.test(brent_data_ts)
plot(brent_data_ts, col = 2,lwd = 2, xlab = "Years", ylab = "brent price", main = "brent oil")
abline(reg = lm(brent_data_ts~time(brent_data_ts)), col = 4, lwd = 2)
# not stationary

#taking first order difference to make stationary
brent.d <- diff(brent_data_ts)

#stationary test
plot(brent.d, col = 2,lwd = 2, xlab = "Years", ylab = "brent", main = "brent oil first difference")
abline(reg = lm(brent.d~time(brent.d)), col = 4, lwd = 2)
adf.test(brent.d)
#Now stationary
#order of I(d) of ARIMA(p,d,q) = 1

#finding order MA(q)of ARIMA(p,d,q)
#Autocorrelation functions
acf(brent.d)
#lags in decimal(because time-series in month)

#converting the lags in integers
acf1 <-acf(brent.d, plot = FALSE)
acf1
acf1$lag = acf1$lag*12
plot(acf1, col = 3, lwd = 2, xlab = "Lag", ylab = "ACF", main = "ACF of brent.d")
#significant lags are 0,1,2,6,14

#finding order AR(p)of ARIMA(p,d,q)
#Partial Autocorrelation function
pacf(brent.d)

# converting lags in integers
pacf1 <- pacf(brent.d, plot = FALSE)
pacf1
pacf1$lag <- pacf1$lag*12
plot(pacf1, col = 3, lwd = 2, xlab = "Lag", ylab = "PACF", main = "PACF of brent.d")
#significant lags are 0,6,13,25

#Installing forecasting packages
install.packages("forecast")
library(forecast)

#Undifferencing for forecasting the real Brent data
#finding inverse of first order difference of Brent data
brent_oil <- diffinv(brent.d,differences = 1)

#estimating the different ARIMA model
model_1 <- arima(brent_oil, order = c(0,1,0))
model_2 <- arima(brent_oil, order = c(0,1,1))
model_3 <- arima(brent_oil, order = c(0,1,2))
model_4 <- arima(brent_oil, order = c(0,1,6))
model_5 <- arima(brent_oil, order = c(0,1,14))
model_6 <- arima(brent_oil, order = c(6,1,0))
model_7 <- arima(brent_oil, order = c(6,1,1))
model_8 <- arima(brent_oil, order = c(6,1,2))
model_9 <- arima(brent_oil, order = c(6,1,6))
model_10 <- arima(brent_oil, order = c(6,1,14))#problem encountered
model_11 <- arima(brent_oil, order = c(13,1,0))
model_12 <- arima(brent_oil, order = c(13,1,1))
model_13 <- arima(brent_oil, order = c(13,1,2))
model_14 <- arima(brent_oil, order = c(13,1,6))
model_15 <- arima(brent_oil, order = c(13,1,14))#problem encountered
model_16 <- arima(brent_oil, order = c(25,1,0))
model_17 <- arima(brent_oil, order = c(25,1,1))
model_18 <- arima(brent_oil, order = c(25,1,2))
model_19 <- arima(brent_oil, order = c(25,1,6))#problem encountered
model_20 <- arima(brent_oil, order = c(25,1,14))# problem encountered


#finding the AIC & BIC score for best model(having low score)
# dropping temporarily model_10,  model_15, model_19, model_20
scores_AIC <- AIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_11, model_12, model_13, model_14,  model_16, model_17, model_18)
scores_BIC <- BIC(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_11, model_12, model_13, model_14,  model_16, model_17, model_18)

aic <- scores_AIC$AIC
bic <- scores_BIC$BIC
scores <- data.frame(aic, bic)
scores
#model_2, model_3, model_14 fits the score criteria

#choosing best among model_2,model_3 & model_14
#checking white noise condition of the residuals for all the three models

#model_2
pacf(model_2$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-2 PACF")
pacf2 <- pacf(model_2$residuals, plot = FALSE)
pacf2
pacf2$lag <- pacf2$lag*12
plot(pacf2,col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-2 PACF" )
# significant lags (2,6,14,25)

acf(model_2$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-2 ACF")
acf2 <- acf(model_2$residuals, plot = FALSE)
acf2
acf2$lag <- acf2$lag*12
plot(acf2,col = 3, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-2 ACF" )
#one significant lag (0,2,6,14,24)

#ADF-test, line plot and histogram can help to conclude white noise and stationarity

plot (model_2$residuals, xlab ="Time", ylab = "Residuals", main = "Residuals of model 2")
abline(reg = lm(model_2$residuals~time(model_2$residuals)), col = 4, lwd = 2)

hist(model_2$residuals, col="grey", xlab ="residuals", ylab= "frequency", main = "Residuals of model-2")
#slightly left skewed

adf.test(model_2$residuals)
#pass

#model_3
pacf(model_3$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-3 PACF")
pacf3 <- pacf(model_3$residuals, plot = FALSE)
pacf3
pacf3$lag <- pacf3$lag*12
plot(pacf3,col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-3 PACF" )
# few significant lags (6,14,24,25)

acf(model_3$residuals, col = 8, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-3 ACF")
acf3 <- acf(model_3$residuals, plot = FALSE)
acf3
acf3$lag <- acf3$lag*12
plot(acf3,col = 3, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-3 ACF" )
# one significant lag(0)

adf.test(model_3$residuals)
#passed 

plot (model_3$residuals, xlab ="Time", ylab = "Residuals", main = "Residuals of model 3")
abline(reg = lm(model_3$residuals~time(model_3$residuals)), col = 4, lwd = 2)

hist(model_3$residuals, col="grey", xlab ="residuals", ylab= "frequency", main = "Residuals of model-3")
#symmetric

#model_14
pacf(model_14$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-14 PACF")
pacf14 <- pacf(model_14$residuals, plot = FALSE)
pacf14
pacf14$lag <- pacf14$lag*12
plot(pacf14,col = 3, lwd = 2, xlab = "Lag", ylab = "Partial Autocorrelations of error", main = "Model-14 PACF" )
#one significant lag (20)

acf(model_14$residuals, col = 3, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-14 ACF")
acf14 <- acf(model_14$residuals, plot = FALSE)
acf14
acf14$lag <- acf14$lag*12
plot(acf14,col = 3, lwd = 2, xlab = "Lag", ylab = "Autocorrelations of error", main = "Model-14 ACF" )
#one significant lag (0)

#ADF-test, line plot and histogram can help to conclude white noise and stationarity

plot (model_14$residuals, xlab ="Time", ylab = "Residuals", main = "Residuals of model 14")
abline(reg = lm(model_14$residuals~time(model_14$residuals)), col = 4, lwd = 2)

hist(model_14$residuals, col="grey", xlab ="residuals", ylab= "frequency", main = "Residuals of model-14")
#slightly left skewed

adf.test(model_14$residuals)
#pass



#forecasting the models for the next 10 time period
model_2_pred <- forecast(model_2, h = 10, level = c(90, 95))
model_3_pred <- forecast(model_3, h = 10, level = c(90, 95))
model_14_pred <- forecast(model_14, h = 10, level = c(90, 95))


# forecasted values 
model_2_pred
model_3_pred
model_14_pred


#Testing the forecasting
#comparing with the the actual data(last 10 time-periods) which we have left out earlier
actuals <- brent_ts[391:400,]
View(actuals)

#Find error in forecasting from actuals
model_2_error <- ((actuals - model_2_pred$mean)/actuals)
model_3_error <- ((actuals - model_3_pred$mean)/actuals)
model_14_error <- ((actuals - model_14_pred$mean)/actuals)
mean_error <- c(mean(model_2_error), mean(model_3_error), mean(model_14_error))
mean_error
#model_2 has lowest absolute error


#Finding root mean square(lowest will be best)
install.packages("Metrics")
library(Metrics)

rmse(actuals, model_2_pred$mean)
rmse(actuals, model_3_pred$mean)
rmse(actuals, model_14_pred$mean)
# model_3 has lowest rmse

#choose model_3 on the basis of mean error and RMSE and white noise
model_3

#plotting the forecast
plot(model_3_pred)





