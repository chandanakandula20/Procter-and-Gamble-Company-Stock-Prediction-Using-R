library(forecast)


# Assuming that your CSV file is structured with 'Date' and 'Close' columns for dates and closing prices
stocks_data <- read.csv("stocks_1.csv")


stocks_data
# Assuming 'stocks_data' is your data frame and 'Adj_Close' is the column containing stock prices

stocks_data$Date <- as.Date(stocks_data$Date, format = "%m/%d/%Y")

# Extract year and month from the Date column
stocks_data$Year <- lubridate::year(stocks_data$Date)
stocks_data$Month <- lubridate::month(stocks_data$Date)

# Calculate the mean 'Adj_Close' for each month
monthly_avg <- aggregate(AdjClose ~ Year + Month, data = stocks_data, FUN = mean)

# Rename the columns for clarity
colnames(monthly_avg) <- c("Year", "Month", "Mean_Adj_Close")

# Print the summarized data
print(monthly_avg)

# Create a new column 'MonthYear' by combining 'Year' and 'Month'
stocks_data$MonthYear <- paste(stocks_data$Year, stocks_data$Month, sep = "")

# Calculate the mean 'Adj_Close' for each MonthYear
monthly_avg <- aggregate(AdjClose ~ MonthYear, data = stocks_data, FUN = mean)

# Rename the columns for clarity
colnames(monthly_avg) <- c("MonthYear", "AdjClose")

# Print the summarized data
print(monthly_avg)

#Create Time Series data, plot ts components, and autocorrelation. 
stocks.ts <- ts(stocks_data$AdjClose, 
                start = c(2000, 1), end = c(2023, 9), freq = 12)
stocks.ts
stocks.stl <- stl(stocks.ts, s.window = "periodic")
autoplot(stocks.stl, main = "stocks Time Series Components.")
# identify autocorrelation and plot autocorrelation for different lags.

autocor <- Acf(stocks.ts, lag.max = 12, 
               main = "Autocorrelation for stock prices")
#Check Predictability of the data

# Use Arima() function to fit AR(1) model 
stocks.ar1<- Arima(stocks.ts, order = c(1,0,0))
summary(stocks.ar1)
# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.9899
s.e. <- 0.0080
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

Acf(stocks.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for stocks data")
#Partitioning the Data
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.

length(stocks.ts)


nValid <- 75
nTrain <- length(stocks.ts) - nValid 
train.ts <- window(stocks.ts, start = c(2000, 1), end = c(2000, nTrain))
valid.ts <- window(stocks.ts, start = c(2000, nTrain + 1), 
                   end = c(2000, nTrain + nValid))
train.ts
valid.ts
#APPLYING FORECASTING METHODS

# Develop two-level forecast by combining regression forecast 
#and trailing MA forecast for residuals.

# Develop Regression model with linear trend and seasonality for training partition
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)
# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res
library(zoo)
# Apply trailing MA for residuals with window width k = 4
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 4, align = "right")
ma.trail.res
## Forecast using regression and trailing MA for validation partition

# Create regression forecast with trend and seasonality for validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred
# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid
# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred
##PLOTS
# Plot original data and regression forecast for training and 
# validation partitions.
plot(stocks.ts, 
     xlab = "Time", ylab = "stock price", ylim = c(10,35), 
     bty = "l", xlim = c(2000, 2023), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(2001,35, legend = c("stock price", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")
# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(fst.2level, valid.ts), 3)
##EXPONENTIAL SMOOTHING

## SIMPLE EXPONENTIAL SMOOTHING (SES) WITH PARTITIONED DATA, ALPHA = 0.2.

# Create simple exponential smoothing (SES) for training data.
#model = "ANN", i.e., additive error(A), no trend (N) & no seasonality (N).
ses.orig <- ets(train.ts, model = "ANN", alpha = 0.2)
ses.orig
# make predictions using  SES model for validation period. 
ses.orig.pred <- forecast(ses.orig, h = nValid, level = 0)
ses.orig.pred
#Accuracy for validation partition
round(accuracy(ses.orig.pred$mean, valid.ts), 3)
## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA. 
## OPTIMAL PARAMETERS FOR ALPHA, BETA, AND GAMMA.

# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "AAA", i.e., additive error(A), 
# additive trend (A), & additive seasonality (A). 
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.AAA <- ets(train.ts, model = "AAA")
hw.AAA
#make predictions using this HW model for validation period. 
hw.AAA.pred <- forecast(hw.AAA, h = nValid, level = 0)
hw.AAA.pred
# Plot HW predictions for HW additive model (AAA) optimal smoothing parameters.
plot(hw.AAA.pred$mean, 
     xlab = "Time", ylab = "stocks", ylim = c(10,35), 
     bty = "l", xlim = c(2000, 2023), xaxt = "n",
     main = "Holt-Winter's Additive Model with Optimal Smoothing Parameters", 
     lty = 2, col = "blue", lwd = 2) 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(hw.AAA.pred$fitted, col = "blue", lwd = 2)
lines(stocks.ts)
legend(2001,35, legend = c("stock price", 
                           "Regression Forecast, Training Partition", 
                           "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")

#accuracy for validation data
round(accuracy(hw.AAA.pred$mean, valid.ts), 3)
# Create Holt-Winter's (HW) exponential smoothing for partitioned data with model = "ZZZ"
#i.e., automatic selection of error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ 

#make predictions using this HW model with validation period. 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)
# Plot HW predictions for original data, automatic selection of the 
# model and optimal smoothing parameters.
plot(hw.ZZZ.pred$mean, 
     xlab = "Time", ylab = "stocsk price", ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "Holt-Winter's Model with Automatic Selection of Model Options", 
     lty = 5, col = "blue", lwd = 2) 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(stocks.ts)
legend(2001,35, 
       legend = c("Stock Price", 
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")
##REGRESSION BASED MODELS

#Regression Model with Seasonality for training dataset
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)


# make predictions for ts with seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Stock price",
     ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "Regression Model with Seasonality ", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2001,35, legend = c("Stocks Time Series", "Seasonality Model for Training Data",
                             "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")

#accuracy for validation partition
round(accuracy(train.season.pred$mean, valid.ts), 3)


#Regression Model with Linear Trend and Seasonality for training partition
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)


# make predictions for ts with linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Stock price",
     ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(2001,35, legend = c("Stocks Time Series", 
                             "Linear Trend and Seasonality Model for Training Data",
                             "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")
#accuracy for validation data 
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

#Regression model with quadratic trend and seasonality for training partition
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# make predictions for ts with trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
# Plot ts data, trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Stock price",
     ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue") 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1, lwd = 1)
lines(valid.ts, col = "black", lty = 1, lwd = 1)
legend(2001,35, legend = c("Stocks Time Series", 
                             "Quadratic Trend and Seasonality Model for Training Data",
                             "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 2), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")

#Accuracy for validation partition
round(accuracy(train.quad.season.pred$mean, valid.ts),3)



## CREATE TWO-LEVEL MODEL WITH LINEAR TREND AND SEASONALITY MODEL AND AR(1) RESIDUALS.


# create linear trend and seasonal model for training partition
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(train.lin.season)

# make predictions for ts with linear trend and seasonal model in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Use Acf() function to identify autocorrelation for the model residuals 
# (training and validation sets)
Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for stock price Training Residuals")
Acf(valid.ts - train.lin.season.pred$mean, lag.max = 12, 
    main = "Autocorrelation for Stock price Validation Residuals")


# Use Arima() function to fit AR(1) model for training residuals. The Arima model of 
# order = c(1,0,0) gives an AR(1) model.

res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)

# make prediction of residuals in validation set.
res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

# identify autocorrelation for the training residual of residuals 
#plot autocorrelation for different lags 
Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Stock price Training Residuals of Residuals")

# Create two-level model's forecast with linear trend and seasonality 
# regression + AR(1) for residuals for validation period. 
valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean
# plot ts data, linear trend and seasonality data, and predictions 
# for validation period.
plot(valid.two.level.pred, 
     xlab = "Time", ylab = "Stock price",
     ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "Two-Level Forecast: Regression with Trend
             and Seasonlity + AR(1) for Residuals", lwd = 2,
     col = "blue", lty = 2) 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lwd = 2, lty = 1)
lines(valid.ts, col = "black", lwd = 2, lty = 1)
legend(2000,35, legend = c("stocks Time Series", "Regression for Training Data",
                             "Two Level Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")

#Accuracy for validation data
round(accuracy(valid.two.level.pred, valid.ts), 3)


###ARIMA MODELS

##ARIMA (2,1,2)

# Use Arima() function to fit ARIMA(2,1,2) model.
# Use summary() to show ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(2,1,2)) 
summary(train.arima)

# make predictions for ts with ARIMA model in validation set.    
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

# Using Acf() function, create autocorrelation chart of ARIMA(2,1,2) model residuals.
Acf(train.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of ARIMA(2,1,2) Model Residuals")
#Accuracy for validation data
round(accuracy(train.arima.pred$mean, valid.ts), 3)



##AUTO ARIMA MODEL

# Use auto.arima() function to fit ARIMA model.
# Use summary() to show auto ARIMA model and its parameters.
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# make predictions for ts with auto ARIMA model in validation set.  
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Using Acf() function, create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(train.auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")
#Accuracy for validation dataset
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


#### After comparing the accuracy, below models would be used for future forecasting:
#Two level model with Linear trend and seasonality + Trailing MA
#Holt-Winter's ZZZ model
#Regression Model with Seasonality
#ARIMA (2,1,2)
#Auto ARIMA


##Using Two level model with Linear trend and seasonality + Trailing MA for entire dataset

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(stocks.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 24 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 24, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 4, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 24 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 24, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 24 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level
#Accuracy for the forecast
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, stocks.ts), 3)
round(accuracy((snaive(stocks.ts))$fitted, stocks.ts), 3)


## FORECAST WITH HOLT-WINTER'S ZZZ MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full data set. 
HW.ZZZ <- ets(stocks.ts, model = "ZZZ")
HW.ZZZ

#make predictions using this HW model for 24 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 24 , level = 0)
HW.ZZZ.pred
#Accuracy for forecast
round(accuracy(HW.ZZZ.pred$fitted, stocks.ts), 3)



## FORECAST WITH REGRESSION MODEL WITH SEASONALITY USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

#Regression Model with Seasonality for training dataset
data.season <- tslm(stocks.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(data.season)


# make predictions for ts with seasonality data in validation set.  
data.season.pred <- forecast(data.season, h = 24, level = 0)
#accuracy for forecast
round(accuracy(data.season.pred$fitted, stocks.ts), 3)


## FORECAST WITH ARIMA(2,1,2) USING ENTIRE DATA SET INTO
## THE FUTURE FOR 24 PERIODS.

# Use arima() function to fit seasonal ARIMA(2,1,2) model 
# for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
arima.seas <- Arima(stocks.ts, order = c(2,1,2))
summary(arima.seas)

# Apply forecast() function to make predictions for ts with ARIMA (2,1,2) model 
#for the future 24 periods. 
arima.seas.pred <- forecast(arima.seas, h = 24, level = 0)
arima.seas.pred


# Use Acf() function to create autocorrelation chart of ARIMA (2,1,2)
# model residuals.
Acf(arima.seas$residuals, lag.max = 12, 
    main = "Autocorrelations of Seasonal ARIMA (2,1,2) Model Residuals")
# Plot historical data, predictions for historical data, and seasonal 
# ARIMA(2,1,2) forecast for 24 future periods.
plot(stocks.ts, 
     xlab = "Time", ylab = "Stock price",
     ylim = c(10, 35), 
     bty = "l", xlim = c(2000, 2025), xaxt = "n",
     main = "ARIMA(2,1,2) Model for Entire Data Set") 
axis(1, at = seq(2000,2023, 1), labels = format(seq(2000,2023, 1)))
lines(arima.seas$fitted, col = "blue", lwd = 2)
lines(arima.seas.pred$mean, col = "blue", lty = 5, lwd = 2)
legend(2000,35, legend = c("Stocks Series", 
                             "ARIMA(2,1,2) Forecast", 
                             "ARIMA(2,1,2) Forecast for 24 Future Periods"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# plot on the chart vertical lines and horizontal arrows
# describing training and future prediction intervals.
lines(c(2017.5, 2017.5), c(0, 35))
lines(c(2022, 2022), c(0, 35))
text(2009, 35, "Training")
text(2019.5, 35, "Validation")
text(2024.5, 35, "Future")

#Accuracy for the forecast
round(accuracy(arima.seas.pred$fitted, stocks.ts), 3)



###AUTO ARIMA
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
auto.arima <- auto.arima(stocks.ts)
summary(auto.arima)
round(accuracy(auto.arima$fitted, stocks.ts), 3)
# make predictions for ts with auto ARIMA model for the future 24 periods. 
auto.arima.pred <- forecast(auto.arima, h = 24, level = 0)
auto.arima.pred

# Use Acf() function to create autocorrelation chart of auto ARIMA 
# model residuals.
Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")
# Plot historical data, predictions for historical data, and Auto ARIMA 
# forecast for 24 future periods.



###all model accuracies

round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, stocks.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, stocks.ts), 3)
round(accuracy(data.season.pred$fitted, stocks.ts), 3)
round(accuracy(arima.seas.pred$fitted, stocks.ts), 3)
round(accuracy(auto.arima$fitted, stocks.ts), 3)
round(accuracy((snaive(stocks.ts))$fitted, stocks.ts), 3)
round(accuracy((naive(stocks.ts))$fitted, stocks.ts), 3)

