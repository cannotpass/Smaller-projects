#Time Series and Forecasting
#Coursework - 2019

# To consider:
#   -> substitute first 14 measurements with a mean from [15:end]
#   -> do all the measurements for the new set of data from the point above



install.packages("readxl")
install.packages("forecast")
install.packages("ggplot2")
install.packages("smooth")
library(readxl)
library(forecast)
library(ggplot2)
library(smooth)


#Data starts from the 1st of Jan 2017 -> Sunday

setwd("C:/Users/Lukasz/Desktop/Time Series and Forecasting/Coursework")

## Create a daily Date object - helps my work on dates
dates <- seq(as.Date("2017-01-01"), as.Date("2019-01-14"), by = "day")

#Importing data
data <- read_excel('ts_data.xlsx')
data <- data[,2]

## Create a time series object
ts.data <- ts(data,
           start = c(1),
           frequency = 7)

ts.data <- ts(ts.data[1:(length(ts.data)-7)],
              start = c(1),
              frequency = 7)
plot.ts(ts.data)

#Creating vector of numbers of days in consecutive months
mont <- c(31L,28L,31L,30L,31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

#Basics:
#   -> averages: all, 1st year, 2nd year, 3rd year

summary(ts.data)

sd(ts.data)

av_all = mean(ts.data)
av_all

av_2017 = mean(ts.data[1:365])
av_2017

av_2018 = mean(ts.data[366:730])
av_2018

av_2019 = mean(ts.data[730:734])
av_2019


#Averages for Days: [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

daily.averages = c(rep(0.0, 7))
for(i in 1:7){
  daily.averages[i] = mean(ts.data[seq(i, length(ts.data), 7)])
}

daily.averages
plot(daily.averages, type="l")

daily.max <- c()
daily.min <- c()
daily.median <- c()
for(i in 1:7){
  daily.max[i] <- max(ts.data[seq(i, length(ts.data), 7)])
  daily.min[i] <- min(ts.data[seq(i, length(ts.data), 7)])
  daily.median[i] <- median(ts.data[seq(i, length(ts.data), 7)])
}
daily.max
daily.min
daily.median

#Averages for weeks

weekly.averages <- c()
for(i in 1:104){
  weekly.averages = c(weekly.averages, mean(ts.data[seq(i, 7*i)]))
}
weekly.averages <- ts(weekly.averages,
                      start = c(2017,1),
                      frequency = 52)
plot.ts(weekly.averages[1:length(weekly.averages)], type='l')
mean(weekly.averages)
mean(weekly.averages[3:length(weekly.averages)])

#Averages for months

monthly.averages <- c()
monthly.median <- c()
monthly.max <- c()
monthly.min <- c()

a <- 1
for(j in 1:2){
  for(i in mont){
    monthly.averages = c(monthly.averages, mean(ts.data[a : i]))
    monthly.median = c(monthly.median, median(ts.data[a : i]))
    monthly.max = c(monthly.max, max(ts.data[a : i]))
    monthly.min = c(monthly.min, min(ts.data[a : i]))
    a <- a+i
  }
}

monthly.max

mont.av <- c()
mont.med <- c()
mont.max <- c()
mont.min <- c()
for(i in 1:12){
  mont.av[i] <- (monthly.averages[i]+monthly.averages[i+12])/2
  mont.med[i] <- (monthly.median[i]+monthly.median[i+12])/2
  mont.max[i] <- (monthly.max[i])
  mont.min[i] <- (monthly.min[i])
}

mont.av
mont.med
mont.max
mont.min

plot(monthly.averages, type = 'l')
plot(mont.av, type = 'l')

rm(i)
rm(j)
rm(a)

#Plotting for years

plot.ts(ts.data[366:731], type='l')
lines(ts.data[1:365], type='l', col="red")





# ---! Decomposition !---

#   -> decomposition for daily data

test <- ts(ts.data[15:(length(ts.data))],
           start = c(2017,1),
           frequency = 7)

stl.test <- stl(test, s.window="per")
plot(stl.test)

stl.daily <- stl(ts.data, s.window="per")
plot(stl.daily)







# ----! FORECASTING !----

# splitting data into training (first 70%) and validating(remaining 30%) sets
train.set <- ts.data[1:(0.7*length(ts.data))]
val.set <- ts.data[((0.7*length(ts.data))+1) : length(ts.data)]

train.set <- ts(train.set,
                start = c(2017,1),
                frequency = 7)

#Moving averages

par(mfrow = c(1,1))

ma3 <- ma(ts.data, 3)
ma5 <- ma(ts.data, 5)
ma7 <- ma(ts.data,7)
ma14 <- ma(ts.data, 14)
ma28 <- ma(ts.data, 28)

plot(ma3, type = 'l')
lines(ma5, type = 'l', col = "green")
lines(ma7, type = 'l', col = "red")
lines(ma14, type = 'l', col = "magenta")
lines(ma28, type = 'l', col = "orange")

# MEAN
m <- mean(train.set)
t <- train.set[1:length(train.set)]
t <- c(t, rep(NA, 220))
v <- val.set[1:length(val.set)]
v <- c(rep(NA, 513), v)

plot(t, type = 'l', main = "Mean-based forecasting", ylab="value", xlab="day")
lines(v, type = 'l', col = 'blue')
lines(rep(av_all, 733), type = 'l', col = 'red')

err <- v[514:length(v)]
plot(err, type="l", main = "Mean-based forecasting", ylab="squared error", xlab="forecasted day")
err <- err - m
plot(err, type="l", main = "Mean-based forecasting", ylab="squared error", xlab="forecasted day")
err <- (err^2)
plot(err, type="l", main = "Mean-based forecasting", ylab="squared error", xlab="forecasted day")
legend("topleft", c("Training", "Model","Validating"), col=c(1,"red"), lty=c(1,4), cex=0.8) 
sum(err)

err <- v[1:513]
err <- err - m
err <- err^2
sum(err)


# NAIVE FORECAST

par(mfrow = c(1,1))

naive.forecast <- naive(t, h=50)
plot(c(0, 1000, rep(NA, 733)))
lines(naive.forecast$fitted)
lines(v, type = 'l', col = 'red')
naive.forecast

# EXPONENTIAL SMOOTHING

tmp <- c(t[1:513], t[1:220])
exp <- es(tmp, model = "ANN", persistence = 0.8)
plot(tmp, type='l')
plot(t, type = 'l', main = "SES forecasting", ylab="value", xlab="day")
legend("topleft", c("Training", "Model","Validating"), col=c(1,"red"), lty=c(1,4), cex=0.8) 
lines(v, type='l', col='blue')
lines(c(rep(NA, 513), exp$fitted[514:733]), type='l', col='red')
pred <- predict(exp, 220)
plot(pred)

SES.7 <- predict(exp, 228)
SES.7 <- SES.7$forecast[222:228]
SES.7

err <- exp$fitted[514:733]-v[514:733]
err <- err^2
plot.ts(err)
plot(err, type="l", main = "SES forecasting", ylab="squared error", xlab="forecasted day")
sum(err)

# Holt-Winter's FORECAST

plot.ts(ts.data)

train.set <- ts(train.set,
                start = 1,
                frequency = 7)

h.w.forecast <- HoltWinters(train.set, seasonal = "multiplicative")

tmp <- -100+(h.w.forecast$fitted[,1]+h.w.forecast$fitted[,2]+h.w.forecast$fitted[,3]+h.w.forecast$fitted[,4])
#plot(tmp, type='l', col='red')
#tmp <- predict(h.w.forecast, 220)
tmp <- tmp[1:length(tmp)]
tmp <- c(rep(NA, 513), tmp)

plot(t, type = 'l', main = "Holt - Winters' forecasting", ylab="value", xlab="day")
lines(v, type = 'l', col = 'blue')
lines(tmp[1:length(t)], col='orange')
legend("topleft", c("Training", "Model","Validating"), col=c(1,"red"), lty=c(1,4), cex=0.8)

HW.7 <- predict(h.w.forecast, 228)
plot(HW.7)
HW.7 <- HW.7[222:228]
HW.7

err <- tmp[514:733]-v[514:733]
err <- err^2
plot.ts(err)
plot(err, type="l", main = "Holt-Winters' forecasting", ylab="squared error", xlab="forecasted day")
sum(err)



# LINEAR REGRESSSION
y = t[1:513]
x = 1:513
linear <- lm(y ~ x)
a <- linear$coefficients[2]
b <- linear$coefficients[1]

linear

slm.7 <- c()
for(i in 735:741){
  slm.7 <- c(slm.7, a*i + b)
}
slm.7


model <- c()
for(i in x){
  model <- c(model, a*i + b)
}
plot(y, type = 'l')
lines(model, type = 'l', col = 'red')

val.y <- c()
val.x <- 1:733
val.model <- c()
for(i in val.x){
  val.model <- c(val.model, a*i + b)
}
plot(c(y, rep(NA,220)), type='l', main = "Simple linear regression forecasting", ylab="value", xlab="day")
lines(c(rep(NA, 513), val.set), type = 'l', col='blue')
lines(val.model, col = 'red')

err <- val.set - val.model[514:733]
err <- err^2
plot(err, type = 'l')
plot(err, type="l", main = "Linear regression forecasting", ylab="squared error", xlab="forecasted day")
legend("topleft", c("Training", "Model","Validating"), col=c(1,"red", 'blue'), lty=c(1,4), cex=0.8)

sum(err)

#MULTIPLE REGRESSION
# -> day of the period, day of the week, day of the month
x1 <- 1:733
x2 <- c(rep(c(1,2,3,4,5,6,7), 104), 1, 2, 3, 4, 5)
x3 <- c(rep(c(seq(1:31), seq(1:28), seq(1:31), seq(1:30), seq(1:31), seq(1:30), seq(1:31), seq(1:31), seq(1:30), seq(1:31), seq(1:30), seq(1:31)), 2), 1, 2, 3)

mult.reg <- lm(y ~ x1[1:513] + x2[1:513] + x3[1:513])
a1 <- mult.reg$coefficients[2]
a2 <- mult.reg$coefficients[3]
a3 <- mult.reg$coefficients[4]
b <- mult.reg$coefficients[1]

mult.reg

mlr.7 <- c()
x17 <- 735:741
x27 <- c(7, 1, 2, 3, 4, 5, 6)
x37 <- c(5, 6, 7, 8, 9, 10, 11)
for(i in x17){
  mlr.7 <- c(mlr.7, (a1)*x17[i] + (a2)*x27[i] + (a3)*x37[i] + b)
}
mlr.7

val.y <- c()
val.mult <- c()
for(i in 1:733){
  val.mult <- c(val.mult, (a1)*x1[i] + (a2)*x2[i] + (a3)*x3[i] + b)
}

plot(c(y, rep(NA, 220)), type = 'l', main = "Multiple linear regression forecasting", ylab="value", xlab="day")
lines(c(rep(NA,513), val.set), type='l', col = 'blue')
lines(val.mult, col = 'red')
legend("topleft", c("Training", "Model","Validating"), col=c(1,"red", 'blue'), lty=c(1,4), cex=0.8)


err <- val.set - val.mult[514:733]
err <- err^2
plot(err, type = 'l', main = "Multiple linear regression forecasting", ylab="squared error", xlab="forecasted day")
sum(err)

# -> day of the week, day of the month

x12 <- c(rep(c(1,2,3,4,5,6,7), 104), 1, 2, 3, 4, 5)
x22 <- c(rep(c(seq(1:31), seq(1:28), seq(1:31), seq(1:30), seq(1:31), seq(1:30), seq(1:31), seq(1:31), seq(1:30), seq(1:31), seq(1:30), seq(1:31)), 2), 1, 2, 3)

mult.reg.2 <- lm(y ~ x12[1:513] + x22[1:513])
a12 <- mult.reg.2$coefficients[2]
a22 <- mult.reg.2$coefficients[3]
b2 <- mult.reg.2$coefficients[1]

mult.reg.2

val.y.2 <- c()
val.mult.2 <- c()
for(i in 1:733){
  val.mult.2 <- c(val.mult.2, (a12)*x12[i] + (a22)*x22[i] + b2)
}

plot(c(y, rep(NA, 220)), type = 'l', main = "Multiple linear regression forecasting", ylab="value", xlab="day")
lines(c(rep(NA,513), val.set), type='l', col = 'blue')
lines(val.mult.2, col = 'red')

err <- val.set - val.mult.2[514:733]
err <- err^2
plot(err, type = 'l')
sum(err)

#ARIMA
# -> SSARIMA (State Space ARIMA)
ar_1 <- auto.ssarima(train.set)
ar_1
ar_1.pred <- predict(ar_1, 220)

ar_1.for <- (ar_1.pred$forecast)[1:220]

err = val.set - ar_1.for
err = err^2
sum(err)

plot(err, type = 'l', main = "State Space ARIMA forecasting", ylab="squared error", xlab="forecasted day")

# -> MSARIMA (Multiple Seasonal ARIMA)
ar_2 <- auto.msarima(train.set)
ar_2
ar_2.pred <- predict(ar_2, 220)

ar_2.for <- ar_2.pred$forecast[1:220]

err = val.set - ar_2.for
err = err^2
sum(err)

plot(err, type = 'l')

plot(ar_2.pred$forecast)

ar.7 <- predict(ar_2,228)$forecast[222:228]
ar.7

plot(ar_2.pred)

#7-day forecast
ar_1.7days <- predict(ar_1,228)
plot(ar_1.7days$forecast[222:228], type = 'l', main = '7-day forecast', ylab = "value", xlab = "day")
ar_1.7days$forecast[222:228]
