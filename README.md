---
title: "Project 2"
author: "Eric He"
date: "April 28, 2017"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r message = FALSE}
library("forecast")
library("tseries")
```

Part 1: The Data
-------

The dataset is once again the motor vehicle collisions in NYC. The original dataset is from https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95; it was downloaded on April 1st, 2017 and transformed in my last project so that the only data left is the number of motor vehicle collisions each day in NYC, beginning from January 1st, 2013 and ending on December 31st, 2016. The data was deseasonalized by month and by day of week by having weekly and monthly averages subtracted. Let's read it in.

```{r}
data <- read.csv("deseasonalizedCPD.csv")
head(data)
```

Here is a time series plot of the data (already deseasonalized), along with the ACFs and PACFs.

```{r}
plot(data$deseason)
Acf(data$deseason)
Pacf(data$deseason)
deseason <- data$deseason[-length(data$deseason)]
```

The data looks stationary, but with a very slight upwards trend, although the ACF and PACF plots do not really say anything either way. We will take a look at the differences.

```{r}
diff.deseasonalized.cpd <- c(NA, diff(deseason))
plot(diff.deseasonalized.cpd)
Acf(diff.deseasonalized.cpd)
Pacf(diff.deseasonalized.cpd)
```

First-order autocorrelations are negative for both the ACF and PACF plots at nearly -0.4,
which could possibly be a sign of overdifferencing. Thus, for the ARIMA, we will use the undifferenced data and include a constant. The ACF and PACF plots both suggest large MA(q) and AR(p) models, but it is likely that a mixed ARMA(p,q) model can do better. We will let AICc decide for p and q up to 4.

Part 2: ARIMA Modeling
-------

```{r}
p <- rep(0:4, times = 5)
d <- rep(1, times = 25)
q <- rep(0:4, each = 5)
pdq <- matrix(c(p,d,q), nrow = 3, byrow = TRUE)
#builds a matrix where each column is a different pdq combination for each combination of p and q from 0 to 4.
aicc <- unlist(lapply(1:25, function(x){Arima(deseason,
pdq[,x], include.constant=TRUE, method = "ML")$aicc}))
#iterates through every possibe combination of p and q from 0 to 4 using the columns of the pdq matrix
```

Here is a table of AICc scores:

```{r}
min(aicc)
aicc.scores <- matrix(c(p, d, q, aicc), ncol = 4, dimnames = list(c(1:25),
c("p", "d", "q", "AICC")))
aicc.scores
```

The minimum AICc yielded the Arima(2,1,2) model.

```{r}
model1 <- Arima(deseason, order = c(2,1,2),
include.constant = TRUE, method = "ML")
summary(model1)
```

The Arima model is given by: Xt = .8906Xt-1 - .1169Xt-2 -1.6211Et-1 + 0.6211Et-2 + Et with a drift of 0.0577.

Every estimated coefficient in the model is statistically significant with the coefficient
estimates being over 2 standard deviations away from 0. 

Here are the fitted values, with the last 10 fitted values printed out:

```{r}
f <- fitted.values(model1)
tail(f, 10)
```

To get a better view of the model's adequacy and to see if a GARCH model is warranted,, we should look at the residuals. 

Part 3: ARIMA Residuals
-------

```{r}
resid <- model1$residuals
plot(resid)
Acf(resid)
Pacf(resid)
```

The time series plot seems to exhibit some level-dependent volatility, with certain areas
much wider than others. The ACF and PACF plots show that the residuals seem to have
statistically significant autocorrelation at lags 5, 10, 14, and 18.

We can verify this using the Ljung-Box statistics. The directions in the homework states to
use lags 12, 24, 26, and 48, but that is with the assumption of monthly data; for daily data,
lags 7, 14, 21, and 28 will be used. The degrees of freedom is 5, since the model estimates 5
coefficients including the constant.

```{r}
lapply(seq(7, 28, length.out = 4), function(x){Box.test(residuals(model1),
lag = x, type = "Ljung-Box", fitdf = 5)})
```

The Ljung-Box test rejects the null hypothesis, which is that there is no autocorrelation within the residuals, for all lags tested. This means that there is very likely to be autocorrelation remaining within the residuals that the ARIMA model was unable to take care of.

Here are an ACF and PACF of the squared residuals.

```{r}
resid.sq <- resid^2
plot(resid.sq, type = "l")
Acf(resid.sq)
Pacf(resid.sq)
```

The ACF and PACF plots have an above average amount of statistically significant autocorrelation in the later lags. These autocorrelations, however, do not account for the extremely obvious correlations in the volatility of the dataset. Thus, a GARCH model for the volatility is warranted.

Part 4: ARCH modeling
-------

Here are the AICc values for the ARCH(q):

```{r}
q2 <- 1:10
N <- length(resid)
loglik <- unlist(
 lapply(1:length(q2), function(x){
 fit1 <- garch(resid, c(0,x), trace = FALSE)
 logLik(fit1)
 }))
loglik <- c(-0.5 * N * (1 + log(2 * pi * mean(resid.sq))), loglik)
q2 <- c(0, q2)
k <- q2 + 1
aicc.garch <- -2 * loglik + 2 * k * N / (N - k - 1)
cbind(q2, loglik, aicc.garch)
```

Here is the AICc for the GARCH(1,1).

```{r}
garch.11 <- garch(resid, c(1,1), trace = FALSE)
loglik.garch.11 <- logLik(garch.11)
k2 <- 2
aicc.garch.11 <- -2 * loglik.garch.11 + 2 * k2 * N / (N - k2 -1)
loglik.garch.11
aicc.garch.11
```

The best model selected by AICc is the ARCH(10) model. Here is a summary of its output:

```{r}
garch.0.10 <- garch(resid, c(0,10), trace = FALSE)
summary(garch.0.10)
logLik(garch.0.10)
```

The Jarque-Bera test, which has null hypothesis that the residuals have 0 skew and kurtosis, has been thoroughly rejected. This suggests that the residuals may have problems with normality.

The Box-Ljung test, which has null hypothesis that the residuals are uncorrelated, is unable to reject the null hypothesis at a 0.05 level. The residuals given by the ARCH model do not seem correlated, which is good.

Not all of the parameters are statistically significant; the a6 parameter, in particular, has a p-value of 1, all but guaranteeing that it is not statistically significant. As a result, its coefficient is very close to zero. Non-statistically significant parameters are a problem when the model is the correct model to use. Here is the formula for the ARCH(10):

h(t) = a0 + a1E^2(t-1) + a2E^2(t-2) + ... + a10E^2(t-10)

h(t) = 2.529e+03 + 1.806e-01 + 4.571e-02 + 2.348e-02 + 3.861e-02 + 2.504e-02 + 2.121e-11 + 3.263e-02 + 3.433e-02 + 6.828e-02 + 5.234e-02

where h(t) is the expected conditional variance at time t given the information set of all shocks before it.

Part 5: 95% one step ahead forecast interval
-------

```{r}
f1 <- forecast(model1, 1)

ht <- garch.0.10$fitted.values[,1]^2
rev.resid.sq <- rev(resid.sq)
h1 <- 2.529 * 10^3 + 1.806 * 10^-1 * rev.resid.sq[1] + 4.571 * 10^-2 * rev.resid.sq[2] + 2.348 * 10^-2 * rev.resid.sq[3] + 3.861 * 10^-2 * rev.resid.sq[4] + 2.504 * 10^-2 * rev.resid.sq[5] + 2.121 * 10^-11 * rev.resid.sq[6] + 3.263 * 10^-2 * rev.resid.sq[7] + 3.433 * 10^-2 * rev.resid.sq[8] + 6.828 * 10^-2 * rev.resid.sq[9] + 5.234 * 10^-2 * rev.resid.sq[10]
h1

f1$mean[1] + c(-1,1) * 1.96 * sqrt(h1)

f1
```

The interval given by the GARCH model (-139.8799, 132.7360) is slightly thinner than the default given by the ARIMA model (-143.7985, 136.6545). This is probably because the most recent data is experiencing a period of relative calm.

```{r}
data$deseason[length(data$deseason)]
```

The last data point of the original dataset, which was left outside of the training dataset, has a value of -81.63417. This is well within the prediction interval given by the ARIMA-ARCH model, so the model does not appear to be too terrible.

Part 6: Volatility Check
-------

Here are the last 10 conditional variances:

```{r}
tail(ht, 10)
```

Here is a plot of the conditional variances.
```{r}
plot(ht, type = "l")
plot(deseason, type = "l")
```

There are two especially large patches of volatility in the neighborhood of data point 400 and 750. The periods of high conditional variances generated by the GARCH model match up with the periods of high volatility within the data for car crashes.

Part 7: Time Series Plot
-------

Here is a time series plot which shows the daily car crash counts, together with the ARIMA-ARCH one-step-ahead 95% forecast intervals based on information available in the previous day.

```{r}
plot(deseason, type = "l")
lines(f + 1.96 * sqrt(ht), lty = 2, col = "red")
lines(f - 1.96 * sqrt(ht), lty = 2, col = "red")
```

The forecast intervals are quite accurate, although they seem to be fairly conservative for much of the data. They are definitely better than the default ones given by the ARIMA model.

Part 8: ARCH Residuals
-------

Here is a normal probability plot of the residuals.

```{r}
resid.arch <- resid / sqrt(ht)
qqnorm(resid.arch)
```

The ARCH residuals seem to have a heavy tails, with especially heavy right tails. Thus, the arch residuals are non-normal, which is also what the results of the Jarque-Bera test implied.

Part 9: Prediction Interval
-------

Here is a count of the number of prediction failures.

```{r}
sum(abs(resid.arch) - 1.96 > 0, na.rm=TRUE)
```

The percentage of prediction interval failures is given by

```{r}
sum(abs(resid.arch) - 1.96 > 0, na.rm = TRUE) / sum(!is.na(resid.arch))
```

Which is very close to the expected failure rate of 5%. In sum, the ARIMA(2,1,2)-ARCH(10) model computed in this analysis can be used to create a forecast and forecast interval of car crash counts for the next day.