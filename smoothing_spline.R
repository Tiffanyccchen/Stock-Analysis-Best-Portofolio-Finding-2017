library(quantmod)

## read data
tw2330 <- getSymbols("2330.TW", auto.assign=FALSE)
dr_tw2330 <- dailyReturn(tw2330)

### choose the most recent k days
k <- 200
n_days <- nrow(tw2330)
sel <- (n_days-k+1):n_days

tw2330_s <- tw2330[sel,]
dr_tw2330_s <- dr_tw2330[sel]

## plot data
par(mfrow=c(1,2))
### stock price
plot(tw2330_s[,4])
### stock return
plot(dr_tw2330_s)
par(mfrow=c(1,1))

## cature the trend
par(mfrow=c(2,2))
day <- 1:k
day <- day/k
cs <- smooth.spline(day, tw2330_s[,4]) # auto tuning by GCV
plot(1:k, fitted.values(cs), type="l", col=2)
points(1:k, tw2330_s[,4])

cs_df_2 <- smooth.spline(day, tw2330_s[,4], df=2)
plot(1:k, fitted.values(cs_df_2), type="l", col=2)
points(1:k, tw2330_s[,4])

cs_df_8 <- smooth.spline(day, tw2330_s[,4], df=8)
plot(1:k, fitted.values(cs_df_8), type="l", col=2)
points(1:k, tw2330_s[,4])

cs_df_15 <- smooth.spline(day, tw2330_s[,4], df=15)
plot(1:k, fitted.values(cs_df_15), type="l", col=2)
points(1:k, tw2330_s[,4])

residuals <- function(y, yhat) y-yhat

plot(1:k, residuals(tw2330_s[,4], fitted.values(cs)), type="l")
plot(1:k, residuals(tw2330_s[,4], fitted.values(cs_df_2)), type="l")
plot(1:k, residuals(tw2330_s[,4], fitted.values(cs_df_8)), type="l")
plot(1:k, residuals(tw2330_s[,4], fitted.values(cs_df_15)), type="l")
par(mfrow=c(1,1))
