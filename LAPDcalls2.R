##### LAPD Calls Part Deux##################


library(ggplot2)
library(dplyr)
library(scales)

############## Time series of number of calls per day#####################
lapd.data <- read.csv("LAPD_Calls_for_Service_YTD_2015.csv")
Day <- unique(lapd.data$Dispatch.Date)
daytotal <- vector()
for (i in 1:length(Day)){
  daytotal[i] <- nrow(filter(lapd.data,lapd.data$Dispatch.Date==Day[i]))
}

x <- seq(as.Date("2015/1/1"), by = "day", length.out = length(Day))
df <- data.frame(x,daytotal)

ggplot(df,aes(x,daytotal))+geom_line()+theme_grey()+scale_x_date(breaks=date_breaks("months"),labels = date_format("%b"))+labs(title="Number of calls to dispatch per day",y="Number of calls",x="Date")

ggplot(df,aes(x[7:304],daytotal[7:304]))+geom_line()+theme_grey()+scale_x_date(breaks=date_breaks("months"),labels = date_format("%b"))+labs(title="Number of calls to dispatch per day",y="Number of calls",x="Date")


##########ACF and PACF###########
acf(daytotal,lag.max = 30)
pacf(daytotal)

############## Box Cox######################
library(MASS)
t <- 1:length(daytotal)
bc <- boxcox(daytotal~t,lambda = seq(-5,0,.2), plotit = T)
lam <- bc$x[which.max(bc$y)]
lam

############# Transformed series ###############

trans_day <- daytotal^lam
qplot(x,trans_day,geom = "line")+theme_minimal()+scale_y_continuous(name = "transformed number of calls (in thousands)")+xlab("Day")+scale_x_date(breaks=date_breaks("months"),labels = date_format("%b")) 

################### Differencing ##############

diff_trans_day <- diff(trans_day)
qplot(x[2:304],diff_trans_day,geom = "line")+theme_minimal()+scale_y_continuous(name = "differenced transformed number of calls (in thousands)")+xlab("Day")+scale_x_date(breaks=date_breaks("months"),labels = date_format("%b")) 

############ Model Selection ##################

acf(diff_trans_day,lag.max = 30)
pacf(diff_trans_day)

library(TSA)
eacf(diff_trans_day) #arima(1,1,2)?

############## first option of model##############
fittedmodel <- arima(trans_day, order = c(1,1,2))
fittedmodel

hist(fittedmodel$residuals,probability = T) # approx normal, with outlier
shapiro.test(fittedmodel$residuals)
#Shapiro-Wilk normality test
#
#data:  fittedmodel$residuals
#W = 0.98484, p-value = 0.3511
# p-value is biggish, we cannot reject for most common significance levels
qqnorm(fittedmodel$residuals)


# check for white noise of residuals by examining the acf and pacf
#par(mfrow=c(2,1))
acf(fittedmodel$residuals)
pacf(fittedmodel$residuals)
