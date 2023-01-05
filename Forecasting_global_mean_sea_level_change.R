library(tidyverse)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(fpp3)
library(repr)
library(viridisLite)
library(viridis)
library(forcats)
library(ggplot2)
library(cowplot)
library(AER)
library(forecast)
library(dplyr)

#Next, import the data as sea_level
sea_level <- read.csv("/Users/viktoriasundelin/Downloads/sea_levels_2015.csv")

str(sea_level)

sea_level<-sea_level[-c(1:1344),]


#Looking at the structure of the data set "sea_level" it is correct to alter the data frame:
# -Rename the Time column to Years.
#- The GMSL.uncertainty is a long name; could be changed to GMSLU.
#- The Time column variable is character and should be set to the universal date format.

#Rename the columns
sea_level <- rename(sea_level, year = Time, GMSLU = GMSL.uncertainty)

#Set the column format to the international format
sea_level$year = as.Date(sea_level$year)

# The next step on data cleaning is to search if there is any missing value.
sea_level %>%
  summarise(na_year = sum(is.na(year)),
            na_GMSL = sum(is.na(GMSL)),
            na_GMSLU = sum(is.na(GMSLU)))

# first, get a visualization on the mean uncertainty of the data gathered throughout the years to 
# have a better understanding of how accurate the data gathered is. Next, visualize the Global Mean 
# Sea Level in order to deduce if the sea level is on the rise.

ggplot(sea_level, aes(x=year, y=GMSLU)) +
  geom_jitter()+
  geom_line(size= 1)+
  labs(title = "Time Plot: Global Mean Sea Level Uncertainty", subtitle= "Data from 1992 to 2013")


ggplot(data = sea_level, aes(x=year, y=GMSL))+
  geom_area(alpha=0.5)+
  geom_line()+
  ylab("GMSL")+
  labs(title= "Time Plot: Global Mean Sea Level", subtitle= "Data from 1992 to 2013")
#   geom_smooth(method = lm, color="#404040",alpha=0.2,se=FALSE)

# Looking at the Global mean sea level, there is a noticeable trend going upwards. By watching the trend, it can be concluded that the growth is based on a linear regression


# Box cox transformation
sea_reg <- lm(sea_level$GMSL ~ sea_level$year)
summary(sea_reg)


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(sea_reg)

car::ncvTest(sea_reg)  # Breusch-Pagan test

# Add 10 as a constant 

sea_level_pos <- mutate(sea_level, GMSL = GMSL+10) 

#transform
distBCMod <- caret::BoxCoxTrans(sea_level_pos$GMSL)
print(distBCMod)

sea_level_tr <- cbind(sea_level_pos, GMSL_new=predict(distBCMod, sea_level_pos$GMSL)) # append the transformed variable to cars
head(sea_level_tr) # view the top 6 row

# Test transformed data
sea_reg_tr <- lm(sea_level_tr$GMSL_new ~ sea_level$year)
summary(sea_reg_tr)


par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(sea_reg_tr)

car::ncvTest(sea_reg_tr)  # Breusch-Pagan test


ggplot(data = sea_level, aes(x=year, y=GMSL))+
  geom_area(alpha=0.5)+
  geom_line()+
  ylab("GMSL")+
  labs(title= "Time Plot: Global Mean Sea Level", subtitle= "Data from 1992 to 2013")+
  geom_smooth(method = lm, color="#404040",alpha=0.2,se=FALSE)


#Data has a strong trend. Eliminate the trend and investigate transformation on a monthly basis
Y <- ts(sea_level_tr$GMSL_new, start = c(1992,1), frequency = 12)


forecast::autoplot(Y) +
  ggtitle("Time Plot : Global Mean Sea Level", subtitle= "Data from 1992 to 2013") +
  ylab("Global Mean Sea Level")

forecast::autoplot(DY) +
  ggtitle("Time Plot Differentiated data: Global Mean Sea Level", subtitle= "Data from 1992 to 2013") +
  ylab("Global Mean Sea Level")

#Eliminated trend
DY <- diff(Y)

par(mfrow=c(1,1)) # init 4 charts in 1 panel

# Plotting ACF Y
AutoCorrelation_y <- acf(Y, plot = FALSE)
plot(AutoCorrelation_y, main = "Autocorrelation: Global Mean Sea Level")


# Plotting ACF DY
AutoCorrelation_DY <- acf(DY, plot = FALSE)
plot(AutoCorrelation_DY, main = "Autocorrelation of differentiated data: Global Mean Sea Level")


# Classical decomposition
Y %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical decomposition")

DY %>% decompose(type="additive") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical decomposition: Transformed data")

#KPSS not eliminated trend
library(urca)
Y %>% ur.kpss(type=c("tau")) %>% summary()

#KPSS eliminated trend
library(urca)
DY %>% ur.kpss(type=c("tau")) %>% summary()

#ADF not eliminated trend
library(urca)
Y %>% ur.df(c("trend")) %>% summary()

#ADF not eliminated trend
library(urca)
Y %>% ur.df(c("drift")) %>% summary()

#ADF eliminated trend
library(urca)
DY %>% ur.df(c("trend")) %>% summary()

#ADF eliminated trend
library(urca)
DY %>% ur.df(c("drift")) %>% summary()

# Deterministic or Stochastic?


# Splittting train and test data
train<-window(DY, start=c(1992,1), end=c(2000,12))
test<-window(DY, start=c(2001,1))


forecast::autoplot(train) + 
  ggtitle("Time Plot : Global Mean Sea Level Per month", subtitle="Transformed data") +
  ylab("Global Mean Sea Level")


forecast::ggseasonplot(Y) +
  ggtitle("Seasonal Plot: Change in Global Mean Sea Level")+
  ylab("Global Mean Sea Level")

train_ts <- as_tsibble(train)
test_ts <- as_tsibble(test)
DY_ts <- as_tsibble(DY) 
Y_ts <- as_tsibble(Y)

#-------------------------------------------------ACF-and-PAC--------------------------------

plot.a <- ACF( DY_ts ) %>%
  autoplot() +
  labs( title = "ACF")

plot.b <-PACF(DY_ts) %>%
  autoplot() +
  labs( title = "PACF")
plot_grid(plot.a, plot.b, ncol = 1)

#____________________________________________________ARIMA________________________________

# Fit the arima model
#autoarima <- auto.arima(train, allowdrift=T)
#autoarima

fit_arima <- auto.arima(train, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) #Residual SD =2.683


guessed_arima <- Arima(train, order=c(1,0,0), seasonal=list(order=c(1,0,0)))

guessed_arima


fit_arima

fit_arima %>%
  forecast(h=48) %>%
  autoplot()


arima_f =forecast(fit_arima, h=48)
checkresiduals(fit_arima)

guessed_arima %>%
  forecast(h=48) %>%
  autoplot()

guessed_arima_f =forecast(guessed_arima, h=48)
checkresiduals(guessed_arima)


autoplot(guessed_arima_f, include = 10) +
  ylab("GMSL")

autoplot(arima_f, include = 10) +
  ylab("GMSL")
#-----------------------------------------------ETS-----------------------------
# Fit the ets model


train_Y<-window(Y, start=c(1992,1), end=c(2000,12))
test_Y<-window(Y, start=c(2001,1))




m_ets<-forecast(ets(train_Y), h=48)

summary(m_ets)
autoplot(m_ets)

m_ets<-forecast(ets(Y), h=48)
summary(m_ets)
autoplot(m_ets)
checkresiduals(m_ets)
#-----------------------------------------------EVALUATION----------------------
# Checking accuracy on both Arima and ETS
accuracy(arima_f, test)
accuracy(guessed_arima_f, test)

accuracy(m_ets, test_Y)


#---------------------------------------------FULL DATA FORECAST----------------

sea_level <- rename(sea_level, year = Time, GMSLU = GMSL.uncertainty)

#Set the column format to the international format
sea_level$year = as.Date(sea_level$year)

Y <- ts(sea_level$GMSL, start = c(1992,1), frequency = 12)


arima=Arima(Y, order=c(4,1,0), seasonal=list(order=c(0,1,1), period=12))

arima %>%
  forecast(h=48) %>%
  autoplot()

#----------------------------------------------Stochastic-and-deterministic-----
trend <- seq_along(train)
(fit1 <- auto.arima(train, d=0, xreg=trend))
(fit2 <- auto.arima(train, d=1))


fc1 <- forecast(fit1, xreg = length(train) + 1:10)
fc2 <- forecast(fit2, h=12)
autoplot(train) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))