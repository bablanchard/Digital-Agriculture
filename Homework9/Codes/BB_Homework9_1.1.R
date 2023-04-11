library(ggplot2)
library(tidyverse)
library(timeSeries)
library(fpp2)
library(forecast)
library(stats)

# Specify the path to the directory containing the CSV file
path <- "C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Data/"

# Load the data from the CSV file using the specified path
data <- read.csv(paste0(path, "Month_Value_1.csv"), header = TRUE)
head(data)
str(data)

#get amount of NAs
data%>%
  is.na()%>%
  sum()

#subset by the nonmissing data
mydata<-na.omit(data)
head(mydata)
#double check
nrow(mydata)
mydata%>%
  is.na()%>%
  sum()

#============ Summary Statistics and Saving the data =====================================
SumStats=describe(mydata,na.rm=TRUE,trim=0.1,quant=c(.25,.75))
write.csv(SumStats,paste0("C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Results/",mydata$Date[nrow(mydata)],"/Summary_Statistics.csv"))
write.csv(mydata,"C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Data/Cleandata.csv")

#Declare revenue column as time series data
revenue_tseries<-ts(mydata[,2], start = c(2015,1), frequency = 12)
revenue_tseries



#Time plot for checking trends
p1 <- autoplot(revenue_tseries) + ggtitle("Time plot: Revenue by month") + ylab("Dollars in millions")
# Save the plot as a PDF
path <- "C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Results/revenue_plot.pdf"
ggsave(path, p1, device = "pdf")

#first difference of the data to remove the trend
DY<-diff(revenue_tseries)
p2 <- autoplot(DY) +ggtitle("Time plot:Change in revenue by month") + ylab("dollars in millions")
path <- "C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Results/revenuechange.pdf"
ggsave(path, p2, device = "pdf")

# checking for seasonality
ggseasonplot(DY) + ggtitle("Seasonal plot: Change in monthly revenue") + ylab("Dolars in millions")

#=============== Decomposing a timeseries ============================================================
revenue_decomposed <- decompose(revenue_tseries)
autoplot(revenue_decomposed)
decmp <- autoplot(revenue_decomposed) + ggtitle("Decomp of Revenue")
path <- "C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Results/decomprevenue_plot.pdf"
ggsave(path, decmp, device = "pdf")

# Access the components of the decomposed time series
trend <- revenue_decomposed$trend  # Trend component
seasonal <- revenue_decomposed$seasonal  # Seasonal component
random <- revenue_decomposed$random  # Random component seems to have no discernable pattern which may indicate that timeseries can predict


#model1: benchmark method to forecast
model1<-snaive(DY)
summary(model1)
#results may indicate a poor fit for this model

##Fit Exponential Smoothing Algorithm 
model2<-ets(revenue_tseries)
summary(model2)
checkresiduals(model2)
#ACF shows possible issues

#Fit model3 ARIMA model
model3<-auto.arima(revenue_tseries, d=1, D=1, stepwise = FALSE, approximation = FALSE,trace = TRUE)
summary(model3)
checkresiduals(model3)
#better ACF but constant line from 2015 to 2016 violates the residuals randomness abserved in previous models

#Forecast with model 2, for the next 3 years.
fcst<-forecast(model2, h=36)
autoplot(fcst)
fcst <- autoplot(fcst) + ggtitle("Forecasts from Exponential Smooting Algorithm")
path <- "C:/Users/BABlanchard/OneDrive - LSU AgCenter/Documents/grad semester 7/EXST 7087/Time_Series_Analysis/Homework9/Results/ForecastETS.pdf"
ggsave(path, fcst, device = "pdf")
summary(fcst)
