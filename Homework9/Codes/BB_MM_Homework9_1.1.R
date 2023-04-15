#============= Load libraries====================
rm(list = ls())
library(easypackages)
libraries("fpp2","zoo","tidyverse","lubridate","GGally","gridExtra","ggrepel","foreign", "urca","psych","hydroGOF")
#set working directory
source("./codes/functions/TSConvert.R") # adds the function 
#===================== Read data, keep latest, format dates =======================
# Specify the path to the directory containing the CSV file
path <- "C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/data/" # insert the path to the data
# Load the data from the CSV file using the specified path
data <- read.csv(paste0(path, "Month_Value_1.csv"), header = TRUE)
data1=data%>%select(Period,Revenue)
colnames(data1)=c("Period","Revenue")
write.csv(data1,"C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/data/Cleandata1.csv")
data1$Period <- as.Date(data1$Period, format = "%d.%m.%Y")
dir.create(paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/",data1$Date[nrow(data1)]),showWarnings = F)
# Remove observations with NAs in "Revenue" column
data1 <- data1[complete.cases(data1$Revenue), ]

#===================== Simple Graph of Revenue ===========================
Rev=ggplot(data1, aes(x=Period, y=Revenue)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "2 month",date_labels="%b")+
  geom_line(color="#00AFBB") + xlab("") + ylab("Revenue")+ 
  ggtitle("Timeseries of Revenue")+
  theme_classic(14)
ggsave(paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/",data1$Date[nrow(data1)],"/Revenue.jpg"),width=10,heigh=10,Rev)

Rev


# another option with ggplot2

library(ggplot2)
ggplot(data1, aes(x = Period, y = Revenue)) + geom_line()

# Optional stationary test
library(tseries)
adf.test(data1$Revenue) # H0: is not stationary, p-value = 0.01393 (we reject H0, the data is stationary)

#===================== Creating moving averages and adding them to the dataset ============
StrtDay=data1$Period[1]
# Parse date string using as.Date()
date_obj <- as.Date(StrtDay)
# Extract year, month, and day components
y_m_d <- c(year(date_obj), month(date_obj), day(date_obj))
# Convert components to numeric
y_m_d <- as.numeric(y_m_d)
#============ Creating the Daily Revenue and their moving Average
Revts <- ts(data1$Revenue, start=y_m_d,frequency=12)
Revts1=c(Revts[1],diff(Revts,1))
Revts_ma=ma(Revts1,7)
data1$DailyRev=as.vector(Revts1)
data1$Rev_ma=as.vector(Revts_ma)

#============ Summary Statistics and Saving the data =====================================
SumStats=describe(data1,na.rm=TRUE,trim=0.1,quant=c(.25,.75))
write.csv(SumStats,paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/",data1$Date[nrow(data1)],"/Summary_Statistics.csv"))
write.csv(data1,"C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/data/Cleandata.csv")


#Declare revenue column as time series data
revenue_tseries<-ts(data1[,2], start = c(2015,1), frequency = 12)
revenue_tseries

#=============== Decomposing a timeseries ============================================================
revenue_decomposed <- decompose(revenue_tseries)
autoplot(revenue_decomposed)
decmp <- autoplot(revenue_decomposed) + ggtitle("Decomp of Revenue")
path <- "C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/decomprevenue_plot.pdf"
ggsave(path, decmp, device = "pdf")

# Access the components of the decomposed time series
trend <- revenue_decomposed$trend  # Trend component
seasonal <- revenue_decomposed$seasonal  # Seasonal component
random <- revenue_decomposed$random  # Random component seems to have no discernable pattern which may indicate that timeseries can predict


#============= Train test split Setting parameters ================
d=0.1
h=3
k=round(nrow(data1)*d)
l=nrow(data1)-k
train=data1[1:l,]
test=data1[(l+1):nrow(data1),]
freq=12
# Check the format of "Period" column in "train" data frame
train$Period <- as.Date(train$Period, format = "%m.%d.%Y")
str(train)
str(test)

# Rename "Period" column to "Date"
colnames(train)[colnames(train) == "Period"] <- "Date"
colnames(test)[colnames(test) == "Period"] <- "Date"

#============= Analysis For Revenue ===============================================================

#============= Creating Time series ============

Allrev_train=TSConvert(train,"Rev_ma",freq)
Allrev_test=TSConvert(test,"Rev_ma",freq)
Allrev=c(Allrev_train,Allrev_test)

#================ NN train and prediction =========================
fit1=nnetar(Allrev_train,p=7,Size=10,repeats=50,lambda = "auto")
for1=forecast(fit1,k)
autoplot(for1)
predictions1=for1$mean
autoplot(predictions1)

fit2=nnetar(Allrev_train)
for2=forecast(fit2,k)
autoplot(for2)
predictions2=for2$mean
autoplot(predictions2)

#================ ARIMA and prediction =========================
fit3=auto.arima(Allrev_train) 
for3=forecast(fit3,h=k)
autoplot(for3)
predictions3=for3$mean
autoplot(predictions3)

fit4=arima(Allrev_train,order=c(4,0,1))
for4=forecast(fit4,k)
autoplot(for4)
predictions4=for4$mean
autoplot(predictions4)

#================ Comparing Predictions vs Truth ===================
results=data.frame(Date=test$Date,Test=test$Rev_ma,PredNN=as.vector(predictions1),
                   PredAutoNN=as.vector(predictions2),PredAutoARIMA=as.vector(predictions3),
                   PredARIMA=as.vector(predictions4))
results <- results[1:(nrow(results)-3), ]


results$Ensamble1=rowMeans(results[,c(3,5)])
results$Ensamble2=rowMeans(results[,3:6])

#================ Plotting Prediction vs Truth ====================
p1=ggplot() +
  geom_line(data=data1, aes(x = Period, y = Rev_ma, colour = "Actual Values"))+
  geom_line(data = results, aes(x = Date, y = Test, colour = "Actual Values")) +
  geom_line(data = results, aes(x = Date, y = PredNN,   colour = "Predictions NN"))  +
  geom_line(data = results, aes(x = Date, y = PredARIMA,   colour = "Predictions ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoARIMA,   colour = "Predictions Auto ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoNN,   colour = "Predictions Auto Neural Network"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble1,   colour = "Predictions Ensamble1"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble2,   colour = "Predictions Ensamble2"))  +
  ylab('Revenue')+
  scale_x_date(breaks = "2 month",date_labels="%b")+
  #scale_x_date(breaks=df1$Date ,labels=format(df1$Date,format="%m-%d"))+
  ggtitle(paste0("Comparison of Predicted vs True of Smoothed (7) Revenue for ",round(nrow(data1)*d)," days"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
ggsave(paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/",data1$Date[nrow(data1)],"/Predictions_vs_Test_days_",k,"_Revenue.jpg"),p1,width=10,heigh=8)
p1


#=============== Create Root Mean Square Errors ==================
RMSE=matrix(0,nrow=1,ncol = ncol(results)-2) 
RMSE=as.data.frame(RMSE)
colnames(RMSE)=colnames(results[,3:ncol(results)])

for (i in 1:ncol(RMSE)){
  RMSE[1,i]=rmse(results[,2],results[,i+2],na.rm=TRUE)
}

write.csv(RMSE,paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results",data1$Date[nrow(data1)],"/Smoothed_7_RMSE_days_",k,"_revenue.csv"))

res=(sort(RMSE[1,])[c(1,2)])
colnames(res)

#=============== Graphing the results with important dates ==============
geom.text.size = 4
theme.size = (14/5) * geom.text.size
Lck=as.Date("2020-03-22")
Ph2=Lck+74
Msk=Lck+111
ScOpen=Lck+151
Ph3=Lck+177
MPh2=Lck+248
Vac=Lck+288
Ph3N=Lck+346
p1=ggplot(data1, aes(x=Period, y=Rev_ma)) +
  #geom_rect(xmin = ScOpen, xmax = ScOpen+5, ymin = -500, ymax = max(df1$Cases_Total)+500,alpha = .05,fill="lightgreen")+
  geom_point(size=0.5)+
  geom_line(data=data1, aes(x = Period, y = Rev_ma, colour = "Actual Values"))+
  geom_line(data=results, aes(x = Date, y = PredNN, colour = "Neural Network Predictions"))+
  
  #geom_vline(xintercept=ScOpen+5, linetype=4,colour="green")+
  
  scale_x_date(breaks = "2 month",date_labels="%b")+
  ylab("Monthly Revenue")+
  ggtitle(paste0("MA(7) Smoothed Timeseries of Monthly Revenue with ",h," months prediction"))+
  theme_bw()+
  theme(axis.text = element_text(size = theme.size, colour="black"))+
  theme(legend.title = element_blank())
ggsave(paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results",data1$Date[nrow(data1)],"/Smoothed_With_",h,"_months_prediction_Revenue.jpg"),width=12,heigh=8,p1) 
ggsave(paste0("C:/Users/mmontiel/OneDrive - LSU AgCenter/Documents/LSU Maria Montiel/Courses/EXST 7087/Homework/Hwk 9/results/Latest_Smoothed_With_",h,"_months_prediction_Revenue.jpg"),width=12,heigh=8,p1) 

p1
