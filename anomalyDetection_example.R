#Install the devtools package then github packages
install.packages(c("curl", "httr"))
install.packages("tidyr")

install.packages("tibble")
install.packages("tibbletime")

install.packages("devtools")
install.packages("Rcpp")
library(devtools)
library(tibbletime)
#install_github("petermeissner/wikipediatrend")
#install_github("twitter/AnomalyDetection")
# install.packages("anomalyDetection")
install.packages("readxl")
install.packages('anomalize') 

install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#library(AnomalyDetection)

library(anomalize)
library(tibble)
library(curl)
library(httr)

library(devtools)
#Loading the libraries
library(Rcpp)
#library(wikipediatrend)
#library(anomalyDetection)
library(readxl)
library(tidyr)
library(dplyr)

#Download the Incident & Work Order file
incident = readxl::read_xlsx("C:/Data_Science/Tableau/IPO_R&D_Incident_WorkOrder/IPO-R&D Raw_Oct_18 - ori.xlsx",sheet="EIS Raw Data",col_names=TRUE)

#Remove spaces from column names
#names(incident)
names(incident) <- make.names(names(incident))

#Plotting data
library(ggplot2)
ggplot(incident, aes(x=Submit.Date, y=Days.Open, color=Days.Open)) + geom_line()

# Keep only relevant features and discard all other variables
columns_to_keep=c("Submit.Date","Days.Open")
incident=incident[,columns_to_keep]

# Find and remove NA rows
summary(incident)
incident <- na.omit(incident)

incident_ts <- incident %>% rownames_to_column() %>% as.tibble() %>% 
mutate(Submit.Date = as.Date(Submit.Date)) %>% select(-one_of('rowname'))

incident_ts <- incident_ts[,c("Submit.Date","Days.Open")]

#Convert to a Time Tibble object
incident_ts <- as_tbl_time(incident_ts, index = Submit.Date)

#Sort the data by Submit.Date for indexation
incident_ts <- arrange(incident_ts,Submit.Date)

#correctly define your date format
incident_ts <- incident_ts %>% as_period("daily")

# Time Series Decomposition with Anomalies
incident_ts %>% 
  time_decompose(Days.Open, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition() 

incident_ts
#Taking Aplha = 0.05

incident_ts%>%
  time_decompose(Days.Open)%>%
  anomalize(remainder, alpha = 0.05)%>%
  time_recompose()%>%
  plot_anomalies(time_recompose = T)+
  ggtitle("alpha = 0.05")

#Taking Aplha = 0.025
incident_ts%>%
  time_decompose(Days.Open)%>%
  anomalize(remainder, alpha = 0.05)%>%
  time_recompose()%>%
  plot_anomalies(time_recompose = T)+
  ggtitle("alpha = 0.025")

#max_anom parameter controls the percentage of data that can be an anomaly
incident_ts%>%
  time_decompose(Days.Open)%>%
  anomalize(remainder, alpha = 0.05, max_anoms = 0.05)%>%
  time_recompose()%>%
  plot_anomalies(time_recompose = T)+
  ggtitle("5% anomaly Allowed")

#Extract actual data points of anomaly

incident_ts %>% 
  time_decompose(Days.Open) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') 


#Outlier detection using Statistical package

install.packages("grDevices")
library(grDevices)

install.packages("EnvStats")
library(EnvStats)

#Download the Incident & Work Order file
inc = readxl::read_xlsx("C:/Data_Science/Tableau/IPO_R&D_Incident_WorkOrder/IPO-R&D Raw_Oct_18 - ori.xlsx",sheet="EIS Raw Data",col_names=TRUE)

#Remove spaces from column names
#names(incident)
names(inc) <- make.names(names(inc))

# Keep only relevant features and discard all other variables
columns_to_keep=c("Submit.Date","Days.Open")
inc=inc[,columns_to_keep]

# Find and remove NA rows
summary(inc)
inc <- na.omit(inc)
inc_days_open <- inc$Days.Open
#  Actually, box-plot is the visualization where we can see how the data is distributed along with if there are any outliers or not
inc_days_open[which(inc_days_open %in% boxplot.stats(inc_days_open)$out)]

#Boxplot before treating the outliers
boxplot(inc_days_open,
        main = "Before removing outlier Days.Open of Incidents",
        xlab = "Days Open",
        ylab = "Tickets",
        col = "blue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

EnvStats::rosnerTest(inc_days_open,k=15,warn=F)

inc_days_open <- sort(inc_days_open)
for(i in 1:length(inc_days_open)){
  if(inc_days_open[i] > quantile(inc_days_open,0.75)+1.5*IQR(inc_days_open)){
    inc_days_open[i] <- max(inc_days_open[1:(i-1)])
  }
}

inc_days_open <- sort(inc_days_open,decreasing = T)
for(i in 1:length(inc_days_open)){
  if(inc_days_open[i] < quantile(inc_days_open,0.25)-1.5*IQR(inc_days_open)){
    inc_days_open[i] <- min(inc_days_open[1:(i-1)])
  }
}

boxplot(inc_days_open,main = "After replacing the outliers")

#Boxplot after treating the outliers
boxplot(inc_days_open,
        main = "After removing outlier Days.Open of Incidents",
        xlab = "Days Open",
        ylab = "Tickets",
        col = "blue",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

### Outlier Detection using K-means Clustering

inc <- as.data.frame(inc)
# On this subset of data perform a k-means cluster using the kmeans() function with k=3
kmeans.result <- kmeans(inc$Days.Open, centers=3)

# To view the results of the k-means cluster type the following command
kmeans.result$centers

# To obtain the cluster ID's type the following command
kmeans.result$cluster

# calculate the distance between the objects and cluster centers to determine the outliers and identify 10 largest distances which are outliers
centers <- kmeans.result$centers[kmeans.result$cluster, ] # "centers" is a data frame of 3 centers but the length of iris dataset so we can canlculate distance difference easily.

#rowsum and clsum dont work with 1-D vectors so using drop=FALSE
distances <- sqrt(rowSums((inc[,2,drop=FALSE] - centers)^2))

#Top 10 outliers
outliers <- order(distances, decreasing=T)[1:10]
print(outliers) # these rows are 10 top outliers

#To print the details about the outliers use the following command
print(inc[outliers,])

#Using the following commands provided below you should be able to plot the clusters with the "+" representing the outliers 
#and the asterisks "*" representing the cluster center.

plot(inc[,c("Days.Open")], pch=19, col=kmeans.result$cluster, cex=1)
points(kmeans.result$centers[,c("Days.Open")], col=1:3, pch=15, cex=2)
points(inc[outliers, c("Days.Open")], pch="+", col=4, cex=3)


################

install.packages("kernlab")
library(kernlab)

test <- runif(100)*10
test[sample(1:100,5)] <- sample(10:20,5)
head(test, 10)
plot(test, type="l", col="blue")
svm_model=kernlab::ksvm(test,nu=0.09, type="one-svc", kernel="vanilladot")

get_index=predict(svm_model)
head(get_index)

out_index=which(get_index[,1]==TRUE)
out_index
test[out_index]

plot(test, col="blue", type="l")
points(x=out_index, y=test[out_index], pch=18, col="red")
