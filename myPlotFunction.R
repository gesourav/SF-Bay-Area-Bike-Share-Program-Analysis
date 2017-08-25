df <- read.csv("data/201408_trip_data (5).csv")

library(sqldf)
library(lubridate)
library(ggplot2)
library(dplyr)
library(chron)

names(df) <- c("Trip_ID","Duration", "Start_Date", "Start_Station", "Start_Terminal",
               "End_Date", "End_Station", "End_Terminal", "BikeID", "Subscriber_Type",
               "Zip_Code")

#Extract Date and Time

df$Start_Date <- parse_date_time(df$Start_Date, '%m/%d/%Y %H:%M', exact = TRUE)
df$Start_Date1 <- as.Date(df$Start_Date)
df$Start_Hour <- hour(df$Start_Date)

df$End_Date <- parse_date_time(df$End_Date, '%m/%d/%Y %H:%M', exact = TRUE)
df$End_Date1 <- as.Date(df$End_Date)
df$End_Hour <- hour(df$End_Date)

df$StartDayofWk <- wday(df$Start_Date1, label=TRUE, abbr = F)

DemandSupply_StatusAtStation <- function(StationID){
  
  if(StationID %in%  unique(df$Start_Terminal)){
    
    Station_data <- subset(df, df$Start_Terminal == StationID | df$End_Terminal == StationID)
    NameofStation <- Station_data[1,4]
    
    #Find out the Demand at this Station at every hour
    Station_dataDD <- sqldf("SELECT Start_Hour, COUNT(Start_Hour) AS Demand
                            FROM Station_data
                            GROUP BY Start_Hour
                            ORDER BY Start_Hour")
    
    #Find out the Supply at this Station at every hour
    Station_dataSS <- sqldf("SELECT End_Hour, COUNT(End_Hour) AS Supply
                            FROM Station_data
                            GROUP BY End_Hour
                            ORDER BY End_Hour")
    
    #Merge the Demand and Supply Tables
    Station_dataDDSS <- sqldf("SELECT b.End_Hour, a.Demand, b.Supply
                              FROM Station_dataDD a
                              INNER JOIN Station_dataSS b
                              ON a.Start_Hour = b.End_Hour")
    
    #Plot the Demand and Supply by Hours
    plot (range(Station_dataDDSS$End_Hour), range(c(Station_dataDDSS$Supply,Station_dataDDSS$Demand)),type='n',
          xlab='Hour of the Day',ylab='Demand/Supply',
          main = paste0("Hourly Status of Availability and Demand at ", NameofStation))
    lines(Station_dataDDSS$End_Hour, Station_dataDDSS$Supply, col='red',lwd=2.5, pch = 2)
    lines(Station_dataDDSS$End_Hour, Station_dataDDSS$Demand, col='blue',lwd=2.5, pch = 10)
    legend(1,max(Station_dataDDSS$Supply), c('Demand', 'Supply'), lty=c(1,1), lwd=c(2.5,2.5),
           col=c('blue','red'), cex = 0.5) 
    
    #Check whether Demand is more than Supply
    
    condition   <- Station_dataDDSS$Demand > Station_dataDDSS$Supply
    status <- ifelse(condition, 1, 0)
    avgBalancedHours <- (sum(status))/nrow(Station_dataDDSS)
    
    plot (range(Station_dataDDSS$End_Hour), range(c(0,max(Station_dataDDSS$Demand/Station_dataDDSS$Supply)+0.1)),type='n',
          xlab='Hour of the Day',ylab='Demand/Supply',
          main = paste0("Demand/Supply Ratio by Hours at ", NameofStation))
    
    lines(Station_dataDDSS$End_Hour, Station_dataDDSS$Demand/Station_dataDDSS$Supply, col='green',lwd=2.5, pch = 12)
    abline(a= 1, b=0, col = "red")
    text(12, 0.25, cex = 0.75, 
         paste0("Fraction\nof Hours\nwhen DD>SS\nis ", round(avgBalancedHours,2)))
  }else{
    print("Please enter a Valid Station ID and call the Function")
  }
}


