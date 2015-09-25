# Examining the Impact of Severe Weather Events from 1950 - 2011
Jack Gidding  
September 24, 2015  

A key concern of governments is the well being of its citizens. Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Weather events can result in fatalities, injuries, and property damage. Preventing such outcomes should be addressed by government in terms of public policy. In order to set the right policy in this area, we must answer two questions. First, which types of events are most harmful with repect to the population? Second, which types of events have the greatest economic impact? To answer these questions for the United States, we examine data from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database.

## Data Processing
Load the libraries that will be used in the analysis. 


```r
library(R.utils)
library(lubridate)
library(plyr)
library(ggplot2)
```

The NOAA storm data is loaded into the environment for processing. The bzipped datafile is stored in the GitHub repository. If the csv data file does not exist, then data data is unzipped and verified. 


```r
filename.data <- "repdata-data-StormData.csv"
if (!file.exists(filename.data)) {
    bunzip2(paste(filename.data,".bz2",sep=""))
}

# Check postcondition
if (!file.exists(filename.data)) {
    errMsg <- paste("Error: Data file (",filename.data,") does not exist.",sep="")
    stop(errMsg)
}

# Read in the data
data.noaa <- read.csv(filename.data)

# Check the characteristics
nrow(data.noaa)
```

```
## [1] 902297
```

```r
length(data.noaa)
```

```
## [1] 37
```

There are 902,297 events in the database and 37 characteristics of each event. 

The start and end dates for each event is turned into a datetime representation useful for processing. The hours and minutes of each event is stored in a separate column. Since that information is not material to this study, it is not processed and added to the datetime object.


```r
# Turn the start and end dates into POSIXct objects for processing
data.noaa[ ,2] <- parse_date_time(data.noaa[ ,2], "m*!/d*!/y! H!:M!:S!")
data.noaa[ ,12] <- parse_date_time(data.noaa[ ,12], "m*!/d*!/y! H!:M!:S!")

# Normalize all EVTYPE strings to uppercase
data.noaa[ ,8] <- toupper(data.noaa[ ,8])
```

## Results

### Impact on Public Health

In order to answer the first question, the number of fatalities and injuries are calculated for each event type from the NOAA storm data. 


```r
# Summarize the data for fatalities and injuries
data.fatalities <- aggregate(data.noaa$FATALITIES, by=list(data.noaa$EVTYPE), sum)
data.injuries <- aggregate(data.noaa$INJURIES, by=list(data.noaa$EVTYPE), sum)

# Rename the columns
colnames(data.fatalities) <- c("Event","Count")
colnames(data.injuries) <- c("Event","Count")

# Sort the data 
data.fatalities.sorted <- arrange(data.fatalities, data.fatalities$Count,
                                  decreasing=TRUE)
data.injuries.sorted <- arrange(data.injuries, data.injuries$Count,
                                  decreasing=TRUE)
colnames(data.fatalities.sorted) <- c("Event","Count")
colnames(data.injuries.sorted) <- c("Event","Count")
```
Once we have the data summarized and sorted, we can derive top 10 lists of weather related causes of fatalities and injuries. 

#### Top 10 Causes of Fatalities


```r
data.fatalities.top10 <- head(data.fatalities.sorted, n=10)
colnames(data.fatalities.top10) <- c("Event","Count")
data.fatalities.top10
```

```
##             Event Count
## 1         TORNADO  5633
## 2  EXCESSIVE HEAT  1903
## 3     FLASH FLOOD   978
## 4            HEAT   937
## 5       LIGHTNING   816
## 6       TSTM WIND   504
## 7           FLOOD   470
## 8     RIP CURRENT   368
## 9       HIGH WIND   248
## 10      AVALANCHE   224
```

#### Top 10 Causes of Injuries


```r
data.injuries.top10 <- head(data.injuries.sorted, n=10)
colnames(data.injuries.top10) <- c("Event","Count")
data.injuries.top10
```

```
##                Event Count
## 1            TORNADO 91346
## 2          TSTM WIND  6957
## 3              FLOOD  6789
## 4     EXCESSIVE HEAT  6525
## 5          LIGHTNING  5230
## 6               HEAT  2100
## 7          ICE STORM  1975
## 8        FLASH FLOOD  1777
## 9  THUNDERSTORM WIND  1488
## 10              HAIL  1361
```
#### Plot comparing Fatalities and Injuries

Next, the top 10 causes of fatalities and injuries are plotted side-by-side on the same chart to show the relative magnitude.  


```r
# Add column to identify fatalities
data.fatalities.top10[ ,3] <- "Fatalities"
colnames(data.fatalities.top10) <- c("Event","Count", "Outcome")

# Add column to identify injuries
data.injuries.top10[ ,3] <- "Injuries"
colnames(data.injuries.top10) <- c("Event","Count", "Outcome")

# Combine fatalities and injuries into same data.frame
data.combined.top10 <- rbind(data.fatalities.top10, data.injuries.top10)

ggplot(data.combined.top10, aes(x=reorder(Event, desc(Count)), y=Count, fill=Outcome)) + 
    geom_bar(stat="identity", position="dodge") +
    labs(x="Event", y="Count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title="Top 10 Causes of Injuries and Fatalities\ndue to Severe Weather, 1950 - 2011")
```

![](RepData_PA2_files/figure-html/publicimpactplot-1.png) 

In the plot above, we can see that the top two causes of fatalities are tornado and excessive heat. The top two causes of injuries are tornado and tstm wind. The number of injuries with the top event, tornado, are 16.2 times that of fatalities. 
