---
title: "Natural disasters in the US - An analysis"
author: "Mauro Leonelli"
date: "Tuesday, August 12, 2014"
output: html_document
---

------

#Synopsis


```r
data <- read.csv(bzfile("C:/R/RepData_PeerAssessment2/repdata_data_StormData.csv.bz2"), nrows = 1000)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.1.1
```

```r
data$YEAR <- format(strptime(data$BGN_DATE, format = '%m/%d/%Y'), '%Y')
data2 <- data[data$YEAR > 1979,]
#data3 <- data2[data$EVTYPE == "TORNADO"]
rm(data)
data2 <- data2[, c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP",    "CROPDMG", "CROPDMGEXP")]

for (i in 1:nrow(data2)) {
    if(!is.na(data2[i,"PROPDMGEXP"])){
      if(data2[i,"PROPDMGEXP"] == "K"){
        data2[i,"PROPDMG"] = data2[i,"PROPDMG"] * 1000
      }
      else if(data2[i,"PROPDMGEXP"] == "M"){
        data2[i,"PROPDMG"] = data2[i,"PROPDMG"] * 1000000
      }
      else if(data2[i,"PROPDMGEXP"] == "B"){
        data2[i,"PROPDMG"] = data2[i,"PROPDMG"] * 1000000000
      }
    }
    if(!is.na(data2[i,"CROPDMGEXP"])){
    if(data2[i,"CROPDMGEXP"] == "K"){
      data2[i,"CROPDMG"] = data2[i,"CROPDMG"] * 1000
    }
    else if(data2[i,"CROPDMGEXP"] == "M"){
      data2[i,"CROPDMG"] = data2[i,"CROPDMG"] * 1000000
    }
    else if(data2[i,"CROPDMGEXP"] == "B"){
      data2[i,"CROPDMG"] = data2[i,"CROPDMG"] * 1000000000
    }
  }
dt <- ddply(data2, .(EVTYPE, YEAR), summarise, fatalities = sum(FATALITIES), injuries = sum(INJURIES), value = sum(CROPDMG + PROPDMG))

ggplot(aes(YEAR, fatalities), data = dt, colour=EVTYPE) + geom_point(aes(group=EVTYPE, color = EVTYPE)) + geom_line(aes(group=EVTYPE, color = EVTYPE))

ggplot(aes(YEAR, value), data = dt, colour=EVTYPE) + geom_point(aes(group=EVTYPE, color = EVTYPE)) + geom_line(aes(group=EVTYPE, color = EVTYPE))
}
```

```
## Error: could not find function "ggplot"
```
#FATALITIES  INJURIES	PROPDMG	PROPDMGEXP	CROPDMG	CROPDMGEXP

