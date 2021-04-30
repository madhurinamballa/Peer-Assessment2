
# creating directory
setwd("F:/Desktop backup 19-03-2021/Research-assessmt")

# importing libraries
install.packages("knitr")
library(knitr)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("ggplot2")
library(ggplot2)

# downloading Storm Data file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile="stormData.csv.bz2")

# data processing
dsNOAA <- read.csv(bzfile("stormData.csv.bz2"), sep=",", header=T)

tidyNOAA <- dsNOAA[,c('EVTYPE','FATALITIES','INJURIES', 'PROPDMG', 'PROPDMGEXP', 'CROPDMG', 'CROPDMGEXP')]

tidyNOAA$PROPDMGNUM = 0
tidyNOAA[tidyNOAA$PROPDMGEXP == "H", ]$PROPDMGNUM = tidyNOAA[tidyNOAA$PROPDMGEXP == "H", ]$PROPDMG * 10^2
tidyNOAA[tidyNOAA$PROPDMGEXP == "K", ]$PROPDMGNUM = tidyNOAA[tidyNOAA$PROPDMGEXP == "K", ]$PROPDMG * 10^3
tidyNOAA[tidyNOAA$PROPDMGEXP == "M", ]$PROPDMGNUM = tidyNOAA[tidyNOAA$PROPDMGEXP == "M", ]$PROPDMG * 10^6
tidyNOAA[tidyNOAA$PROPDMGEXP == "B", ]$PROPDMGNUM = tidyNOAA[tidyNOAA$PROPDMGEXP == "B", ]$PROPDMG * 10^9

tidyNOAA$CROPDMGNUM = 0
tidyNOAA[tidyNOAA$CROPDMGEXP == "H", ]$CROPDMGNUM = tidyNOAA[tidyNOAA$CROPDMGEXP == "H", ]$CROPDMG * 10^2
tidyNOAA[tidyNOAA$CROPDMGEXP == "K", ]$CROPDMGNUM = tidyNOAA[tidyNOAA$CROPDMGEXP == "K", ]$CROPDMG * 10^3
tidyNOAA[tidyNOAA$CROPDMGEXP == "M", ]$CROPDMGNUM = tidyNOAA[tidyNOAA$CROPDMGEXP == "M", ]$CROPDMG * 10^6
tidyNOAA[tidyNOAA$CROPDMGEXP == "B", ]$CROPDMGNUM = tidyNOAA[tidyNOAA$CROPDMGEXP == "B", ]$CROPDMG * 10^9

# Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health ?

fatalities <- aggregate(FATALITIES ~ EVTYPE, data=tidyNOAA, sum)
fatalities <- fatalities[order(-fatalities$FATALITIES), ][1:10, ]
fatalities$EVTYPE <- factor(fatalities$EVTYPE, levels = fatalities$EVTYPE)

ggplot(fatalities, aes(x = EVTYPE, y = FATALITIES)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities by top 10 Weather Events")

# Event Type
injuries <- aggregate(INJURIES ~ EVTYPE, data=tidyNOAA, sum)
injuries <- injuries[order(-injuries$INJURIES), ][1:10, ]
injuries$EVTYPE <- factor(injuries$EVTYPE, levels = injuries$EVTYPE)

ggplot(injuries, aes(x = EVTYPE, y = INJURIES)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Injuries") + ggtitle("Number of injuries by top 10 Weather Events")

# Across the United States, which types of events have the greatest economic consequences ?
# Event-Type

damages <- aggregate(PROPDMGNUM + CROPDMGNUM ~ EVTYPE, data=tidyNOAA, sum)
names(damages) = c("EVTYPE", "TOTALDAMAGE")
damages <- damages[order(-damages$TOTALDAMAGE), ][1:10, ]
damages$EVTYPE <- factor(damages$EVTYPE, levels = damages$EVTYPE)

ggplot(damages, aes(x = EVTYPE, y = TOTALDAMAGE)) + 
  geom_bar(stat = "identity", fill = "red", las = 3) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event Type") + ylab("Damages ($)") + ggtitle("Property & Crop Damages by top 10 Weather Events")


