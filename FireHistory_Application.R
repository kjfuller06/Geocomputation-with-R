# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

# Load packages for chapter 2
library(sf)          # classes and functions for vector data
library(sp)

firehist<-st_read("Fire_NPWS_FireHistory.shp")
firehist

# Grab just the first year listed. It looks like the original dataset describes year as the couplet describing fire season, i.e. "1996-97". Need to double check***
firehist$year<-as.Date(substr(firehist$Label,1,4),as.year)

# Grab burn type from Label and convert to factor
firehist$burntype<-as.factor(substr(firehist$Label,9,length(firehist$Label)))

# Create df subset from year, including only fires from 2018-2019
recent<-firehist[firehist$year>2017,]





