# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

# Load packages for chapter 2
library(sf)          # classes and functions for vector data
library(sp)
library(spData)

firehist<-st_read("Fire_NPWS_FireHistory.shp")
firehist

# Grab just the first year listed. The original dataset describes year as the couplet describing fire season, i.e. "1996-97".
firehist$year<-as.numeric(substr(firehist$Label,1,4),format="%Y")

# Grab burn type from Label and convert to factor
firehist$burntype<-as.factor(substr(firehist$Label,9,length(firehist$Label)))

# Create df subset from year, including only fires from 2018-2019
recent<-firehist[firehist$year>2017,]
# plot(recent)
fire_2018 <- recent[recent$FireName == "MER Bournda Sth LMZ -Tura Beach",]


# isolate just Australia and map recent fires over NSW
world_aus <- world[world$name_long == "Australia", ]
aus <- st_union(world_aus)
# expandBB = c(bottom, left, top, right)
plot(aus, expandBB = c( -0.18, -0.85, -0.51, -0.1))
plot(recent["PerimeterM"], add = TRUE, col = recent$year, border=NA)
legend( "topright", legend = c("2018-19","2019-20"), col= c("red", "green"), lwd=10)
