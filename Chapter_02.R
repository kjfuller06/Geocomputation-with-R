#lassign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

#Load packages for chapter 2
library(devtools)
library(spData)
install.packages("sf")
install.packages("raster")
install.packages("spData")
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data


