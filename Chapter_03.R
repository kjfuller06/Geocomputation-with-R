# lassign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

# Load packages for chapter 3
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(dplyr)
library(stringr)
library(spData)

# what methods are available for sf data?
methods(class = "sf")

# check out the df in the sf
dim(world) # it is a 2 dimensional object, with rows and columns
nrow(world) # how many rows?
ncol(world)
world_df = st_drop_geometry(world)
class(world_df)






