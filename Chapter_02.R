#lassign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

#Load packages for chapter 2
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

#examine world
names(world)

#plot maps of all variables in world
plot(world)

#plot just a selection of attributes of world
plot(world[3:6])
plot(world["pop"])

#examine the summary stats of just the "lifeEXp" attribute of world
summary(world["lifeExp"])

#subset the world DF
worldmini<-world[1:2,1:3]
worldmini

#many packages aren't equipped to handle SF objects
#convert world to an SP object
library(sp)
world_sp<-as(world,Class="Spatial")
world_sp

#convert back to SF
world_sf<-st_as_sf(world_sp,"sf")
world_sf

#combine polygons
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)
plot(asia)

#plot asia over a map of the world. reset=FALSe retains the legen from the first plot
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

#more advanced plotting with an SF object 
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop)/10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

#plot a specific area around a country, with the bounding box defined by expandBB = c(bottom, left, top, right)
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
#plot the rest of asia for context, using [0] to plot only the geometry and not the attributes
plot(world_asia[0], add = TRUE)

