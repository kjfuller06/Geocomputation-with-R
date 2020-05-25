# assign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

# Load packages for chapter 2
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data

# examine world
names(world)

# plot maps of all variables in world
plot(world)

# plot just a selection of attributes of world
plot(world[3:6])
plot(world["pop"])

# examine the summary stats of just the "lifeEXp" attribute of world
summary(world["lifeExp"])

# subset the world DF
worldmini<-world[1:2,1:3]
worldmini

# many packages aren't equipped to handle SF objects
# convert world to an SP object
library(sp)
world_sp<-as(world,Class="Spatial")
world_sp

# convert back to SF
world_sf<-st_as_sf(world_sp,"sf")
world_sf

# combine polygons
world_asia = world[world$continent == "Asia", ]
asia = st_union(world_asia)
plot(asia)

# plot asia over a map of the world. reset=FALSe retains the legen from the first plot
plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red")

# more advanced plotting with an SF object 
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop)/10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

# plot a specific area around a country, with the bounding box defined by expandBB = c(bottom, left, top, right)
india = world[world$name_long == "India", ]
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3)
#plot the rest of asia for context, using [0] to plot only the geometry and not the attributes
plot(world_asia[0], add = TRUE)


## vectors
# creating simple features (sfg). These can't hold information other than the object's geometry.
point1<-st_point(c(1,2))
point2 = st_point(c(1, 3))
st_point(c(1,2,5))
st_point(c(1,2,5),dim="XYM") #M is an additional variable, usually accuracy
multipoint_matrix = rbind(c(5, 2), c(1, 3), c(3, 4), c(3, 2))
st_multipoint(multipoint_matrix)
#use rbind to make a simple matrix and convert to linestring
linestring_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
st_linestring(linestring_matrix)
# a polygon is just a linestring where the first and last points are the same, but "list" is used for creation of multilinestrings, polygons, multipolygons, and geometry collections
polygon_list = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
st_polygon(polygon_list)
# special POLYGON example with a hole
polygon_border = rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))
polygon_hole = rbind(c(2, 4), c(3, 4), c(3, 3), c(2, 3), c(2, 4))
polygon_with_hole_list = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole_list)
# multilinestring
multilinestring_list = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                            rbind(c(1, 2), c(2, 4)))
st_multilinestring((multilinestring_list))
# MULTIPOLYGON
multipolygon_list = list(list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))),
                         list(rbind(c(0, 2), c(1, 2), c(1, 3), c(0, 3), c(0, 2))))
st_multipolygon(multipolygon_list)
# GEOMETRYCOLLECTION
gemetrycollection_list = list(st_multipoint(multipoint_matrix),
                              st_linestring(linestring_matrix))
st_geometrycollection(gemetrycollection_list)

# combine two simple features (sfg) into a one single object with two features called a simple feature geometry column (sfc). Works for all sfg objects, including sfg's of different geometry types.
multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))
point_multilinestring_sfc = st_sfc(point1, multilinestring1)
st_geometry_type(point_multilinestring_sfc)

# sfc's can store additional info, such as the coordinate reference system. To add this, all objects in the sfc must have the same crs.
points_sfc_wgs = st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)
# OR
st_sfc(point1, point2, crs = "+proj=longlat +datum=WGS84 +no_defs")

# creating an sf with attributes
lnd_point = st_point(c(0.1, 51.5))                 # sfg object
lnd_geom = st_sfc(lnd_point, crs = 4326)           # sfc object
lnd_attrib = data.frame(                           # data.frame object
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)    # sf object
lnd_sf
class(lnd_sf)


## rasters
library(raster)
library(rgdal)

#check out the raster package
help("raster")

#create new RasterLayer using a dataset from spDataLarge
raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
new_raster = raster(raster_filepath)
new_raster

#examine file
dim(new_raster)
ncell(new_raster)
res(new_raster)
extent(new_raster)
inMemory(new_raster)
plot(new_raster)

#Functions such as spplot() and levelplot() (from the sp and rasterVis packages, respectively) to create facets, a common technique for visualizing change over time. Packages such as tmap, mapview and leaflet to create interactive maps of raster and vector objects (see Chapter 8).

#see what drivers are available on my system
raster::writeFormats()
rgdal::gdalDrivers()

#create RasterLayer from scratch. Cell values are assigned row-wise, starting at the upper left corner
new_raster2 = raster(nrows = 6, ncols = 6, res = 0.5, xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5, vals = 1:36)
new_raster2

#create RasterBrick from spDataLarge data. Must come from the same source and is a single object. Landsat data with multiple bands is a great example
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
r_brick = brick(multi_raster_file)
r_brick
nlayers(r_brick)

#RasterStack lets you combine multiple layers from different sources
raster_on_disk = raster(r_brick, layer = 1)
raster_in_memory = raster(xmn = 301905, xmx = 335745, ymn = 4111245, ymx = 4154085, res = 30)
values(raster_in_memory) = sample(seq_len(ncell(raster_in_memory)))
crs(raster_in_memory) = crs(raster_on_disk)
r_stack = stack(raster_in_memory, raster_on_disk)
r_stack


## coordinate reference systems
# The CRs includes the datum, which is the ellipsoid representation of the earth the system is based on. A local datum optimises the ellipsoid to fit local topography and position. A geocentric datum (WSG84) is optimised for Earth's center of gravity. The specification is listed in the towgs84 argument of proj4string notation
#check out the different projections and corresponding ellipsoids here:
projInfo(type="datum")

# projected systems are based on Cartesian coordinates on a flat surface. They are based on the geographic CRSs described above but are converted using a map projection
projInfo(type="proj")

# CRSs are defined using an epsg code, signifying a well-defined system encompassing all parameters, or using the proj4string approach, which allows you to specify the parameters
crs_data<-rgdal::make_EPSG()
View(crs_data)

# check and set the CRS of a vector dataset
vector_filepath = system.file("vector/zion.gpkg", package = "spDataLarge")
new_vector = st_read(vector_filepath)
st_crs(new_vector)
st_transform(new_vector,crs=4326) #used to reproject a vector dataset
new_vector = st_set_crs(new_vector, 4326) #used to reassign, doesn't reproject

# check and set the CRS of a raster dataset
projection(new_raster)
projection(new_raster) = "+proj=utm +zone=12 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0+units=m +no_defs"

#st_area includes the units of a vector
luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)
attributes(st_area(luxembourg))

#convert the units
units::set_units(st_area(luxembourg), km^2)

# you can't access the units of raster files. Raster files follow the units of their projectsions, so that's what you have to check. If you change the projection, the units change.
res(new_raster)

## Exercises
# 1. Use summary() on the geometry column of the world data object. What does the output tell us about:
summary(world['geom'])

    # Its geometry type? 
# multipolygon
    # The number of countries? 
# No
    # Its coordinate reference system (CRS)? 
# epsg 4326

# 2. Run the code that ‘generated’ the map of the world in Figure 2.5 at the end of Section 2.2.4. Find two similarities and two differences between the image on your computer and that in the book
plot(world["continent"], reset = FALSE)
cex = sqrt(world$pop) / 10000
world_cents = st_centroid(world, of_largest = TRUE)
plot(st_geometry(world_cents), add = TRUE, cex = cex)

    # What does the cex argument do (see ?plot)? 
# changes the size of the centroids
    # Why was cex set to the sqrt(world$pop) / 10000? 
# to make it variable based on world population
    # Bonus: experiment with different ways to visualize the global population.
# no

# Use plot() to create maps of Nigeria in context (see Section 2.2.4).
nigeria = world[world$name_long == "Nigeria", ]
africa = world[world$continent == "Africa", ]
plot(st_geometry(nigeria), col = "white", lwd = 3, main = "Nigeria in context", border = "lightgrey", expandBB = c(0.5, 0.2, 0.5, 0.2))
plot(st_geometry(world), lty = 3, add = TRUE, border = "grey")
plot(st_geometry(nigeria), col = "yellow", add = TRUE, border = "darkgrey")
a = africa[grepl("Niger", africa$name_long), ]
ncentre = st_centroid(a)
ncentre_num = st_coordinates(ncentre)
text(x = ncentre_num[, 1], y = ncentre_num[, 2], labels = a$name_long)

    # Adjust the lwd, col and expandBB arguments of plot().
    # Challenge: read the documentation of text() and annotate the map. 

# 4. Create an empty RasterLayer object called my_raster with 10 columns and 10 rows. Assign random values between 0 and 10 to the new raster and plot it.
my_raster<-raster(nrow=10,ncol=10, vals=sample(1:10,100,replace=T))
plot(my_raster)

# 5. Read-in the raster/nlcd2011.tif file from the spDataLarge package. What kind of information can you get about the properties of this file? 
# class, dimensions, resolution, extent, CRS, range of values
raster_filepath = system.file("raster/nlcd2011.tif", package = "spDataLarge")
new_raster5<- raster(raster_filepath)
plot(new_raster5)
new_raster5






# rm(list=ls())
