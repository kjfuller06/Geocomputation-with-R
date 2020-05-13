# lassign library path
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

rm(list=ls())




























