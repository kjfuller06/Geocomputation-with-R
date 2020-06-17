.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(sf)
library(raster)
library(dplyr)
library(spData)

# 4.2.1 geographical subsetting
canterbury = nz %>% 
  filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
plot(st_geometry(nz))
plot(st_geometry(canterbury), add = TRUE, col = "yellow")

# spatial intersects are done using subsetting, with the default being "intersect". Including "op =" as the third argument allows you to change the default spatial function (or topographical operator)
# disjoint returns the polygons in nz_height that do not intersect canterbury
nz_height[canterbury, , op = st_disjoint]

# other forms of subsetting are less common
# create an sgbp (sparse geometry binary predicate). This is a list of binary results that indication if i in x does (1) or doesn't(0) intersect with y. Can have more results besides 0 and 1 if applied to more than two layers
sel_sgbp = st_intersects(x = nz_height, y = canterbury)
class(sel_sgbp)
# select only the "does" results
sel_logical = lengths(sel_sgbp) > 0
# use the result to subset the original df and extract only the intersecting polygons
canterbury_height2 = nz_height[sel_logical, ]
canterbury_height2
# can generate TRUE and FALSE in a list as well if preferred over an sgbp, but sgbp's are very effecient and more generalisable
canterbury_height3 = st_intersects(x = nz_height, y = canterbury, sparse = FALSE)[, 1]
# OR
canterbury_height3 = nz_height %>%
  filter(st_intersects(x = ., y = canterbury, sparse = FALSE))

# 4.2.2 topological relations
# create a polygon by creating a list object (matrix within a list?)
a_poly = st_polygon(list(rbind(c(-1, -1), c(1, -1), c(1, 1), c(-1, -1))))
a = st_sfc(a_poly)
# create a line by creating a matrix- first two values are x's, second two are y's
l_line = st_linestring(x = matrix(c(-1, -1, -0.5, 1), ncol = 2))
l = st_sfc(l_line)
# create points by creating a matrix- first four values are x's, second four are y's
p_matrix = matrix(c(0.5, 1, -1, 0, 0, 1, 0.5, 1), ncol = 2)
p_multi = st_multipoint(x = p_matrix)
p = st_cast(st_sfc(p_multi), "POINT")

# which points overlap with the polygon?
plot(a)
plot(p, add = TRUE)
st_intersects(p, a)
# the above creates a sparse geometry binary predicate
st_intersects(p, a, sparse = FALSE)
# the above returns TRUE and FALSE for every point

# points that are completely within the polygon
st_within(p, a)
# points that touch the border of the polygon
st_touches(p, a)
# points that are within a certain distance from the polygon
## this operator can only return a sparse matrix. NOTE: the last point is 1 unit away but is still caught when the distance is set to 0.9
st_is_within_distance(p, a, dist = 0.9)
# to get a non-sparse list
sel = st_is_within_distance(p, a, dist = 0.9) # can only return a sparse matrix
lengths(sel) > 0

# 4.2.3 spatial joining
# create a random set of coordinates on the earth
set.seed(2018) # set seed for reproducibility
(bb_world = st_bbox(world)) # the world's bounds
random_df = tibble(
  x = runif(n = 10, min = bb_world[1], max = bb_world[3]),
  y = runif(n = 10, min = bb_world[2], max = bb_world[4])
)
random_points = random_df %>% 
  st_as_sf(coords = c("x", "y")) %>% # set coordinates
  st_set_crs(4326) # set geographic CRS

# now join attribute data
# select the places that intersect- st_intersect is the default
world_random = world[random_points, ]
# join attributes- left_join is default
random_joined = st_join(random_points, world["name_long"])
plot(world$geom)
plot(random_joined$geometry, add = TRUE, col = "red", pch = 4)

# join two non-overlapping point layers. We want the attributes of one layer to be linked with the point locations of another.
# are any two points the same?
any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))
# reproject to same CRS
cycle_hire_P = st_transform(cycle_hire, 27700)
cycle_hire_osm_P = st_transform(cycle_hire_osm, 27700)
# keep all records within 20m of the desired point locations
sel = st_is_within_distance(cycle_hire_P, cycle_hire_osm_P, dist = 20)
summary(lengths(sel) > 0)
# join
z = st_join(cycle_hire_P, cycle_hire_osm_P, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)
# the joined layer has more entries than the original target layer because the join created duplicates. We'll take the means in order to aggregate them
z = z %>% 
  group_by(id) %>% 
  summarize(capacity = mean(capacity))
nrow(z) == nrow(cycle_hire)
# check by plotting
plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

# 4.2.5 spatial data aggregation
# find the mean height of high points in each region of NZ. nz_height contains the high points. nz contains the regions.
nz_avheight = aggregate(x = nz_height, by = nz, FUN = mean)
# how did it do that? nz has a whole bunch of columns and none of them is "region". None of them are the same between the layers.
## ok. I think because nz is a list of 16 polygons (multipolygons), and nz_height is a bunch of points, it found the mean value of all listed points within the polygons. So some came back as NAs if there aren't any high points in that polygon and some came back as averages if there were more than 1. There are 16 multipolygons in the result. That's a pretty complex procedure for one line of code. Definitely want to keep this in mind but I think I prefer the tidy version:
nz_avheight2 = nz %>%
  st_join(nz_height) %>%
  group_by(Name) %>%
  summarize(elevation = mean(elevation, na.rm = TRUE))
# result looks slightly different because it's a tibble, rather than a df

# NOTE: there are sometimes issues where polygons don't line up exactly. For example, an LGA inside an SUA. Sometimes an LGA will be part of two different SUAs. Coping with this is a whole subdiscipline of geopreocessing. One solution is to divide summed values attributed to the straddling polygon using the weight of area that is divided by the split. In other words- a larger area on one side of the split would receive the same proportion of a summed value. If it's an average, this is applicable.
# use st_interpolate_aw() to split summary values between two polygons weighted by their area.
agg_aw = st_interpolate_aw(incongruent[, "value"], aggregating_zones,
                           extensive = TRUE)
agg_aw$value

# 4.2.6 distance relations
# calculate heightest point in NZ
nz_heighest = nz_height %>% 
  top_n(n = 1, wt = elevation)
# calculate centroid for canterbury shire
canterbury_centroid = st_centroid(canterbury)
# what's the distance between the centroid and the heighest point?
st_distance(nz_heighest, canterbury_centroid)
# let's plot
plot(st_geometry(nz))
plot(st_geometry(nz_heighest), add = TRUE, pch = 21, col = "black", bg = "blue")
plot(st_geometry(canterbury_centroid), add = TRUE, pch = 21, col = "black", bg = "red")

# st_distance returns a matrix, even when there's only one returned value. It can also return a matrix of multiple calculations:
co = filter(nz, grepl("Canter|Otag", Name))
st_distance(nz_height[1:3, ], co)
# ^this calculates all combinations of distances between the three points selected from nz_height and the two polygons in "co"
# the last two values in the second column are zero because the second two points are within the second polygon. st_distance calculates the distance between points and <i>any part of</i> polygons. Verfiy with:
plot(st_geometry(co)[2])
plot(st_geometry(nz_height)[2], add = TRUE, col = "red")
plot(st_geometry(nz_height)[3], add = TRUE, col = "blue")

## 4.3 spatial operations on raster data
# 4.3.1 spatial subsetting
# use coordinates to extract the value at a specific point
# get the cell ID
id = cellFromXY(elev, xy = c(0.1, 0.1))
elev[id]
# OR
raster::extract(elev, data.frame(x = 0.1, y = 0.1))
# ^be careful with extract(), it is also a function tidyverse

# clip raster using another raster
clip = raster(xmn = 0.9, xmx = 1.8, ymn = -0.45, ymx = 0.45,
              res = 0.3, vals = rep(1, 9))
elev[clip]
# plot
plot(elev)
plot(clip, add = TRUE, col = adjustcolor("yellow", alpha = 0.5))
# ^this seems odd. clip doesn't overlap completely with any pixels in elev. It intersects four pixels but the subset returns two values. Maybe it has to intersect a certain threshold of the area of the pixels to select them?

# subset into a spatial object instead
elev[1:2, drop = FALSE]    # spatial subsetting with cell IDs
elev[1, 1:2, drop = FALSE] # spatial subsetting by row,column indices

# create raster mask- values are randomly assigned "NA" and "TRUE"
rmask = elev 
values(rmask) = sample(c(NA, TRUE), 36, replace = TRUE)
# spatial subsetting using a mask
elev[rmask, drop = FALSE]           # with [ operator
mask(elev, rmask)                   # with mask()
overlay(elev, rmask, fun = "max")   # with overlay

# 4.3.2 map algebra
# Local- per-cell operations
# Focal or neighborhood operations- usually a 3x3 cell block summary
# Zonal- just like focal but with irregular shapes
# Global- whole raster or multiple raster summary

# 4.3.3 local operations
# reclassify a continuous variable into categorical
# first create a reclassification matrix, where the first column is the low value in each category, the second column is the high value in each category and the third column is the replacement value
rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
recl = reclassify(elev, rcl = rcl)

# simple examples of raster calculations supported by Raster. Google vignette Raster for further info
elev + elev
elev^2
log(elev)
elev > 5

# calc() and overlay() are more efficient so these should be used for large raster files. They also allow you to directly store an output file.

# NDVI is a local operation
# NDVI = (NIR - Red) / (NIR + Red)

# 4.3.4 focal operations
# considers the neighbourhood of a cell- also called kernel, filter or moving window
# calculate the minimum for each cell from a window of 3x3. The weight of all cells in the filtering matrix are equal and set to 1 but this can be changed.
r_focal = focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)
plot(elev)
plot(r_focal, add = TRUE)
# ^NOTE: the resulting raster retains the same spatial extent and dimensions of the original raster file but the outer edges of pixels become NAs for obvious reasons.

# focal operations can smooth or accentuate images and are an integral part of image processing.

# 4.3.5 zonal operations
# this is like a classic method for watershed analyses
# you can use attributes to calculate statistics- but how would you input a watershed?
## you would need to assign IDs to all the watersheds and calculate the stats for each ID
z = zonal(elev, grain, fun = "mean") %>%
  as.data.frame()
z

# 4.3.6 global operations and distances
# this section is confusing. One example of a global operation is to calculate distance between one pixel and all other pixels. You can also weight these distances using elevation, for example. This allows for calculation of Euclidean distances.
# they then caution that functions used for vectors and rasters can use the same word but have different meanings. Aggergating vectors is a summary exercise. Aggregating rasters increases the spatial resolution.

# 4.3.7 merging rasters
# this sections uses the term "scene" for some things and I don't quite get it. "The corresponding imagery is often divided into scenes covering a specific spatial extent. Frequently, a study area covers more than one scene." I think this refers to various "passes" of satellites that are stitched together. It might also refer to broken up chunks of rasters that are divided for downloading and processing convenience. Googling didn't help greatly but it might also mean something about 3D data?
# load scenes for Austria and Switzerland 
aut = getData("alt", country = "AUT", mask = TRUE)
ch = getData("alt", country = "CHE", mask = TRUE)
aut_ch = merge(aut, ch)
plot(aut_ch)
# if rasters overlap, merge() uses the first raster to fill the overlapping space. mosaic() let's you decide what to do with the overlapping area- such as compute the mean between the two rasters.
## gdalUtils::mosaic_rasters() is faster, and therefore recommended if you have to merge a multitude of large rasters stored on disk.

### see this section for more advice on scene blending and remote sensing resources for landsat, etc.

# 4.4 exercises
# 1












