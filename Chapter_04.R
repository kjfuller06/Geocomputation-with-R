.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(sf)
library(raster)
library(dplyr)
library(spData)

# 4.2.1 geographical subsetting
canterbury = nz %>% 
  filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]

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
## ok. I think because nz is a list of 16 polygons (multipolygons), and nz_height is a bunch of points, it found the mean value of all listed points within the polygons. So some came back as NAs if there aren't any high points in that polygon and some came back as averages if there were more than 1. There are 16 multipolygons in the result.
# OR
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







