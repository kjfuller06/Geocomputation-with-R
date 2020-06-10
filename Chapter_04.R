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






