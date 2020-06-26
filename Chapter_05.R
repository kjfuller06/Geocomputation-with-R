.libPaths("C:/Users/90946112/R/win-library/3.6.2")
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(rmapshaper)

# 5.2.1 simplification
# st_simpify() reduces the number of vertices in lines and polygons
# plot(st_geometry(seine))
seine_simp = st_simplify(seine, dTolerance = 2000) #2000 m
# plot(st_geometry(seine_simp), col = "blue", add = TRUE)
object.size(seine)
#> 18,096 bytes
object.size(seine_simp)
#> 9,112 bytes

# GEOS(?) assumes data are in a projected CRS, not a geographic CRS. Reproject states and simplify
us_states2163 = st_transform(us_states, 2163)
us_states_simp1 = st_simplify(us_states2163, dTolerance = 100000)  # 100 km
plot(st_geometry(us_states2163))
plot(st_geometry(us_states_simp1))
## A limitation with st_simplify() is that it simplifies objects on a per-geometry basis. This means the ‘topology’ is lost, resulting in overlapping and ‘holy’ areal units. ms_simplify() from rmapshaper provides an alternative that overcomes this issue. By default it uses the Visvalingam algorithm, which overcomes some limitations of the Douglas-Peucker algorithm 
us_states2163$AREA = as.numeric(us_states2163$AREA)
us_states_simp2 = rmapshaper::ms_simplify(us_states2163, keep = 0.01,
                                          keep_shapes = TRUE)
plot(st_geometry(us_states_simp2))
## topology not lost

# 5.2.2 centroids. st_centroid() takes the absolute center of the polygon. st_point_on_surface() makes sure the point is actually on the polygon and not floating in space.
nz_centroid = st_centroid(nz)
seine_centroid = st_centroid(seine)
nz_pos = st_point_on_surface(nz)
seine_pos = st_point_on_surface(seine)

# NOTE: Other types of centroids exist, including the Chebyshev center and the visual center

# 5.2.3 buffers
seine_buff_5km = st_buffer(seine, dist = 5000)
seine_buff_50km = st_buffer(seine, dist = 50000)
plot(seine_buff_5km)
plot(seine_buff_50km)

# NOTE: The third and final argument of st_buffer() is nQuadSegs, which means ‘number of segments per quadrant’ and is set by default to 30 (meaning circles created by buffers are composed of 4×30=120 lines). This argument rarely needs to be set. Unusual cases where it may be useful include when the memory consumed by the output of a buffer operation is a major concern (in which case it should be reduced) or when very high precision is needed (in which case it should be increased). 

# 5.2.4 affine transformations
# Affine transformation is any transformation that preserves lines and parallelism. However, angles or length are not necessarily preserved. Affine transformations include, among others, shifting (translation), scaling and rotation. Additionally, it is possible to use any combination of these. 
nz_sfc = st_geometry(nz)
# move every point in a polygon along the y axis by 100,000 m
nz_shift = nz_sfc + c(0, 100000)

# Scaling enlarges or shrinks objects by a factor. It can be applied either globally or locally. Global scaling increases or decreases all coordinates values in relation to the origin coordinates, while keeping all geometries topological relations intact. It can be done by subtraction or multiplication of asfg or sfc object.

# Local scaling treats geometries independently and requires points around which geometries are going to be scaled, e.g., centroids. In the example below, each geometry is shrunk by a factor of two around the centroids (middle panel in Figure 5.5). To achieve that, each object is firstly shifted in a way that its center has coordinates of 0, 0 ((nz_sfc - nz_centroid_sfc)). Next, the sizes of the geometries are reduced by half (* 0.5). Finally, each object’s centroid is moved back to the input data coordinates (+ nz_centroid_sfc).
nz_centroid_sfc = st_centroid(nz_sfc)
nz_scale = (nz_sfc - nz_centroid_sfc) * 0.5 + nz_centroid_sfc
# plot
plot(st_geometry(nz_sfc), expandBB = c(-0.25, -0.25, -0.25, -0.25))
plot(st_geometry(nz_centroid_sfc), add=TRUE)
plot(st_geometry(nz_scale), expandBB = c(-0.25, -0.25, -0.25, -0.25))

# rotating a 2D object requires a rotation matrix
rotation = function(a){
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 
# The rotation function accepts one argument a - a rotation angle in degrees. Rotation could be done around selected points, such as centroids
# rotate nz by 30 radians
nz_rotate = (nz_sfc - nz_centroid_sfc) * rotation(30) + nz_centroid_sfc
# ^this rotates each polygon in nz separately
plot(st_geometry(nz_sfc))
plot(st_geometry(nz_rotate), add= TRUE)

# reset geometries with newly calculated geometries with st_set_geometry()
nz_scale_sf = st_set_geometry(nz, nz_scale)

# 5.2.5 clipping
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # create 2 points
b = st_buffer(b, dist = 1) # convert points to circles
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text

x = b[1]
y = b[2]
# st_intersection() grabs the area overlapped by both polygons
x_and_y = st_intersection(x, y)
plot(b)
plot(x_and_y, col = "lightgrey", add = TRUE) # color intersecting area

# st_sym_difference does the opposite- it takes the areas in x and y that aren't overlapping and combines them into one shape- probably handy for fire history polygons
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text
plot(st_sym_difference(x, y), col = adjustcolor("lightgrey", alpha = 0.5), add = TRUE)

# st_difference(x, y) takes only the part of x that is not overlapping with y
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text
plot(st_difference(x, y), col = adjustcolor("lightgrey", alpha = 0.5), add = TRUE)
# and vice versa
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text
plot(st_difference(y, x), col = adjustcolor("lightgrey", alpha = 0.5), add = TRUE)

# union() grabs the area of both circles and merges the boundaries to make one shape
plot(b)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y")) # add text
plot(st_union(y, x), col = adjustcolor("lightgrey", alpha = 0.5), add = TRUE)

# subset points using a geometry operation
# generate points in and around the circles
bb = st_bbox(st_union(x, y))
box = st_as_sfc(bb)
set.seed(2017)
p = st_sample(x = box, size = 10)
plot(box)
plot(x, add = TRUE)
plot(y, add = TRUE)
plot(p, add = TRUE)
text(x = c(-0.5, 1.5), y = 1, labels = c("x", "y"))

# two methods to select only those points within either x or y
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy1 = p[sel_p_xy]
p_xy2 = p[x_and_y]
identical(p_xy1, p_xy2)

# 5.2.6 Geometry unions
# spatial aggregation can silently dissolve the geometries of touching polygons in the same group. 
regions = aggregate(x = us_states[, "total_pop_15"], by = list(us_states$REGION),
                    FUN = sum, na.rm = TRUE)
regions2 = us_states %>% group_by(REGION) %>%
  summarize(pop = sum(total_pop_15, na.rm = TRUE))
plot(st_geometry(regions))
# What is going on in terms of the geometries? Behind the scenes, both aggregate() and summarize() combine the geometries and dissolve the boundaries between them using st_union(). This is demonstrated in the code chunk below which creates a united western US:
us_west = us_states[us_states$REGION == "West", ]
us_west_union = st_union(us_west)
plot(st_geometry(us_west))




