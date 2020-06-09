# lassign library path
.libPaths("C:/Users/90946112/R/win-library/3.6.2")

# Load packages for chapter 3
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(dplyr)
library(stringr)
library(tidyverse)
library(spData)

# what methods are available for sf data?
methods(class = "sf")

# check out the df in the sf
dim(world) # it is a 2 dimensional object, with rows and columns
nrow(world) # how many rows?
ncol(world)
world_df = st_drop_geometry(world)
class(world_df)

# dplyr functions
world4 = dplyr::select(world, name_long, population = pop)
names(world4)

# more concise than the base R equivalent:
world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[names(world5) == "pop"] = "population"
names(world5)

# select() also works with ‘helper functions’ for advanced subsetting operations, including contains(), starts_with() and num_range() (see the help page with ?select for details).

# Most dplyr verbs return a data frame. To extract a single vector, one has to explicitly use the pull() command. The subsetting operator in base R (see ?[), by contrast, tries to return objects in the lowest possible dimension. This means selecting a single column returns a vector in base R. To turn off this behavior, set the drop argument to FALSE. As follows:
# create throw-away data frame
d = data.frame(pop = 1:10, area = 1:10)
# return data frame object when selecting a single column
d[, "pop", drop = FALSE]
select(d, pop)
# return a vector when selecting a single column
d[, "pop"]
pull(d, pop)


# Due to the sticky geometry column, selecting a single attribute from an sf-object with the help of [() returns also a data frame. Contrastingly, pull() and $ will give back a vector.
# data frame object
world[, "pop"]
# vector objects
world$pop
pull(world, pop)

# slice() is the row-equivalent of select(). The following code chunk, for example, selects the 3rd to 5th rows:
slice(world, 3:5)

# filter() is dplyr’s equivalent of base R’s subset() function. It keeps only rows matching given criteria, e.g., only countries with a very high average of life expectancy:
# Countries with a life expectancy longer than 82 years
world6 = filter(world, lifeExp > 82)
world6[, "lifeExp"]

# Now let's do something with it
world7 = world %>%
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)
world7

# group variables and do operations
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
world_agg1
# returns a df
class(world_agg1)

# aggregate() is a generic function which means that it behaves differently depending on its inputs. sf provides a function that can be called directly with sf:::aggregate() that is activated when a ***~by~*** argument is provided, rather than using the ~ to refer to the grouping variable:
world_agg2 = aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)
world_agg2
# this retains the geom of "continent" so it is still an sf object
class(world_agg2)

# summarize() is the dplyr equivalent of aggregate(). It usually follows group_by(), which specifies the grouping variable, as illustrated below:
world_agg3 = world %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))
world_agg3

# Let's create the names of the results. pop and n are column names in the result. sum() and n() were the summaring functions. The result is an sf object with a single row representing the world (this works thanks to the geometric operation ‘union’, as explained in Section 5.2.6).
whole = world %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n = n()) %>% 
  glimpse()
plot(whole)

# combining functions. Find the three most populous continents and the number of countries that make them up.
world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
  top_n(n = 3, wt = pop) %>%
  st_drop_geometry() 

# join left (preserve the left dataset)
world_coffee = left_join(world, coffee_data)
class(world_coffee)
names(world_coffee)
plot(world_coffee["coffee_production_2017"])

# inner join (keeps only the records that match both)
world_coffee_inner = inner_join(world, coffee_data)
world_coffee_inner
#lost two that didn't match "world"; Dem of Congo was abbreviated so it didn't match
setdiff(coffee_data$name_long, world$name_long)
str_subset(world$name_long, "Dem*.+Congo")
# fix it
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = 
  str_subset(world$name_long, "Dem*.+Congo")
world_coffee_match = inner_join(world, coffee_data)
nrow(world_coffee_match)

# create an sf object from a df. In this case, a left join that forces the sf into a df. It retains its geometry but it loses its class in the process
coffee_world = left_join(coffee_data, world)
coffee_world
class(coffee_world)
st_as_sf(coffee_world)

## 3.2.4
# create new columns
world %>% 
  mutate(pop_dens = pop / area_km2)
# transmute removes all other columns, retaining the geometry
world %>% 
  transmute(pop_dens = pop / area_km2)
# combine strings of two columns
world_unite = world %>%
  unite("con_reg", continent:region_un, sep = ":", remove = TRUE)
world_unite["con_reg"]
# and separate them again
world_separate = world_unite %>% 
  separate(con_reg, c("continent", "region_un"), sep = ":")
world_separate
# renaming
world %>% 
  rename(name = name_long)
new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world %>% 
  set_names(new_names)

# drop geometry
world_data = world %>% st_drop_geometry()
class(world_data)

## 3.3
# create a raster object from scratch
elev = raster(nrows = 6, ncols = 6, res = 0.5,
              xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
              vals = 1:36)
elev

# categorical values
grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = raster(nrows = 6, ncols = 6, res = 0.5, 
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = grain_fact)

# note: vectors cannot contain character values

# categorical variables are stored as integers
grain[1, 1]
plot(grain)

# ooooh ok so you have a raster, which represents categories as integers for fast computation. Then you have the Raster Attribute Table (RAT) for reference or look-up table
levels(grain)[[1]] = cbind(levels(grain)[[1]], wetness = c("wet", "moist", "dry"))
levels(grain)
grain

# look at the values for specific cells (why would you ever want to do this?)
factorValues(grain, grain[c(1, 11, 35)])

## 3.3.1
# subsetting uses the classic base index []
# row 1, column 1
elev[1,1]
# cell ID 1
elev[1]

# values() and getValues() are used to extract all values or complete rows. For mulit-layered raster objects (stack or brick), this returns the cell values for each raster in the stack. Example: stack(elev, grain)[1] returns a matrix with one row and two columns, the first cell values for elev and grain. You can also subset a multi-layer raster using raster::subset().
r_stack = stack(elev, grain)
names(r_stack) = c("elev", "grain")
# three ways to extract a layer of a stack
raster::subset(r_stack, "elev")
r_stack[["elev"]]
r_stack$elev

# change the value of one cell
elev[1, 1] = 0
elev[]
# or multiple values
elev[1, 1:2] = 0
elev[]

## 3.3.2
# calculate quick stats
cellStats(elev, sd)
# calculate stats for a stack or brick
summary(brick(elev, grain))

# boxplot(), density(), hist() and pairs() work on raster objects as well
hist(elev)
# if other visualisations don't work with raster objects, just use values or getValues of [] to extract the values first, then visualise

## 3.4 Exercises
data(us_states)
data(us_states_df)

# 1
us_states_name = us_states[, "NAME"]
class(us_states_name)
# solution from the book has same result
us_states_name = us_states %>% dplyr::select(NAME)
class(us_states_name)

# 2
library(tidyselect)
us_states[,grep("pop",names(us_states))]
us_states %>% 
  select(contains("pop"))
us_states %>% 
  select(starts_with("total_pop"))
# solutions. note: it's important to include dplyr:: infront of "select" because there is conflict with this function
us_states %>% dplyr::select(total_pop_10, total_pop_15)
us_states %>% dplyr::select(starts_with("total_pop"))
us_states %>% dplyr::select(contains("total_pop"))

# 3
# find
us_states[us_states$REGION=="Midwest",]
midwest = us_states %>% 
  filter(REGION == "Midwest")
# plot
plot(midwest[,"NAME"])

# find
# gave up. solution:
west = us_states %>% 
  filter(REGION == "West", AREA < units::set_units(250000, km^2),total_pop_15 > 5000000)
# plot
plot(west[,"REGION"])

#find
south = us_states %>% 
  filter(REGION == "South", AREA > units::set_units(150000, km^2),total_pop_15 > 7000000)
# also this option from solutions
us_states %>% 
  filter(REGION == "South", as.numeric(AREA) > 150000, total_pop_15 > 7000000)
# plot
us = world %>% 
  filter(continent == "North America")
plot(us$geom)
plot(south[, "total_pop_15"], add = TRUE)
# IDK can't get it to plot over the us_states sf. No solution provided for plotting. Managed to do it with the world dataset.

# 4
totalpop = sum(us_states$total_pop_15)
totalpop
minmax = c(min(us_states$total_pop_15),max(us_states$total_pop_15))
minmax
# solution
us_states %>% summarize(total_pop = sum(total_pop_15),
                        min_pop = min(total_pop_15),
                        max_pop = max(total_pop_15))

# 5
us_states %>% 
  group_by(REGION) %>% 
  summarize(num = n())

# 6
us_states %>% 
  group_by(REGION) %>% 
  summarize(pop = sum(total_pop_15),
            min_pop = min(total_pop_15),
            max_pop = max(total_pop_15))

# 7
us_states_stats = us_states %>% 
  left_join(us_states_df, by = c("NAME" = "state"))
class(us_states_stats)

# 8
us_states_df %>% 
  anti_join(us_states, by = c("state" = "NAME"))

# 9 
us_states2 = us_states %>% 
  mutate(dens_15 = total_pop_15/AREA, 
         dens_10 = total_pop_10/AREA)
us_states2

# 10
us_states2 = us_states2 %>% 
  mutate(change = ((dens_15-dens_10)/dens_10)*100)
plot(us_states2["change"])

# 11
names(us_states) = tolower(names(us_states))
# from solutions
us_states %>%
  setNames(tolower(colnames(.)))

# 12
us_states_sel = us_states_stats %>% 
  select(Income = "median_income_15")

# 13- worded badly, had to change because of solutions code
us_states_sel2 = us_states_stats %>% 
  mutate(change = median_income_15 - median_income_10)
us_states_sel3 = us_states_sel2 %>% 
  group_by(REGION) %>% 
  summarize(minin = min(change),
            maxin = max(change),
            avgin = mean(change))
us_states_sel4 = us_states_sel3 %>% 
  group_by(REGION) %>% 
  summarize(avginin = mean(avgin)) %>% 
  top_n(n = 1, wt = avginin)
# from solutions
us_states_sel3 %>%
  filter(avgin == max(avgin)) %>%
  pull(REGION) %>%
  as.character()

# 14, had to get the answer from solutions; had something to do with the xmn, xmx, etc. functions
test = raster(nrows = 9, ncols = 9, res = 0.5, xmn = 0, xmx = 4.5,
              ymn = 0, ymx = 4.5, vals = sample(81, x = c(1:10), replace = TRUE))
plot(test)
# extract values from the four corners (from solutions with a correction); using cell IDs
test[c(1, 9, (81 - 8), 81)]
# OR using row indexing
test[c(1, nrow(test)), c(1, ncol(test))]

# 15- straight from solutions
grain_size = c("clay", "silt", "sand")
grain = raster(nrow = 6, ncol = 6, res = 0.5,
               xmn = -1.5, xmx = 1.5, ymn = -1.5, ymx = 1.5,
               vals = factor(sample(grain_size, 36, replace = TRUE),
                             levels = grain_size))
cellStats(grain, modal) %>%
  factorValues(grain, .)
factorValues(grain, modal(values(grain)))

# 16- data not available


