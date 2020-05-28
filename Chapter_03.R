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

# dplyr functions
world4 = dplyr::select(world, name_long, population = pop)
names(world4)

# more concise than the base R equivalent:
world5 = world[, c("name_long", "pop")] # subset columns by name
names(world5)[names(world5) == "pop"] = "population"

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

# Now let's do something with it
world7 = world %>%
  filter(continent == "Asia") %>%
  dplyr::select(name_long, continent) %>%
  slice(1:5)

# group variables and do operations
world_agg1 = aggregate(pop ~ continent, FUN = sum, data = world, na.rm = TRUE)
class(world_agg1)

# aggregate() is a generic function which means that it behaves differently depending on its inputs. sf provides a function that can be called directly with sf:::aggregate() that is activated when a by argument is provided, rather than using the ~ to refer to the grouping variable:
world_agg2 = aggregate(world["pop"], by = list(world$continent),
                       FUN = sum, na.rm = TRUE)
# this retains the geom of "continent" so it is still an sf object
class(world_agg2)

# summarize() is the dplyr equivalent of aggregate(). It usually follows group_by(), which specifies the grouping variable, as illustrated below:
world_agg3 = world %>%
  group_by(continent) %>%
  summarize(pop = sum(pop, na.rm = TRUE))

# controlled common names. pop and n are column names in the result. sum() and n() were the aggregating functions. The result is an sf object with a single row representing the world (this works thanks to the geometric operation ‘union’, as explained in Section 5.2.6).
world %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n = n()) %>% 
  glimpse()

# combining functions. Find the three most populous continents and the number of countries that make them up.
world %>% 
  dplyr::select(pop, continent) %>% 
  group_by(continent) %>% 
  summarize(pop = sum(pop, na.rm = TRUE), n_countries = n()) %>% 
  top_n(n = 3, wt = pop) %>%
  st_drop_geometry() 









