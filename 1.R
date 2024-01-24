## #############################################################################
## warm-up exercise and security reminder

## we learned at the "Intro to R" course that we should not do this:
source('http://bit.ly/CEU-R-shoes')

## let's install a package instead!
install.packages('remotes')
remotes::install_github('daroczig/students')

library(students)
?students

## this is a dataset on students from a study group,
## where we run a math test and found interesting association with the shoe size
## TODO EDA
students

cor(students$shoe, students$math)
lm(math ~ shoe, students)

plot(students$shoe, students$math)
abline(lm(math ~ shoe, students), col = 'red')

library(ggplot2)
ggplot(students, aes(math, shoe)) + geom_point() + geom_smooth(method = 'lm')

## EDA - everyone!
str(students)
summary(students)
plot(students)

library(ggplot2)
ggplot(students, aes(math, shoe, color = z)) + geom_point()
ggplot(students, aes(math, shoe, color = y)) + geom_point() # !!

library(GGally)
ggpairs(students)

## https://datavizuniverse.substack.com/p/navigating-the-table-jungle
library(gtExtra) # grammar of tables, "g" like in "ggplot"
gt_plt_summary(students)

## partial correlation
residuals(lm(math ~ x, students))
residuals(lm(shoe ~ x, students))
cor(residuals(lm(math ~ x, students)), residuals(lm(shoe ~ x, students)))

library(psych)
partial.r(students, 1:2, 3)

plot(residuals(lm(math ~ x, students)), residuals(lm(shoe ~ x, students)))
abline(lm(residuals(lm(math ~ x, students)) ~ residuals(lm(shoe ~ x, students))))

plot(residuals(lm(math ~ x, students)), residuals(lm(shoe ~ x, students)))
abline(lm(residuals(lm(shoe ~ x, students)) ~ residuals(lm(math ~ x, students))))

## had enough
rm(list = ls())

## but wow!
.secret # "A warm hello from the Internet."
## TODO look at the source code of the package!
## TODO always install from trusted source

## #############################################################################
## intro / recap on R and ggplot2 from previous courses by introducing MDS

## distance between 40 Hungarian cities -> 2D scatterplot
## http://bit.ly/hun-cities-distance

## download data to a file in your temp folder
t <- tempfile()
t
t <- tempfile(fileext = '.xls')
t

## or keep in the current working directory
t <- 'cities.xls'

download.file('http://bit.ly/hun-cities-distance', t)
## on windows, you might need to specify the mode as well
download.file('http://bit.ly/hun-cities-distance', t, mode = 'wb')

## ALTERNATIVE sources for Austrian and German cities
## https://au.drivebestway.com/mileage-chart-with-distances-between-cities/at.xls?measure=metric&type=road
## https://au.drivebestway.com/mileage-chart-with-distances-between-cities/at-vienna.xls?measure=metric&type=air
download.file('https://bit.ly/at-cities-distance', 'cities.xls', mode = 'wb')
## https://au.drivebestway.com/mileage-chart-with-distances-between-cities/de.xls?measure=metric&type=air
download.file('https://bit.ly/de-cities-distance', 'cities.xls', mode = 'wb')

## further checks on the downloaded file
file.info(t)
pander::openFileInOS(t)

## read the downloaded file
library(readxl)
cities <- read_excel(t)

cities
## tibble VS data.frame VS data.table
str(cities)

## get rid of 1st column and last three rows (metadata)
cities <- cities[, -1]
cities <- cities[1:(nrow(cities) - 3), ]
str(cities)

mds <- cmdscale(as.dist(cities))
mds

plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## TODO interpret what we see
## NOTE Dusseldorf should be on West, Munich on the South, Berlin/Bremen on North, Dresden on East

## flipping both x and y axis
mds <- -mds
plot(mds)
text(mds[, 1], mds[, 2], names(cities))
## flipping only on y axis
mds[, 1] <- -mds[, 1]
plot(mds)
text(mds[, 1], mds[, 2], names(cities))
## flipping only on x axis
mds[, 2] <- -mds[, 2]
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## TODO ggplot2 way
mds <- as.data.frame(mds)
mds$city <- rownames(mds)
str(mds)

library(ggplot2)
ggplot(mds, aes(V1, V2, label = city)) +
    geom_text() + theme_bw()

## flip one axis and grid
ggplot(mds, aes(V1, -V2, label = city)) +
    geom_text() + theme_void()

## #############################################################################
## TODO visualize the distance between the European cities
## stored in the built-in dataframe:

?eurodist

mds <- cmdscale(eurodist)
mds <- as.data.frame(mds)
mds$city <- rownames(mds)
ggplot(mds, aes(V1, -V2, label = city)) +
    geom_text() + theme_bw()

## ############################################################################
## geocoding

## look up countrycode pkg on CRAN

## test on https://nominatim.openstreetmap.org/ui/search.html
## e.g. "CEU, Budapest, Hungary"

library(ggmap)          # geocode one address
library(tidygeocoder)   # geocode a data.frame

library(data.table)
mds <- data.table(geocode(mds, 'city'))
str(mds)

## built-in polygons for the background
?maps::world
world <- map_data('world')
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
    coord_fixed(1.3)

## adding the points
ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
    coord_fixed(1.3) +
    geom_point(data = mds, aes(long, lat), color = 'orange')

## wow, that's odd to see a few cities from EU in USA :O
mds

ggplot() +
    geom_map(data = world, map = world, aes(long, lat, map_id = region)) +
    coord_fixed(1.3) +
    geom_point(data = mds, aes(long, lat, color = city))

## make it a thematic map
str(world)
world$a <- grepl('^A', world$region)

ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region, fill = a)) +
  coord_fixed(1.3) +
  geom_point(data = mds, aes(long, lat), color = 'black') +
  theme(legend.position = 'none')


## fancy background from http://maps.stamen.com -- unfortunately deprecated,
## so need to register a key from get_stadiamap
library(ggmap)
register_stadiamaps('YOUR-API-KEY')

map <- get_stadiamap(
  c(
    left = min(mds$long) * 0.995,
    right = max(mds$long) * 1.001,
    bottom = min(mds$lat) * 0.999,
    top = max(mds$lat)) * 1.001,
  maptype = 'stamen_toner',
  zoom = 4)

ggmap(map) +
  geom_point(data = mds, aes(long, lat), color = 'orange') +
  theme_void() + theme(legend.position = 'none')

library(sf)
geomds <- st_as_sf(x = mds, coords = c("long", "lat"))
st_bbox(geomds)
unname(st_bbox(geomds))

map <- get_stadiamap(
    unname(st_bbox(geomds)),
    maptype = 'stamen_terrain_background',
    zoom = 4)

ggmap(map) +
    geom_point(data = mds, aes(long, lat), color = 'black') +
    theme_void() + theme(legend.position = 'none')

## #############################################################################
## plotting shapefiles
## #############################################################################

## search for "Austria shapefile"  University of Texas
## https://geodata.lib.utexas.edu/?f%5Bdc_format_s%5D%5B%5D=Shapefile&f%5Bdc_subject_sm%5D%5B%5D=Boundaries&f%5Bdct_spatial_sm%5D%5B%5D=Austria&f%5Blayer_geom_type_s%5D%5B%5D=Polygon&per_page=20
download.file(
    'https://stacks.stanford.edu/file/druid:rc343vz5889/data.zip',
    'Austria_boundary.zip')
download.file(
    'https://stacks.stanford.edu/file/druid:yv617vc9132/data.zip',
    'Austria_divisions.zip')
unzip('Austria_boundary.zip')
unzip('Austria_divisions.zip')

## Look around what we have
library(sf) # simple features
st_layers('.')

## Read a shapefile from the current working directory (refer to the filename without file extension)
adm0 <- st_read('.', layer = 'AUT_adm0')
str(adm0)

plot(adm0)

st_geometry(adm0)
ggplot() + geom_sf(data = adm0)

## Now let's load smaller administrative areas as well
adm2 <- st_read('.', 'AUT_adm2')

## And some points to be added to the map as well
cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')

## Party time
library(ggplot2)
ggplot() +
    geom_sf(data = adm0, color = 'black', size = 1) +
    geom_sf(data = adm2, color = 'gray', size = .2) +
    geom_point(data = cities,
               aes(lng, lat, size = population),
               color = 'orange') +
    theme_void() +
    theme(legend.position = 'top')

## how to access the original data elements?
str(adm2)
adm2$NAME_2

## add label
ggplot() +
    geom_sf(data = adm0, color = 'black', size = 1) +
    geom_sf(data = adm2, color = 'gray', size = .2) +
    geom_sf_text(data = adm2, aes(label = NAME_2)) +
    geom_point(data = cities,
               aes(lng, lat, size = population),
               color = 'orange') +
    theme_void() +
    theme(legend.position = 'top')

## thematic maps: Statutory city VS
ggplot() +
    geom_sf(data = adm0, color = 'black', size = 1) +
    geom_sf(data = adm2, color = 'gray', aes(fill = TYPE_2), size = .2) +
    geom_sf_text(data = adm2, aes(label = NAME_2)) +
    geom_point(data = cities,
               aes(lng, lat, size = population),
               color = 'orange') +
    theme_void() +
    theme(legend.position = 'top')

## using geojson files
download.file(
    'https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json',
    'austria.geojson')

library(leaflet)

map <- st_read('austria.geojson')
popup <- paste0('<strong>Name: </strong>', map$name)

leaflet(map) %>%
    addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        popup = popup,
        label = ~name,
        layerId = ~name,
        labelOptions = labelOptions(noHide = TRUE),
        highlightOptions = highlightOptions(
            color = 'white', weight = 2,
            bringToFront = TRUE))

library(ggplot2)
ggplot() +
    geom_sf(data = map, color = 'white', fill = 'darkgreen', size = .2) +
    theme_void()

## find more details at https://tmieno2.github.io/R-as-GIS-for-Economists/index.html

## #############################################################################
## TODO non-geo example

?mtcars
str(mtcars)
mtcars

mds <- cmdscale(dist(mtcars))
plot(mds)
text(mds[, 1], mds[, 2], rownames(mtcars))
## oh no, the overlaps!

mds <- as.data.frame(mds)
mds$car <- rownames(mds)
ggplot(mds, aes(V1, V2, label = car)) +
    geom_text() + theme_bw()

library(ggrepel)
ggplot(mds, aes(V1, V2, label = car)) +
    geom_text_repel() + theme_bw()

## #############################################################################
## QQ what does it mean that two cards are "close to each other"?
## NOTE think about why the above visualization is off

## check actual distances eg for Camaro (or other sport cars)
which(rownames(mtcars) == 'Camaro Z28')
sort(as.matrix(dist(mtcars))[, 24])
## Mercedes sedans are closer?! than e.g. Ferrari Dino or Maserati Bora

mtcars

subset(mtcars, hp >= 245)

?cmdscale
?dist

summary(mtcars)

## need to standardize to give every variable equal weight!
mtcars$hp - mean(mtcars$hp)
mean(mtcars$hp - mean(mtcars$hp))

(x <- (mtcars$hp - mean(mtcars$hp)) / sd(mtcars$hp))
mean(x)
sd(x)
hist(x)

x
scale(mtcars$hp)
plot(x, scale(mtcars$hp))
x - scale(mtcars$hp)

plot(mtcars$hp, scale(mtcars$hp))

?scale
scale(mtcars)

mds <- cmdscale(dist(scale(mtcars)))
mds <- as.data.frame(mds)
mds$car <- rownames(mds)
ggplot(mds, aes(V1, V2, label = car)) +
    geom_text_repel() + theme_bw()

subset(mtcars, hp >= 200)

## #############################################################################
## introduction to Simpson's paradox with the Berkeley example

## then do the analysis in R
UCBAdmissions
plot(UCBAdmissions)

berkeley <- as.data.frame(UCBAdmissions)

ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + geom_col()

p <- ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + geom_col(position = 'fill')
p

p + facet_wrap(~Dept)
p + facet_wrap(~Dept) + scale_fill_manual(values = c('Admitted' = 'darkgreen', 'Rejected' = 'red'))
# https://colorbrewer2.org
p + facet_wrap(~Dept) + scale_fill_brewer(palette = 'Dark2')

ggplot(berkeley, aes(Gender, Freq, fill = Admit)) + geom_col() +
    facet_wrap(~Dept) + scale_fill_brewer(palette = 'Dark2')

## #############################################################################
## TODO exercise visualize a model on the association between
## the lengh and width of sepal in the iris dataset

?iris
str(iris)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
    geom_smooth()
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
    geom_smooth(method = 'lm', se = FALSE)
## note the change in the sign of the slope!
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point() +
    geom_smooth(method = 'lm', se = FALSE)

summary(lm(Sepal.Width ~ Sepal.Length, data = iris))
summary(lm(Sepal.Width ~ Sepal.Length + Species, data = iris))

## NOTE when checking the scatterplot colored by Species
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

## TODO add model
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point() + geom_smooth(method = 'lm')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point(aes(color = Species)) +
    geom_smooth(aes(color = Species), method = 'lm') +
    geom_smooth(method = 'lm', color = 'black', linewidth = 2)

## compare the overlap of groups with the MDS version:
mds <- as.data.frame(cmdscale(dist(iris)))
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point()
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point() + geom_smooth(method = 'lm')

## without the Species column
mds <- as.data.frame(cmdscale(dist(iris[, -5])))
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point()

## #############################################################################
## back to dataviz ... why? let's see Anscombe's quartett

## dirty data
anscombe

## select 1st set
anscombe[, c(1, 5)]

## using base R to see the 1st set
plot(anscombe[, c(1, 5)])

## add linear model
lm(anscombe[, c(5, 1)])
## note to change 5 and 1 (estimating y based on x)
abline(lm(anscombe[, c(5, 1)]))

library(ggplot2)
ggplot(anscombe[, c(1, 5)], aes(x1, y1)) + geom_point() + geom_smooth(method = 'lm', se = TRUE)
ggplot(anscombe[, c(2, 6)], aes(x2, y2)) + geom_point() + geom_smooth(method = 'lm', se = TRUE)

## TODO compute the mean of x1 and y1
mean(anscombe[, 1])
mean(anscombe[, 5])

## TODO compute the mean of x2 and y2
mean(anscombe[, 2])
mean(anscombe[, 6])

## intro to loops
lapply(1:4, function(i) mean(anscombe[, c(i)]))
lapply(1:4, function(i) sd(anscombe[, c(i)]))
lapply(1:4, function(i) cor(anscombe[, c(i, i+4)]))

apply(anscombe, 2, mean)

## loop to create separate & tidy dataset for each set
lapply(1:4, function(i) anscombe[, c(i, i+4)])

## use data.table to merge into one single data frame
library(data.table)
rbindlist(lapply(1:4, function(i) anscombe[, c(i, i+4)]))
## add a "set" column so that we know which set the row belongs to
rbindlist(lapply(1:4, function(i) cbind(dataset = i, anscombe[, c(i, i+4)])))

## save in a variable for future use
df <- rbindlist(lapply(1:4, function(i) cbind(dataset = i, anscombe[, c(i, i+4)])))
setnames(df, c('dataset', 'x', 'y'))

## let's switch from base R to ggplot and show all 4 sets in subplots
library(ggplot2)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset) + geom_smooth()
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset) + geom_smooth(method = 'lm')

## intro to https://www.research.autodesk.com/publications/same-stats-different-graphs/
df <- datasauRus::datasaurus_dozen_wide
dino_df <- rbindlist(lapply(seq(1, 26, by = 2), function(i) {
    data.frame(
        x = df[, c(i), drop = TRUE],
        y = df[, c(i+1), drop = TRUE],
        # sub('_x$', '', names(df)[i])
        dataset = substr(names(df)[i], 1, nchar(names(df)[i])-2))
}
))

ggplot(dino_df, aes(x, y)) + geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    facet_wrap(~dataset) +
    theme_bw()

## alternative solution
dino_df <- datasauRus::datasaurus_dozen
