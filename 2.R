## #############################################################################
## _potential_ homework solutions for the 1st week
## #############################################################################

## 1. Load `bookings` data from http://bit.ly/CEU-R-hotels-2018-prices and the hotel `features` from http://bit.ly/CEU-R-hotels-2018-features
library(data.table)
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

## 2. Count the number of 4 stars hotels in Hungary
features[stars == 4 & country == 'Hungary', .N]
nrow(features[stars == 4 & country == 'Hungary'])

## 3. Compute the average rating of 4 and 5 star hotels in Hungary and Germany
features[stars >= 4 & country %in% c('Hungary', 'Germany'), mean(rating, na.rm = TRUE)]
features[stars >= 4 & country %in% c('Hungary', 'Germany') & !is.na(rating), mean(rating)]

## 4. Round up the previously computed average rating to 2 digits
features[stars >= 4 & country %in% c('Hungary', 'Germany') & !is.na(rating), round(mean(rating), 2)]

## 5. Do we have any bookings in unknown hotels (as per the features dataset)?
bookings[!hotel_id %in% unique(features$hotel_id)]
features

## 6. Clean up the bookings dataset from bookings from unknown hotels and print the number of remaining bookings
bookings <- bookings[hotel_id %in% unique(features$hotel_id)]

## 7. What's the average distance of hotels from the city central in Budapest
features[city_actual == 'Budapest', mean(distance)]

## 8. List all neighbourhoods in Budapest
features[city_actual == 'Budapest', unique(neighbourhood)]
features[city_actual == 'Budapest', mean(distance), by = neighbourhood][, neighbourhood]

## 9. Compute the average distance from the city center for the neighbourhoods in Budapest
features[city_actual == 'Budapest', mean(distance), by = neighbourhood]

## 10. Count the number of bookings in Hungary
bookings[hotel_id %in% features[country == 'Hungary', hotel_id], .N]

## Homework extra:
## 1. Create a scatterplot on the `iris` dataset using the length and width of sepal + 4 linear models (3 colored lines per species, 1 black line fitted on the global dataset)
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point(aes(color = Species)) +
    geom_smooth(aes(color = Species), method = 'lm') +
    geom_smooth(method = 'lm', color = 'black') +
    theme(legend.position = 'top')

## #############################################################################
## why the dataviz? edu purposes, eg showing clustering
## #############################################################################

## hierarchical clustering
?hclust

## NOTE check the kmeans function or the cluster package's pam function for other methods
## NOTE check the NbClust package to find the optimal number of clusters

dm <- dist(iris[, 1:4])
str(dm)

hc <- hclust(dm)
str(hc)

## plot the dendogram
plot(hc)
rect.hclust(hc, k = 3)

## poor man's animation
for (i in 2:8) {
    plot(hc)
    rect.hclust(hc, k = i)
    Sys.sleep(1)
}

## actual animation!
library(animation)
ani.options(interval = 1)
saveGIF({
    for (i in 2:8) {
        plot(hc)
        rect.hclust(hc, k = i)
    }
})
?saveGIF
?ani.options
## ani.width, ani.height = 480

## ggplot for dendograms
library(dendextend)
d <- as.dendrogram(hc)
d <- color_branches(d, k = 2)
plot(d)
ggplot(d)

for (i in 2:8) {
    ## NOTE that we need to print
    print(ggplot(color_branches(as.dendrogram(hc), k = i)))
    Sys.sleep(1)
}

saveGIF({
    for (i in 2:8) {
        print(ggplot(color_branches(as.dendrogram(hc), k = i)))
    }
}, ani.width = 960)

## dedicated package for ggplot2-based animations: gganimate
library(gganimate)
## see more details later when dealing with objects actually created via ggplot2

## cluster membership
clusters <- cutree(hc, 3)
clusters

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top')
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top')

## add liner models by both species and cluster membership?
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top') +
    geom_smooth(method = 'lm')
## hm, that's difficult to interpret .. let's differentiate line types as well?

ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters), linetype = Species)) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top') +
    geom_smooth(method = 'lm')
## still not happy

## loop over Species?
library(data.table)
## need ~tidy data
IRIS <- as.data.table(iris)
IRIS$cluster <- factor(clusters)
saveGIF({
    for (species in unique(IRIS$Species)) {
        print(ggplot(IRIS[Species == species],
                     aes(Sepal.Length, Sepal.Width, color = cluster)) +
                  geom_point(size = 3) +
                  geom_smooth(method = 'lm') +
                  ggtitle(species)) # add later
    }
}, ani.width = 960)
## meh, we can do better

library(gganimate)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = clusters)) +
    geom_point(size = 3) +
    geom_smooth(method = 'lm') +
    transition_states(Species)
## NOTE check the scale for clusters ... also need to add title

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = factor(clusters))) +
    geom_point(size = 3) +
    geom_smooth(method = 'lm') +
    transition_states(Species) +
    labs(
        title = paste("{closest_state}"),
        subtitle = 'Number of flowers: {nrow(subset(iris, Species == closest_state))}')

## see more fancy plot at https://twitter.com/daroczig/status/1201542038640111616

## confusion matrix
table(iris$Species)
table(clusters)
table(iris$Species, clusters)

## TODO exercise cluster mtcars into 3 groups and visualize the clusters
hc <- hclust(dist(scale(mtcars)))
plot(hc)

## TODO visualize the difference between running MDS on raw and standardized mtcars
cmdscale(dist(mtcars))

mds <- data.table(cmdscale(dist(mtcars)), keep.rownames = TRUE)
## creating new vars
mds[, scaled := FALSE]

cbind(scaled = FALSE, data.table(cmdscale(dist(mtcars)), keep.rownames = TRUE))

mds <- rbind(
    cbind(scaled = FALSE, data.table(cmdscale(dist(mtcars)), keep.rownames = TRUE)),
    cbind(scaled = TRUE, data.table(cmdscale(dist(scale(mtcars))), keep.rownames = TRUE))
)

ggplot(mds, aes(V1, V2)) +
    geom_point(size = 3) +
    transition_states(scaled) +
    labs(title = paste("Scaled: {closest_state}"))

## ohh ... pretty trivial
ggplot(mds, aes(V1, V2)) +
    geom_point(size = 3) +
    transition_states(scaled) +
    labs(title = paste("Scaled: {closest_state}")) +
    view_follow()

ggplot(mds, aes(V1, V2)) +
    geom_point(size = 3) +
    transition_states(scaled) +
    labs(title = paste("Scaled: {closest_state}")) +
    view_follow() +
    ease_aes('bounce-in-out')

## why did we even load the row names?
ggplot(mds, aes(V1, V2, label = rn)) +
    geom_text() +
    theme_bw() +
    transition_states(scaled) +
    labs(title = paste("Scaled: {closest_state}")) +
    view_follow() +
    ease_aes('exponential-in-out')

## doable, but probably not the best
library(ggrepel)
ggplot(mds, aes(V1, V2, label = rn)) +
    geom_text_repel() +
    theme_bw() +
    transition_states(scaled) +
    labs(title = paste("Scaled: {closest_state}")) +
    view_follow() +
    ease_aes('exponential-in-out')

## #############################################################################
## another example: Anscombe's quartett
## #############################################################################

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

ggplot(anscombe[, c(1, 5)], aes(x1, y1)) + geom_point() + geom_smooth(method = 'lm', se = TRUE)
ggplot(anscombe[, c(2, 6)], aes(x2, y2)) + geom_point() + geom_smooth(method = 'lm', se = TRUE)

## intro to loops
lapply(1:4, function(i) mean(anscombe[, c(i)]))
lapply(1:4, function(i) sd(anscombe[, c(i)]))
lapply(1:4, function(i) cor(anscombe[, c(i, i+4)]))

## loop to create separate & tidy dataset for each set
lapply(1:4, function(i) anscombe[, c(i, i+4)])

## use data.table to merge into one single data frame
library(data.table)
rbindlist(lapply(1:4, function(i) anscombe[, c(i, i+4)]))
## add a "set" column so that we know which set the row belongs to
rbindlist(lapply(1:4, function(i) cbind(set = i, anscombe[, c(i, i+4)])))

## save in a variable for future use
df <- rbindlist(lapply(1:4, function(i) cbind(set = i, anscombe[, c(i, i+4)])))
setnames(df, c('set', 'x', 'y'))

## let's switch from base R to ggplot and show all 4 sets in subplots
library(ggplot2)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~set)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~set) + geom_smooth()
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~set) + geom_smooth(method = 'lm')

## instead of 4 facets, let's make an animation .. transitioning between sets
library(gganimate)
ggplot(df, aes(x, y)) +
    geom_point() +
    transition_states(set)

## add linear trend line -- requires the transformr package if not installed
## as that can compute transition of lines
library(transformr)
ggplot(df, aes(x, y)) +
    geom_point() + geom_smooth(method = 'lm') +
    transition_states(set)

## add title recording the closest "set" + custom transitions between sets
ggplot(df, aes(x, y)) +
    geom_point() + geom_smooth(method = 'lm') +
    transition_states(set) +
    labs(title = "{closest_state}") +
    ease_aes('bounce-in-out')

## #############################################################################
## another example: the Datasaurus
## via https://www.meetup.com/rladies-budapest/events/266577128/
## #############################################################################

library(datasauRus)

datasaurus_dozen_wide
str(datasaurus_dozen_wide)

## transforming the wide table into tidy data
library(data.table)
rbindlist(lapply(1:13, function(i) cbind(
    set = sub('_y$', '', names(datasaurus_dozen_wide)[i*2]),
    setnames(
        datasaurus_dozen_wide[, c((i-1)*2 + 1:2)],
        c('x', 'y')))))

## or just be open-eyed
datasaurus_dozen

library(ggplot2)
ggplot(datasaurus_dozen, aes(x, y)) +
    geom_point() + facet_wrap(~dataset)

library(gganimate)
ggplot(datasaurus_dozen, aes(x, y)) +
    geom_point() + geom_smooth(method = 'lm') +
    transition_states(dataset)

subtitle <- function(df, digits = 4) {
    paste0(
        'mean(x)=', round(mean(df$x), digits), ', ', 'sd(x)=', round(sd(df$x), digits), '\n',
        'mean(y)=', round(mean(df$y), digits), ', ', 'sd(y)=', round(sd(df$y), digits), '\n',
        'cor(x,y)=', round(cor(df$x, df$y), digits)
    )
}
subtitle(datasaurus_dozen)

ggplot(datasaurus_dozen, aes(x, y)) +
    geom_point() + geom_smooth(method = 'lm') +
    transition_states(dataset) +
    labs(
        title = paste("{closest_state}"),
        subtitle = '{subtitle(subset(datasaurus_dozen, dataset == closest_state))}')

## #############################################################################
## creating new variables: numeric
## #############################################################################

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

## new variable via the data.frame way
bookings$price_per_night <- bookings$price / bookings$nnights
## new variable via data.table's in-place update operator
bookings[, price_per_night := price / nnights]

hotels <- merge(
    features,
    bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
    by = 'hotel_id')

## TODO compute average rating per number of stars
hotels[, mean(rating), by = stars]

hotels[!is.na(rating), mean(rating), by = stars]
hotels[, mean(rating, na.rm = TRUE), by = stars]

hotels[, mean(rating, na.rm = TRUE), by = stars][order(stars)]
hotels[!is.na(stars), mean(rating, na.rm = TRUE), by = stars][order(stars)]
hotels[!is.na(stars), .(rating = mean(rating, na.rm = TRUE)), by = stars][order(stars)]

hotels[!is.na(stars), .(rating = mean(rating, na.rm = TRUE), .N), by = stars][order(stars)]
hotels[!is.na(stars), .(
    rating = mean(rating, na.rm = TRUE),
    .N,
    bookings = sum(bookings)), by = stars][order(stars)]

## maybe weight by the number of bookings?
hotels[!is.na(stars), .(rating = weighted.mean(rating, bookings, na.rm = TRUE)), by = stars][order(stars)]

## TODO list countries above average rating
countries <- hotels[, .(
    price = mean(price_per_night),
    rating = mean(rating, na.rm = TRUE),
    stars = mean(stars, na.rm = TRUE)
), by = country]
countries[country == 'Hungary']

mean(countries$rating, na.rm = TRUE)
countries[rating > mean(rating, na.rm = TRUE)]

## #############################################################################
## merging data (from online sources)
## #############################################################################

## plot the rating VS price per night
library(ggplot2)
ggplot(countries, aes(rating, price_per_night)) + geom_point()
## add number of hotels
ggplot(countries, aes(rating, price_per_night, size = hotels)) + geom_point()
## add actualy country name
ggplot(countries, aes(rating, price_per_night, label = country)) + geom_text()
ggplot(countries, aes(rating, price_per_night, label = country, size = hotels)) + geom_text()

## similar results? how to group countries? maybe geo?
library(ggmap)

geocode('CEU, Budapest')
?register_google

geocode('CEU, Budapest', source = 'dsk')

## NOTE you might need to install and older version of the package, see the below for details:
## https://github.com/dkahle/ggmap/commit/025f98af996c56cd419830073111347b2d1971f3
## https://cran.r-project.org/src/contrib/Archive/ggmap/
## install.packages('https://cran.r-project.org/src/contrib/Archive/ggmap/ggmap_2.6.2.tar.gz')
## install.packages('ggmap', repos = 'https://cran.microsoft.com/snapshot/2019-02-01/')

## unload and then reload a package (or restart R)
detach('package:ggmap', unload = TRUE)
library(ggmap)

## paste to chrome ... reverse lat lot ... Budapest?
geocode('Budapest, Nador utca 9', source = 'dsk', )
geocode('Budapest, Nador utca 9', source = 'dsk', output = 'all')
## Google is much smarter to find eg CEU + lot of extra metadata ... but not free

geocode(countries[1, country], source = 'dsk')

for (i in 1:nrow(countries)) {
    geocode <- geocode(countries[i, country], source = 'dsk')
    countries[i, lon := geocode$lon]
    countries[i, lat := geocode$lat]
}

## TODO while it's running, copy and paste a URL into the browser
## http://www.datasciencetoolkit.org/maps/api/geocode/json?address=Hungary&sensor=false

countries

## plot the location of the countries
ggplot(countries, aes(lon, lat, label = country)) + geom_text()

## plot the avg rating of the countries
ggplot(countries, aes(lon, lat, label = country, color = rating)) + geom_text()
ggplot(countries, aes(lon, lat, size = rating)) + geom_point()

## plot the prices per night in the countries
ggplot(countries, aes(lon, lat, size = price_per_night)) + geom_point()

## plot over map
worldmap <- map_data('world')
str(worldmap)
ggplot() +
    geom_polygon(data = worldmap, aes(x = long, y = lat, group = group)) +
    geom_point(data = countries, aes(lon, lat, size = price_per_night), color = 'orange') +
    coord_fixed(1.3)

## plot over Europe
europe <- get_map(location = 'Berlin', zoom = 4, maptype = 'terrain')
europe <- get_map(location = 'Berlin', zoom = 4, maptype = 'terrain', api_key = key)
## argh

center <- countries[country == 'Germany', c(lon = lon, lat = lat)]
center <- c(left = -12, bottom = 35, right = 30, top = 63)

europe <- get_map(location = center, zoom = 4, source = 'stamen', maptype = 'toner')
europe <- get_map(location = center, zoom = 4, source = 'stamen', maptype = 'watercolor')
## http://maps.stamen.com

ggmap(europe) +
    geom_point(data = countries, aes(lon, lat, size = price_per_night), color = 'orange')

## TODO add further data ... eg why some countries might be similar besides geo?
library(XML) # or rvest
## https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita
gdp <- readHTMLTable(readLines('http://bit.ly/CEU-R-gdp'), which = 3, header = TRUE)
head(gdp)

gdp <- data.table(gdp)

gdp[, country := iconv(`Country/Territory\n`, to = 'ASCII', sub = '')]
gdp[, gdp := as.numeric(sub(',', '', `Int$\n`))]

countries$country %in% gdp$country
## \o/

countries <- merge(countries, gdp[, .(country, gdp)], by = 'country')

ggplot(countries, aes(gdp, price_per_night)) + geom_point()
ggplot(countries, aes(gdp, price_per_night)) + geom_point() + geom_smooth(method = 'lm')
