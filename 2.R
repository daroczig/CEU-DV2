## #############################################################################
## homework format
## #############################################################################

## File > New > R markdown
##
## 1. Fill in article metadata
## 2. Paste exercise (markdown)
## 3. Write up R code chunks solving the exercise
## 4. Kint to e.g. PDF

## #############################################################################
## homework solutions
## #############################################################################

## 7. Plot the airports as per their geolocation on a world map, by mapping the number flights going to that destionation to the size of the symbol!

library(ggplot2)
library(nycflights13)
library(data.table)

?airports
?flights

worldmap <- map_data('world')

ggplot() +
  geom_polygon(
    data = worldmap,
    aes(x = long, y = lat, group = group),
    fill = 'gray', color = 'black') +
  geom_point(
    data = merge(
      airports,
      data.table(flights)[, .(flights = .N), by = .(faa = dest)],
      by = 'faa'),
    aes(lon, lat, size = flights),
    color = 'orange') +
  theme_bw() +
  theme(legend.position = 'top') +
  xlab('') + ylab('') +
  coord_fixed(1.3)

## BTW when you need to map things without known lat/long
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

library(tidygeocoder)
str(hotels)

hotels[, address := paste(center1label, city_actual, country, sep = ', ')]

geo(address = hotels$address[1], method = "osm", verbose = TRUE)

## batch geocoding?
hotels[, .N, by = .(country, city_actual, center1label)]
geocodes <- geocode(hotels, 'address')
## NOTE the rate limits as per docs

## batch geocoding?
hotels[, .N, by = .(country, city_actual, center1label)]
geocodes <- geocode(hotels, 'address')
## NOTE the rate limits as per docs
## saveRDS(geocodes, 'geocodes.RDS')

geocodes <- readRDS('geocodes.RDS')
geocodes <- data.table(geocodes)

ggplot() +
  geom_polygon(
    data = worldmap,
    aes(x = long, y = lat, group = group),
    fill = 'gray', color = 'black') +
  geom_point(
    data = geocodes[, .N, by = .(lat, long)],
    aes(long, lat, size = N),
    color = 'orange') +
  theme_bw() +
  theme(legend.position = 'top') +
  xlab('') + ylab('') +
  coord_fixed(1.3)

## #############################################################################
## EDA warmup - alternatives to boxplot
## #############################################################################

## df <- rbind(
##     data.table(x = 1, y = rbeta(1e3, 0.1, 0.1)),
##     data.table(x = 2, y = rnorm(1e3, 0.5, 0.75)),
##     data.table(x = 3, y = runif(1e3) * 2 - 0.5),
##     data.table(x = 4, y = rnorm(1e3, 0.5, 0.75)))

library(data.table)
df <- fread('http://bit.ly/CEU-R-numbers-set')

summary(df)
lapply(df, summary)
lapply(unique(df$x), function(set) summary(df[x == set]))
df[, as.list(summary(y)), by = x]

library(ggplot2)
ggplot(df,aes(x, y)) + geom_point()
ggplot(df,aes(x, y)) + geom_point(alpha = .25)

ggplot(df,aes(x, y)) + geom_hex()

ggplot(df,aes(x, y)) + geom_boxplot()
ggplot(df,aes(factor(x), y)) + geom_boxplot()

ggplot(df,aes(factor(x), y)) + geom_violin()
ggplot(df,aes(factor(x), y)) + geom_violin() + geom_jitter()
ggplot(df,aes(factor(x), y)) + geom_violin() + geom_jitter(height = 0, width = 0.1)

ggplot(df,aes(y)) + geom_histogram() + facet_wrap(~x)

ggplot(df,aes(y, fill = x)) + geom_density()
ggplot(df,aes(y, fill = factor(x))) + geom_density()
ggplot(df,aes(y, fill = factor(x))) + geom_density(alpha = 0.25) + theme(legend.position = 'top')

## let's do a heatmap on the mean and standard deviation of the sets!
ggplot(melt(df[, .(mean = mean(y),sd = sd(y)), by = x], id = 'x'),
       aes(x, variable, fill = value)) + geom_tile()

## TODO do similar exploratory data analysis on the below dataset
df <- read.csv('http://bit.ly/CEU-R-numbers')
## generated at https://gist.github.com/daroczig/23d1323652a70c03b27cfaa6b080aa3c

ggplot(df, aes(x, y)) + geom_point()
ggplot(df, aes(x, y)) + geom_point(alpha = 0.05)
ggplot(df, aes(x, y)) + geom_point(size = 0.2, alpha = 0.1)
ggplot(df, aes(x, y)) + geom_hex(binwidth = 25)

## TODO find interesting pattern in data?

## #############################################################################
## why the dataviz? edu purposes, eg showing how clustering works
## #############################################################################

## hierarchical clustering
?hclust

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
ggplot(d, labels = FALSE)
ggplot(d, labels = FALSE, horiz = TRUE)
ggplot(d, labels = FALSE) + scale_y_reverse(expand = c(0.2, 0)) + coord_polar(theta="x")
## https://talgalili.github.io/dendextend/articles/dendextend.html

for (i in 2:8) {
    ## NOTE the need to print
    print(ggplot(color_branches(as.dendrogram(hc), k = i)))
    Sys.sleep(1)
}

saveGIF({
    for (i in 2:8) {
        print(ggplot(color_branches(as.dendrogram(hc), k = i)))
    }
}, ani.width = 960)

## cluster membership
clusters <- cutree(hc, 3)
clusters

## why 3?
library(NbClust)
NbClust(iris[, 1:4], method = 'complete', index = 'all')
## QQ elbow rule: diminishing returns are no longer worth the additional cost
suggestions <- NbClust(iris[, 1:4], method = 'complete', index = 'all')
str(suggestions)

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top')

## add linear models by both species and cluster membership?
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top') +
    geom_smooth(method = 'lm')
## hm, that's difficult to interpret .. let's differentiate line types as well?
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters), linetype = Species)) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top') +
    geom_smooth(method = 'lm')
## still not happy

## loop over Species? instead of faceting, our new dimension for visualization will be time
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
        title = paste("{closest_state}"), # Python's f-string, R's glue
        subtitle = 'Number of flowers: {nrow(subset(iris, Species == closest_state))}')

## see more fancy plot at https://twitter.com/daroczig/status/1201542038640111616
