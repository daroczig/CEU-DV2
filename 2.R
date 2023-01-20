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

## Compute the `mean` and `sd` of all numeric variables grouped by `District`, something like:

numcols <- which(sapply(df, is.numeric))
mystats <- function(x) list(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE))
districts <- df[, as.list(unlist(lapply(.SD, mystats))), .SDcols = numcols, by = District]

## Then apply MDS on this dataset and visualize the similarities of the Budapest districts:

mds <- cmdscale(dist(scale(districts)))
mds <- data.frame(mds)
ggplot(mds, aes(X1, X2, label = districts$District)) + geom_label() +
    theme_bw() + theme_void() +
    ggtitle('Budapest districts (note that I am centered)') +
    theme(plot.title = element_text(hjust = 0.5))

## Geocode the 23 districts of Budapest (like we did in the class this week), and show them on a map:

library(ggmap)
library(tidygeocoder)
library(scatterpie)

districts <- df[, .N, by = District]
## test on https://nominatim.openstreetmap.org/ui/search.html
## works: district VIII, budapest, hungary
districts[, address := paste0('District ', as.roman(District), ', Budapest, Hungary')]
districts <- data.table(geocode(districts, 'address'))

## get map background
bp <- get_stamenmap(
    c(
        left = min(districts$long) * 0.995,
        right = max(districts$long) * 1.001,
        bottom = min(districts$lat) * 0.999,
        top = max(districts$lat)) * 1.001,
    maptype = 'toner-2011',
    zoom = 12)

ggmap(bp) +
    geom_point(data = districts, aes(long, lat, size = N), shape = 19, color = 'orange') +
    theme_void() + theme(legend.position = 'none')

## Now use the location data from above, but instead of points, place small pie-charts (!) using the `scatterpie` package on the map to show the distribution of comfort level for each district. You might need to pivot your long table into a wide one using `dcast`. Don't forget to order the labels of `Comfort_lev` first:

?geom_scatterpie

districts <- df[!is.na(Comfort_lev), .N, by = .(District, Comfort_lev)]
comfort_lev_categories <- c('very low', 'low', 'average', 'high', 'very high', 'luxury')
districts[, Comfort_lev := factor(Comfort_lev, levels = comfort_lev_categories)]
districts <- dcast(districts, District ~ Comfort_lev)
## test on https://nominatim.openstreetmap.org/ui/search.html
districts[, address := paste0('District ', as.roman(District), ', Budapest, Hungary')]
districts <- geocode(districts, 'address')
districts[is.na(districts)] <- 0

ggmap(bp) +
    geom_scatterpie(
        data = districts,
        aes(long, lat, group = District, r = 0.012),
        cols = comfort_lev_categories, color = NA, alpha = .8) +
    theme_void() + theme(legend.position = 'top') +
    guides(fill = guide_legend(title = 'Comfort level', nrow = 1))

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
library(rgdal) # R -> Geospatial Data Abstraction Library
ogrInfo('.')

## Read a shapefile from the current working directory (refer to the filename without file extension)
adm0 <- readOGR('.', 'AUT_adm0')
str(adm0)

## Convert to data.frame so that we can pass to ggplot2 soon
adm0 <- fortify(adm0)

## Now let's load smaller administrative areas as well
adm2 <- fortify(readOGR('.', 'AUT_adm2'))

## And some points to be added to the map as well
cities <- fread('https://simplemaps.com/static/data/country-cities/at/at.csv')

## Party time
library(ggplot2)
ggplot() +
    geom_path(data = adm0,
              aes(x = long, y = lat, group = group),
              color = 'gray', size = 1) +
    geom_path(data = adm2,
              aes(x = long, y = lat, group = group),
              color = 'gray', size = .2) +
    geom_point(data = cities,
               aes(lng, lat, size = population),
               color = 'orange') +
    theme_void() +
    theme(legend.position = 'top') +
    coord_fixed(1.3)

## how to access the original data elements?
adm2 <- readOGR('.', 'AUT_adm2')

str(adm2@data)
adm2@data$NAME_2

adm2df <- fortify(adm2)
str(merge(adm2df, data.table(adm2@data)[, .(id = ID_2, NAME_2)], by = 'id'))

## using geojson files
## see e.g. https://github.com/ginseng666/GeoJSON-TopoJSON-Austria/blob/master/2021/simplified-95/bezirke_95_topo.json

download.file(
    'https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2021/simplified-95/bezirke_95_topo.json',
    'austria.geojson')

library(rgdal)
library(leaflet)

map <- readOGR('austria.geojson')
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
    geom_polygon(data = map, aes( x = long, y = lat, group = group), fill='#69b3a2', color='white') +
    theme_void() +
    coord_map()

## #############################################################################
## creating new variables: numeric to factor
## #############################################################################

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

## TODO add a new column to hotels: categorize price into 3 buckets
?cut
hotels[, pricecat := cut(price_per_night, 3, dig.lab = 8)]
str(hotels)
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_per_night, c(0, 100, 250, Inf))]
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_per_night, c(0, 100, 250, Inf),
                         labels = c('cheap', 'average', 'expensive'))]
hotels[, .N, by = pricecat]
hotels[, .N, by = pricecat][order(pricecat)]


## TODO use a stats approach to categorize hotels into below avg, avg, above avg price groups
price_mean <- mean(hotels$price_per_night)
price_sd <- sd(hotels$price_per_night)

## NOTE below avg: 0 -> mean - sd
## NOTE avg: mean - sd -> mean + sd
## NOTE above avg: mean + sd

hotels[, pricecat := cut(price_per_night, c(
  0,
  price_mean - price_sd,
  price_mean + price_sd,
  Inf),
  labels = c('below avg', 'avg', 'above avg'))]
hotels[, .N, by = pricecat]
## NOTE skewed distribution & very different by country

## TODO avg and sd by country
hotels[, avg_price_per_country := mean(price_per_night), by = country]
hotels[, sd_price_per_country := sd(price_per_night), by = country]
str(hotels)

hotels[, pricecat := cut(price_per_night, c(
  0,
  avg_price_per_country[1] - sd_price_per_country[1], # NOTE skip [1] first
  avg_price_per_country[1] + sd_price_per_country[1],
  Inf),
  labels = c('below avg', 'avg', 'above avg')),
  by = country]
## OH NOOO!!

hotels[, .N, by = pricecat]

## NOTE the above not working due to
hotels[, hotels_per_country := .N, by = country]
hotels[hotels_per_country == 1]
hotels <- hotels[hotels_per_country > 1]

## #############################################################################
## multiple summaries with data.table
## #############################################################################

## reminder
hotels[, .(price_avg = mean(price_per_night)), by = city]

## TODO compute the average price, rating and stars per city in a new dataset
hotels[, .(price_avg = mean(price_per_night),
           rating_avg = mean(rating),
           stars_avg = mean(stars)), by = city]

## NOTE we need to remove NAs
hotels[, .(price_avg = mean(price_per_night),
           rating_avg = mean(rating, na.rm = TRUE),
           stars_avg = mean(stars, na.rm = TRUE)), by = city]

## TODO check the same on distance and the number of bookings as well ...
hotels[, lapply(.SD, mean), by = city,
       .SDcols = c('price_per_night', 'rating', 'stars', 'distance', 'bookings')]

hotels[, lapply(.SD, mean, na.rm = TRUE), by = city,
       .SDcols = c('price_per_night', 'rating', 'stars', 'distance', 'bookings')]

numcols <- c('price_per_night', 'rating', 'stars', 'distance', 'bookings')
hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, .SDcols = numcols]

sapply(hotels, is.numeric)
numcols <- which(sapply(hotels, is.numeric))
hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, .SDcols = numcols]

## #############################################################################
## extra examples on multiple summaries
## #############################################################################

## TODO min, avg, median and max price in EUR per city
hotels[, list(
    min_price = min(price_per_night),
    price_per_night = mean(price_per_night),
    med_price = median(price_per_night),
    max_price = max(price_per_night)
), by = city]

## TODO round it up to EUR
hotels[, list(
    min_price = round(min(price_per_night)),
    price_per_night = round(mean(price_per_night)),
    med_price = round(median(price_per_night)),
    max_price = round(max(price_per_night))
), by = city]

mystats <- function(x) list(
    min = min(x),
    mean = mean(x),
    median = median(x),
    max = max(x))

hotels[, lapply(.SD, mystats), .SDcols = c('price_per_night')]
hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols = c('price_per_night')]

mystats <- function(x) list(
    min = round(min(x, na.rm = TRUE), 2),
    mean = round(mean(x, na.rm = TRUE), 2),
    median = round(median(x, na.rm = TRUE), 2),
    max = round(max(x, na.rm = TRUE), 2))
hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols = c('price_per_night', 'rating')]
hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols = which(sapply(hotels, is.numeric))]
summary(hotels)

hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols = which(sapply(hotels, is.numeric)), by = country]

## add multiple columns at once
hotels[, c('price_per_night_huf', 'price_per_night_usd') := list(
    price_per_night * 300,
    price_per_night * 1.1)]

hotels[, `:=` (price_per_night_huf = price_per_night * 300,
               price_per_night_usd = price_per_night * 1.1)]

## https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

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

lapply(df, summary)
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
ggplot(df, aes(x, y)) + geom_hex(binwidth = 5)

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

## https://bit.ly/iris-species
## see more fancy plot at https://twitter.com/daroczig/status/1201542038640111616

## ############################################################################
## back to datasaurus!
## ############################################################################

library(datasauRus)
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

## ############################################################################
## tweaking ggplot2 themes
## ############################################################################

## plot weight and acceleration of cars on a scatterplot
## colored by transmission
ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point()
p <- last_plot()

## themes
library(ggthemes)
p + theme_economist() + scale_fill_economist()
p + theme_stata() + scale_fill_stata()
p + theme_excel() + scale_fill_excel()
p + theme_gdocs() + scale_fill_gdocs()

## create a custom theme for future usage
?theme
theme_custom <- function() {
    theme(
        axis.text = element_text(
            family = 'Times New Roman',
            color  = "orange",
            size   = 12),
        axis.title = element_text(
            family = 'Times New Roman',
            color  = "orange",
            size   = 16,
            face   = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        panel.background = element_rect(
            fill = "orange",
            color = "white",
            linewidth = 2)
    )
}
p + theme_custom()
?theme

library(ggthemr)
?ggthemr
## https://github.com/cttobin/ggthemr

ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')
p

## old-school palette-definition without helper functions
ggthemr(
    palette = structure(
        list(
            background = 'papayawhip',
            text = c(inner = 'orange', outer = 'black'),
            line = c(inner = 'orange', outer = 'black'),
            gridline = 'white',
            swatch = structure(RColorBrewer::brewer.pal(8, 'Dark2'),
                class = 'ggthemr_swatch'),
            gradient = c(low = 'white', high = 'red')),
        class = 'ggthemr_palette'),
    layout = structure(
        list(
            panel.grid.major = function(...) element_line(...),
            panel.grid.minor = function(...) element_line(linewidth = 0.25, ...)
        ), class = 'ggthemr_layout'),
    text_size = 12, spacing = 0.5)
p

## drop back to the default colors and layout
ggthemr_reset()

## ############################################################################
## interactive plots
## ############################################################################

## plot weight and acceleration of cars on a scatterplot
## colored by transmission
(p <- ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point())

library(ggiraph)
?girafe
girafe(ggobj = p)
## not much change, only an extra export feature?
## although can highlight, also shows up in the Viewer (where the anims went as well)

p <- ggplot(mtcars, aes(
  x = wt,
  y = qsec,
  color = factor(am),
  ## paste('Number of gears:', gear))
  tooltip = rownames(mtcars))) +
  geom_point_interactive()
girafe(ggobj = p)

p <- ggplot(mtcars, aes(
  x = wt,
  y = qsec,
  color = factor(am),
  ## NOTE this newly added field
  data_id = factor(gear),
  tooltip = rownames(mtcars))) +
  geom_point_interactive()
girafe(ggobj = p)
girafe(ggobj = p, options = list(opts_hover(css = "fill:black;")))

girafe(ggobj = p, options = list(
  opts_hover(css = "fill:black;"),
  opts_zoom(max = 2)
))

## #############################################################################
## PCA demo on image processing
## #############################################################################

## https://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues

## http://www.nasa.gov/images/content/694811main_pia16225-43_full.jpg
download.file('http://bit.ly/r-intro-nasa', 'image.jpg', method = 'curl', extra = '-L')
download.file('http://bit.ly/r-intro-nasa', 'image.jpg', mode = 'wb')
## look at the file

## load image file
library(jpeg)
img <- readJPEG('image.jpg')

## it's a 3D array
str(img)

## check a pixel
img[1, 1, ]
col <- do.call(rgb, as.list(img[1, 1, ]))
col

barplot(1, col = col)

## record image height and width
dim(img)
h <- dim(img)[1]
w <- dim(img)[2]

## transform the 3D array into a 2D table with h*w rows and 3 columns for the RGB values
img1d <- matrix(img, h * w)
str(img1d)

## run principal component analysis
pca <- prcomp(img1d)
pca
summary(pca)

## rotation matrix (the eigenvectors of the covariance matrix)
pca$rotation
eigen(cov(img1d))$vectors

## how to get the scores?
str(pca$x)
str(sweep(img1d, 2, colMeans(img1d)) %*% pca$rotation)

cor(pca$x[, 1], img1d)
cor(pca$x, img1d)

## visualize the correlation coefficients as colors
extractColors <- function(x) rgb(x[1], x[2], x[3])
(colors <- apply(abs(pca$rotation), 2, extractColors))
pie(pca$sdev, col = colors, labels = colors)

?pie
pie(c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5),
    init.angle = 315, col = c("deepskyblue", "yellow", "yellow3"), border = FALSE)

## redraw the original image with artificial colors based on the components
image(matrix(pca$x[, 1], h))

image(matrix(pca$x[, 2], h))
image(matrix(pca$x[, 2], h), col = gray.colors(100))

image(matrix(pca$x[, 3], h))

## Blog post: http://bit.ly/r-intro-pca-blog
## More on PCA: https://stats.stackexchange.com/a/140579
