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
suggestions <- NbClust(iris[, 1:4], method = 'complete', index = 'all')

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species)) + geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3)
ggplot(iris, aes(Sepal.Length, Sepal.Width, shape = Species, color = factor(clusters))) +
    geom_point(size = 3) + theme_bw() + theme(legend.position = 'top')
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
        title = paste("{closest_state}"), # Python's f-string, R's glue
        subtitle = 'Number of flowers: {nrow(subset(iris, Species == closest_state))}')

## see more fancy plot at https://twitter.com/daroczig/status/1201542038640111616

## #############################################################################
## another example: the Datasaurus
## via https://www.meetup.com/rladies-budapest/events/266577128/
## #############################################################################

## NOTE refer back to Anscombe
library(data.table)
df <- rbindlist(lapply(1:4, function(i) cbind(dataset = i, anscombe[, c(i, i+4)])))
setnames(df, c('dataset', 'x', 'y'))
library(ggplot2)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset) + geom_smooth(method = 'lm')


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
## multiple summaries with data.table

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

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
    price_per_night * 360,
    price_per_night * 1.18)]

hotels[, `:=` (price_per_night_huf = price_per_night * 360,
               price_per_night_usd = price_per_night * 1.18)]

## https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

## ############################################################################
## tweaking ggplot2 themes
## ############################################################################

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
bookings[, price_per_night := price / nnights]
hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

## TODO create a new variable on how popular a hotel is in the city
hotels[, bookings_in_city := .N, by = city]
hotels[, popularity := bookings / bookings_in_city]
p <- ggplot(hotels, aes(x = rating, y = popularity)) + geom_point(alpha = 0.2)

hotels[, popularity := cut(bookings, c(0, 3, 7, Inf))]
(p <- ggplot(hotels, aes(x = rating, fill = popularity)) + geom_density(alpha = 0.2))

(p <- ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point())

## themes
library(ggthemes)
p + theme_economist() + scale_fill_economist()
p + theme_stata() + scale_fill_stata()
p + theme_excel() + scale_fill_excel()

ggplot(mtcars, aes(wt, hp, color = factor(am))) + geom_point() + theme_excel() + scale_color_excel()

p + theme_wsj() + scale_fill_wsj('colors6', '')
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
            size = 2)
    )
}
p + theme_custom()
?theme

library(ggthemr)
?ggthemr
## https://github.com/cttobin/ggthemr

ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')
p

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
            panel.grid.minor = function(...) element_line(size = 0.25, ...)
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
ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point()

p <- last_plot()

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
  data_id = factor(gear),
  ## paste('Number of gears:', gear))
  tooltip = rownames(mtcars))) +
  geom_point_interactive()
girafe(ggobj = p)
girafe(ggobj = p, options = list(opts_hover(css = "fill:black;")))

girafe(ggobj = p, options = list(
  opts_hover(css = "fill:black;"),
  opts_zoom(max = 2)
))

## Rx Studio demo?

## #############################################################################
## PCA demo on image processing

## Blog post: http://bit.ly/r-intro-pca-blog
## http://www.nasa.gov/images/content/694811main_pia16225-43_full.jpg
download.file('http://bit.ly/r-intro-nasa', 'image.jpg', method = 'curl', extra = '-L')
download.file('http://bit.ly/r-intro-nasa', 'image.jpg', mode = 'wb')
## look at the file

## load image file
library(jpeg)
img <- readJPEG('image.jpg')

## it's a 3D array
str(img)

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

## rotation matrix
pca$rotation
eigen(cov(img1d))$vectors

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
