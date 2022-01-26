## #############################################################################
## homework solutions
## https://rpubs.com/daroczig-ceu/dv2-h2
## #############################################################################

## Geocode the (center of the) 23 districts in Budapest (like we did
## in the class this week), and show the distribution of comfort level
## for each district on a map. Yeah, using pie charts via
## `scatterpie`! You can use `get_stamenmap` from the `ggmap` package
## to grab the background raster image:

## NOTE typo in exercise .. this is just a scatterplot

library(data.table)
df <- data.table(readRDS('~/downloads/flats.rds'))

library(ggplot2)
library(ggmap)
library(tidygeocoder)

districts <- df[, .N, by = District]
## test on https://nominatim.openstreetmap.org/ui/search.html
## e.g. "District 11, Budapest, Hungary" vs "District XI, Budapest, Hungary"
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


## Now use the location data from above, but instead of points, place
## small pie-charts (!) on the map to show the distribution of comfort
## level for each district. You might need to pivot your long table
## into a wide one using `dcast`. Don't forget to order the labels of
## `Comfort_lev` first:

districts <- df[!is.na(Comfort_lev), .N, by = .(District, Comfort_lev)]
comfort_lev_categories <- c('very low', 'low', 'average', 'high', 'very high', 'luxury')
districts[, Comfort_lev := factor(Comfort_lev, levels = comfort_lev_categories)]
districts <- dcast(districts, District ~ Comfort_lev) # tidyr::spread works too
## fix missing values
districts[is.na(districts)] <- 0
## test on https://nominatim.openstreetmap.org/ui/search.html
districts[, address := paste0('District ', as.roman(District), ', Budapest, Hungary')]
districts <- geocode(districts, 'address')

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

download.file(
    'https://biogeo.ucdavis.edu/data/diva/adm/HUN_adm.zip',
    'Hungary_shapefile.zip')
unzip('Hungary_shapefile.zip')

## Look around what we have
library(rgdal)
ogrInfo('.')

## Read a shapefile from the current working directory (refer to the filename without file extension)
adm1 <- readOGR('.', 'HUN_adm1')

## Convert to data.frame so that we can pass to ggplot2 soon
adm1 <- fortify(adm1)

## Now let's load smaller administrative areas as well
adm2 <- fortify(readOGR('.', 'HUN_adm2'))

## And some points to be added to the map as well
cities <- fread('https://simplemaps.com/static/data/country-cities/hu/hu.csv')

## Party time
library(ggplot2)
ggplot() +
    geom_path(data = adm1,
              aes(x = long, y = lat, group = group),
              color = 'gray', size = 1) +
    geom_path(data = adm2,
              aes(x = long, y = lat, group = group),
              color = 'gray', size = .2) +
    geom_point(data = cities,
               aes(lng, lat, size = population)) +
    theme_void() +
    theme(legend.position = 'top')

## #############################################################################
## another example: the Datasaurus via
## https://www.meetup.com/rladies-budapest/events/266577128/
## #############################################################################

## NOTE refer back to Anscombe
library(data.table)
df <- rbindlist(lapply(1:4, function(i) cbind(dataset = i, anscombe[, c(i, i+4)])))
setnames(df, c('dataset', 'x', 'y'))
library(ggplot2)
ggplot(df, aes(x, y)) + geom_point() + facet_wrap(~dataset) + geom_smooth(method = 'lm')

## fix the warning
df <- rbindlist(lapply(1:4, function(i) cbind(dataset = i, setnames(anscombe[, c(i, i+4)], c('x', 'y')))))

library(datasauRus)
datasaurus_dozen_wide
str(datasaurus_dozen_wide)

## transforming the wide table into tidy data
library(data.table)
rbindlist(lapply(1:13, function(i) cbind(
    dataset = sub('_y$', '', names(datasaurus_dozen_wide)[i*2]),
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
    price_per_night * 360,
    price_per_night * 1.18)]

hotels[, `:=` (price_per_night_huf = price_per_night * 360,
               price_per_night_usd = price_per_night * 1.18)]

## https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

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
    palette = define_palette(
        background = 'papayawhip',
        text = c('orange', 'black'),
        line = c('orange', 'black'),
        gridline = 'white',
        swatch = RColorBrewer::brewer.pal(8, 'Dark2'),
        gradient = c(lower = 'white', upper = 'red')),
    layout = structure(
        list(
            panel.grid.major = function(...) element_line(...),
            panel.grid.minor = function(...) element_line(size = 0.25, ...)
        ), class = 'ggthemr_layout'),
    text_size = 12, spacing = 0.5)
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
            panel.grid.minor = function(...) element_line(size = 0.25, ...)
        ), class = 'ggthemr_layout'),
    text_size = 12, spacing = 0.5)

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

## #############################################################################
## PCA demo on image processing
## #############################################################################

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


