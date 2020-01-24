## #############################################################################
## homework solutions
## #############################################################################

## 7. Plot the airports as per their geolocation on a world map, by mapping the number flights going to that destionation to the size of the symbol!

library(ggplot2)
library(nycflights13)
library(data.table)

## NO need to geocode the airports!
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
## warmup - alternatives to boxplot
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

ggplot(df,aes(x, y)) + geom_point()
ggplot(df,aes(x, y)) + geom_point(alpha = .25)
ggplot(df,aes(x, y)) + geom_point(alpha = .25) + geom_smooth(method = 'lm')

ggplot(df,aes(x, y)) + geom_jitter()

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
## creating new variables (rather than at 5pm): numeric to factor
## #############################################################################

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

hotels <- merge(
    features,
    bookings[, .(bookings = .N, price_per_night = mean(price / nnights)), by = hotel_id],
    by = 'hotel_id')

## TODO add a new column to hotels: categorize price into 3 buckets
?ggplot2::cut_width # familiar from DA1? check the sources ...
?cut
hotels[, pricecat := cut(price_per_night, 3, dig.lab = 8)]
str(hotels)
hotels[, .N, by = pricecat]

hotels[, pricecat := cut(price_per_night, breaks = c(0, 100, 250, Inf))]
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

## TODO compute avg and sd by country and then use these breakpoints
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

## rerun the above aggr

## TODO create a new variable on city type: small / big <- number of hotels in the city
hotels[, hotels_per_city := .N, by = city_actual]
hotels[, citytype := cut(hotels_per_city, 2, labels = c('small', 'big'))]
hotels[, .N, by = citytype]

## TODO number of cases by citytype + pricecat
hotels[, .N, by = .(pricecat, citytype)][order(pricecat, citytype)]
hotels[, .N, by = .(pricecat, citytype)][order(-N)]

## TODO visualize the percentage of hotels in big/small cities by country
ggplot(hotels, aes(country, fill = citytype)) + geom_bar()
ggplot(hotels, aes(country, fill = citytype)) + geom_bar(position = 'fill')
ggplot(hotels, aes(country, fill = citytype)) + geom_bar(position = 'fill') +
    ylab('') + xlab('') +
    scale_y_continuous(labels = scales::percent) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = 'top')

## fun fact
hotels[citytype == 'big', .N, by = country]

## TODO compute (instead of vosualize) the percentages of price categories within city type
price_per_type <- hotels[, .N, by = list(pricecat, citytype)][order(citytype, pricecat)]
price_per_type[, `%` := N / sum(N), by = citytype]
price_per_type[, `%` := round(N / sum(N) * 100, 2), by = citytype]
price_per_type

## TODO exercise visualize the percentages of price categories within city type
ggplot(price_per_type, aes(citytype, `%`, fill = pricecat)) + geom_col()
ggplot(price_per_type, aes(citytype, N, fill = pricecat)) + geom_col(position = 'fill')

## why is it still better to do the custom computation?
## TODO add actual number of hotels to the plot
ggplot(price_per_type, aes(citytype, `%`, fill = pricecat)) +
    geom_col() +
    geom_text(aes(label = N))
## ouch -- need to compute cumsum of percentages?

price_per_type[, CP := cumsum(`%`), by = citytype]
ggplot(price_per_type, aes(citytype, fill = pricecat)) +
    geom_col(aes(y = `%`)) +
    geom_text(aes(y = CP, label = N))

## still wrong order?
setorder(price_per_type, citytype, -pricecat)
price_per_type[, CP := cumsum(`%`), by = citytype]

ggplot(price_per_type, aes(citytype, fill = pricecat)) +
    geom_col(aes(y = `%`)) +
    geom_text(aes(y = CP, label = N))

## drop low values and tweak design a bit
price_per_type[N < 100, N := NA]
ggplot(price_per_type, aes(citytype, fill = pricecat)) +
    geom_col(aes(y = `%`)) +
    geom_text(aes(y = CP, label = N), vjust = 1.5, color = 'white') +
    theme_bw() +
    theme(legend.position = 'top') +
    xlab('') + ylab('') +
    scale_y_continuous(labels = scales::percent)
## now it's a good time to get rid of that lame percentage calculator rounding to 2 digits

## #############################################################################
## FOR FUTURE REFERENCE: creating new variables: numeric to numeric
## #############################################################################

## TODO create a new variable with the count of hotels in the same country
hotels[, hotels_in_country := .N, by = country]

## TODO create a new variable with the count of cities in the same country
hotels[, cities_in_country := length(unique(city_actual)), by = country]

## #############################################################################
## multiple summaries
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
is.logical
numcols <- which(sapply(hotels, is.numeric))
hotels[, lapply(.SD, mean, na.rm = TRUE), by = .(country,citytype), .SDcols = numcols]

## #############################################################################
## FOR FUTURE REFERENCE: extra examples on multiple summaries
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
str(hotels[, as.list(unlist(lapply(.SD, mystats))), .SDcols = which(sapply(hotels, is.numeric)), by = country])

## ###############################################################################################
## tweaking ggplot2 themes
## ###############################################################################################

## TODO create a new variable on how popular a hotel is in the city
hotels[, bookings_in_city := .N, by = city]
hotels[, popularity := bookings / bookings_in_city]
ggplot(hotels, aes(x = rating, y = popularity)) + geom_point(alpha = 0.2)

hotels[, popularity := cut(bookings, c(0, 3, 7, Inf))]
(p <- ggplot(hotels, aes(x = rating, fill = popularity)) + geom_density(alpha = 0.2))

## themes
library(ggthemes)
p + theme_economist() + scale_fill_economist()
p + theme_stata() + scale_fill_stata()
p + theme_excel() + scale_fill_excel()

ggplot(mtcars, aes(wt, hp, color = factor(am))) +
  geom_point() + theme_excel() + scale_color_excel()

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
p + theme_custom() + theme(panel.background = element_rect(
  fill = "#242323",
  color = "white",
  size = 2))
?theme

library(ggthemr)
?ggthemr
## https://github.com/cttobin/ggthemr

ggthemr(
  'pale',
  layout = 'scientific',
  spacing = 2,
  type = 'inner')
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

## ###############################################################################################
## interactive plots
## ###############################################################################################

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
  tooltip = paste('Number of gears:', gear))) +
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
