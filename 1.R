## #############################################################################
## warm-up exercise and security reminder

## clean your R session from past objects ...
rm(list = ls())

## running an R script from a trusted source
source('http://bit.ly/CEU-R-heights-2018')
ls()
heights

## but wow:
ls(all = TRUE)
.secret # "A warm hello from the Internet."

readLines('http://bit.ly/CEU-R-heights-2018')

## Take aways:
## * don't use `rm(list = ls())` in your scripts ... rather set RStudio to never save/load your session again
## * don't `source` R scripts downloaded from the Internet without first checking the content

## #############################################################################
## intro / recap on R and ggplot2 from previous courses by introducing MDS

## distance between 40 Hungarian cities -> 2D scatterplot
## http://bit.ly/hun-cities-distance

## download data
t <- tempfile()
t
t <- tempfile(fileext = '.xls')
t
download.file('http://bit.ly/hun-cities-distance', t)
## on windows, you might need to specify the mode as well
download.file('http://bit.ly/hun-cities-distance', t, mode = 'wb')

## further checks on the downloaded file
file.info(t)
pander::openFileInOS(t)

## read the downloaded file
library(readxl)
cities <- read_excel(t)

cities
## tibble VS data.frame VS data.table
str(cities)

## get rid of 1st column and last row (metadata)
cities <- cities[, -1]
cities <- cities[-nrow(cities), ]
str(cities)

mds <- cmdscale(as.dist(cities))
mds

plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## flipping both x and y axis
mds <- -mds
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

## ggplot2 way
mds <- as.data.frame(mds)
mds$city <- rownames(mds)
str(mds)

library(ggplot2)
ggplot(mds, aes(V1, V2, label = city)) +
    geom_text() + theme_bw()

## #############################################################################
## TODO visualize the distance between the European cities
## stored in the built-in dataframe:

?eurodist

mds <- cmdscale(eurodist)
mds <- as.data.frame(mds)
mds$city <- rownames(mds)
ggplot(mds, aes(V1, -V2, label = city)) +
    geom_text() + theme_bw()

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
## NOTE think about why the above visualization is off

## check actual distances eg for Camaro (or other sport cars)
which(rownames(mtcars) == 'Camaro Z28')
sort(as.matrix(dist(mtcars))[, 24])

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
p + facet_wrap(~Dept) + scale_fill_brewer(palette = 'Dark2')

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
## compare the overlap of groups with the MDS version:
mds <- as.data.frame(cmdscale(dist(iris)))
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point()
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point() + geom_smooth(method = 'lm')

## #############################################################################
## more on data transformations: data.table

library(data.table)

?fread

bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
str(bookings)
bookings

## dt[i]
bookings[1]
bookings[1:5]
bookings[price < 100]
bookings[offer == 0 & price < 100]
bookings[price < 100 & offer == 0 & nnights == 4 & holiday == 1]
bookings[price < 100 & offer == 0 & nnights == 4 & holiday == 1][1:5]

## dt[i, j]
bookings[, .N]
bookings[price < 100, .N]
bookings[price < 100, mean(price)]
bookings[price < 100, summary(price)]
bookings[price < 100, hist(price)]

## TODO compute the average price of bookings on weekends
bookings[weekend == 1, mean(price)]
## TODO compute the average price of bookings on weekdays
bookings[weekend == 0, mean(price)]

bookings[, mean(price), by = weekend] # dt[, j, by]
bookings[, mean(price), by = weekend][order(weekend)] # dt[, j, by][i]
?setorder

str(bookings)
bookings[, mean(price), by = list(weekend, nnights)]
bookings[, mean(price), by = .(weekend, nnights, holiday, year)]

bookings[, .(price = mean(price)), by = .(weekend, nnights, holiday, year)]

bookings[, .(price = mean(price), .N), by = .(weekend, nnights, holiday, year)]
bookings[, .(price = mean(price), .N, min = min(price), max = max(price)),
         by = .(weekend, nnights, holiday, year)]

## TODO compute the average price per number of stars
?merge
## x[y] -> rolling joins, overlap joins

features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
merge(bookings, features)

## NOTE hotel_id was picked as the join key
bookings[hotel_id == 1]
features[hotel_id == 1]
merge(bookings, features)[hotel_id == 1]

## NOTE there's a missing row??
bookings[, .N]
merge(bookings, features)[, .N]

features[is.na(hotel_id)]
bookings[!hotel_id %in% features$hotel_id]
features[hotel_id == 2]

## NOTE do we really need to merge features to all bookings??
booking_summary <- bookings[, .(price = mean(price)), by = hotel_id]
hotels <- merge(features, booking_summary, by = 'hotel_id')
hotels[, mean(price), by = stars][order(stars)]

## hm ... 4.5 stars more expensive than 5 stars?
## probaly only a few cases with 4.5 stars ...
## should we divide price per nnights?
bookings[hotel_id == 2]

booking_summary <- bookings[, .(bookings = .N, price = mean(price/ nnights)), by = hotel_id]
hotels <- merge(features, booking_summary, by = 'hotel_id')
hotels[, .(bookings = sum(bookings), price = mean(price)), by = stars][order(stars)]
