## #############################################################################
## warm-up exercise and security reminder

## running an R script from a trusted source
source('http://bit.ly/CEU-R-shoes')

## this is a dataset on students from a study group,
## where we run a math test and found interesting association with the shoe size
ls()
students
plot(students$shoe, students$math)

## EDA - everyone!
str(students)
summary(students)
plot(students)

library(ggplot2)
ggplot(students, aes(math, shoe, color = z)) + geom_point()
ggplot(students, aes(math, shoe, color = y)) + geom_point() # !!

library(GGally)
ggpairs(students)

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
ls(all = TRUE)
.secret # "A warm hello from the Internet."

readLines('http://bit.ly/CEU-R-shoes')

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

## TODO ggplot2 way
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

## TODO add model
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point() + geom_smooth(method = 'lm')

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point(aes(color = Species)) +
    geom_smooth(aes(color = Species), method = 'lm') +
    geom_smooth(method = 'lm', color = 'black', lwd = 2)

## compare the overlap of groups with the MDS version:
mds <- as.data.frame(cmdscale(dist(iris)))
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point()
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point() + geom_smooth(method = 'lm')

## without the Species column
mds <- as.data.frame(cmdscale(dist(iris[, -5])))
ggplot(mds, aes(V1, V2, color = iris$Species)) + geom_point()

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

## #############################################################################
## creating new variables: numeric

## new variable via the data.frame way
bookings$price_per_night <- bookings$price / bookings$nnights
## new variable via data.table's in-place update operator
bookings[, price_per_night := price / nnights]

hotels <- merge(
  features,
  bookings[, .(bookings = .N, price_per_night = mean(price_per_night)), by = hotel_id],
  by = 'hotel_id')

## TODO compute average price per number of stars
hotels[, mean(price_per_night), by = stars]
hotels[, mean(price_per_night), by = stars][order(stars)]
hotels[!is.na(stars), mean(price_per_night), by = stars][order(stars)]
hotels[!is.na(stars), .(rating = mean(price_per_night)), by = stars][order(stars)]
## NOTE we have not weighted ...
hotels[!is.na(stars), .(rating = weighted.mean(
  price_per_night, bookings, na.rm = TRUE)), by = stars][order(stars)]

plot(hotels[!is.na(stars), .(rating = weighted.mean(
  price_per_night, bookings, na.rm = TRUE)), by = stars][order(stars)])

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
