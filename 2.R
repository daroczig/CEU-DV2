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

## QQ how long it took? 1/2/4 hours?
## TODO check Rmd at homework-solutions.rmd
## TODO discuss reordering categories (ex 3)
## TODO geocode -- was already part of the airports dataset
## TODO recap on data.table

## #############################################################################
## data.table recap
## #############################################################################

## aggregation
library(nycflights13)
aggregate(arr_delay ~ dest, data = flights, FUN = function(x) mean(x, na.rm = TRUE))

library(data.table)
data.table(flights)[, .(arr_delay = mean(arr_delay, na.rm = TRUE)), by = dest]

## multiple summaries: e.g. include dep_delay as well
data.table(flights)[, .(
  arr_delay = mean(arr_delay, na.rm = TRUE),
  dep_delay = mean(dep_delay, na.rm = TRUE)
), by = dest]

data.table(flights)[, lapply(.SD, mean), by = dest, .SDcols = c('arr_delay', 'dep_delay')]
data.table(flights)[, lapply(.SD, mean, na.rm = TRUE), by = dest, .SDcols = c('arr_delay', 'dep_delay')]

## round up to the minutes
data.table(flights)[, lapply(.SD, function(x) round(mean(x, na.rm = TRUE))),
  by = dest, .SDcols = c('arr_delay', 'dep_delay')]

roundmean <- function(x) round(mean(x, na.rm = TRUE), 2)
data.table(flights)[, lapply(.SD, roundmean), by = dest, .SDcols = c('arr_delay', 'dep_delay')]

## other columns
numcols <- c('arr_delay', 'dep_delay')
data.table(flights)[, lapply(.SD, roundmean), by = dest, .SDcols = numcols]

lapply(flights, is.numeric)
sapply(flights, is.numeric)
numcols <- which(sapply(flights, is.numeric))
data.table(flights)[, lapply(.SD, roundmean), by = dest, .SDcols = numcols]

## TODO check out Introduciton to R / Week 2!

## #############################################################################
## EDA warmup - alternatives to boxplot
## #############################################################################

library(data.table)
df <- fread('http://bit.ly/CEU-R-numbers-set')
str(df)

summary(df)
table(df$x)

summary(df)
lapply(df, summary)
lapply(unique(df$x), function(set) summary(df[x == set]))

## data.table way
df[, as.list(summary(y)), by = x]

library(ggplot2)
ggplot(df, aes(x, y)) + geom_point()
ggplot(df, aes(x, y)) + geom_point() + geom_smooth(method = 'lm')
ggplot(df, aes(x, y)) + geom_point(alpha = 0.1)
ggplot(df, aes(x, y)) + geom_jitter(alpha = 0.1)

## hexbin
ggplot(df, aes(x, y)) + geom_hex()

ggplot(df, aes(factor(x), y)) + geom_boxplot()

ggplot(df, aes(factor(x), y)) + geom_violin()
ggplot(df, aes(factor(x), y)) + geom_violin() + geom_jitter()
ggplot(df, aes(factor(x), y)) + geom_violin() +
  geom_jitter(width = 0.1, alpha = 0.1)

ggplot(df, aes(y)) + geom_histogram() + facet_wrap(~x)

ggplot(df, aes(y, fill = factor(x))) + geom_density()
ggplot(df, aes(y, fill = factor(x))) + geom_density(alpha = .25)
ggplot(df, aes(y, fill = factor(x))) +
  geom_density(alpha = .25) +
  theme(legend.position = 'top')

## df <- rbind(
##     data.table(x = 1, y = rbeta(1e3, 0.1, 0.1)),
##     data.table(x = 2, y = rnorm(1e3, 0.5, 0.75)),
##     data.table(x = 3, y = runif(1e3) * 2 - 0.5),
##     data.table(x = 4, y = rnorm(1e3, 0.5, 0.75)))

## TODO do similar exploratory data analysis on the below dataset
df <- read.csv('http://bit.ly/CEU-R-numbers')
## generated at https://gist.github.com/daroczig/23d1323652a70c03b27cfaa6b080aa3c

## TODO find interesting pattern in data?

ggplot(df, aes(x, y)) + geom_point() # slow?
ggplot(df, aes(x, y)) + geom_point(alpha = 0.05)
ggplot(df, aes(x, y)) + geom_point(size = 0.2, alpha = 0.1)
ggplot(df, aes(x, y)) + geom_hex(binwidth = 5)
ggplot(df, aes(x, y)) + geom_count()

## #############################################################################
## manually extract data from plots
## #############################################################################

## using grid.locator
devtools::install_github('doehm/traceR')

library(traceR)
## NOTE locator is not reliable in the RStudio IDE built-in graphics device
## so we need to open a new device
grDevices::x11()
df2 <- trace_image()
df2
inspect_trace(df2)

ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_path()
library(ggforce)
ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_bspline0()
ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_bspline_closed0()

## this fish doesn't have an eye!
df3 <- trace_image()
ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_bspline_closed0() +
  geom_bspline_closed0(data=df3, fill = 'white')

## oops
df3 <- trace_image(scale = FALSE)
ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_bspline_closed0() +
  geom_bspline_closed0(data=df3, fill = 'white')

ggplot(df, aes(x, y)) + geom_hex(binwidth = 5)
df2 <- trace_image(scale = FALSE)
df3 <- trace_image(scale = FALSE)
ggplot(df2, aes(x, y)) + geom_point(color = 'red') + geom_bspline_closed0() +
  geom_bspline_closed0(data=df3, fill = 'white')

## #############################################################################
## why the dataviz? edu purposes, eg showing how clustering works
## #############################################################################

## hierarchical clustering
?hclust

## doing this on iris instead of mtcars (re MDS) to be able to compare with Species
dm <- dist(iris[, 1:4])
str(dm)

hc <- hclust(dm)
str(hc)

## plot the dendogram
plot(hc)
rect.hclust(hc, k = 3)

## see first 2 observations merged into a cluster:
iris[c(102, 143), ]

## poor man's animation
for (i in 2:8) {
    plot(hc)
    rect.hclust(hc, k = i)
    Sys.sleep(1)
}

## actual animation!
library(animation)
ani.options(interval = 1)
ani.options(autobrowse = FALSE) # on server
saveGIF({
    for (i in 2:8) {
        plot(hc)
        rect.hclust(hc, k = i)
        ## no need for sleep
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

## TODO create an animation with ggplot2 instead of the above plot/rect.hclust method
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
## hm, that's difficult to interpret .. e.g. why 2 green lines?
## let's differentiate line types as well?
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
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = factor(clusters))) +
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

## TODO this should have been a facet ...
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = factor(clusters))) +
    geom_point(size = 3) +
    geom_smooth(method = 'lm') +
    facet_wrap(~Species)

## tradition cluster plots
library(cluster)
clusplot(iris, clusters, color = TRUE, shade = TRUE, labels = 2)

## gg Hull plot
## "convex hull of a shape is the smallest convex set that contains it"
library(ggforce)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = factor(clusters))) +
    geom_point(size = 3) +
    geom_mark_hull(aes(label = clusters))

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
    geom_point(size = 3) +
    geom_mark_hull(aes(fill = factor(clusters), label = clusters),
      ## very concave
      concavity = 1)

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

## TODO racing bar chart on departure delay per origin
library(nycflights13)
library(data.table)
dt <- data.table(flights)[, .(delay = mean(dep_delay, na.rm = TRUE)), by = .(origin, month)][order(month, delay)]
## NOTE need to record order within the dataset that we can map it to the plot
dt[, order := order(delay), by = month]
library(ggplot2)
library(gganimate)
g <- ggplot(dt, aes(order, delay)) +
  geom_col(aes(fill = origin)) +
  ## NOTE need to add text as X axis labels are for order
  ## NOTE vjust/hjust https://stackoverflow.com/a/7267364/564164
  geom_text(aes(label = origin), hjust = 1.2, vjust = 0.5, angle = 90, size = 14, color = 'white') +
  xlab('') + ylab('Average departure delays (minutes)') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  ## NOTE look up month based on closest state
  ggtitle('2013/{month.name[as.numeric(closest_state)]}') +
  transition_states(month) +
  view_follow(fixed_x = FALSE)
animate(g, height = 600, width = 600)

## TODO racing bar chart on arr delay per dest
dt <- data.table(flights)[, .(delay = mean(arr_delay, na.rm = TRUE)), by = .(dest, month)][order(month, delay)]
dt[, order := order(delay), by = month]
g <- ggplot(dt, aes(order, delay)) +
  geom_col(aes(fill = dest)) +
  geom_text(aes(label = dest, hjust = delay < 0), angle = 90, size = 2) +
  xlab('') + ylab('Average arrival delays (minutes)') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  ggtitle('2013/{month.name[as.numeric(closest_state)]}') +
  transition_states(month) +
  view_follow(fixed_x = FALSE)
animate(g, height = 600, width = 1200, fps = 5)

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

remotes::install_github('cttobin/ggthemr')
library(ggthemr)
?ggthemr
## https://github.com/cttobin/ggthemr

ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')
p

ggthemr('grass', layout = 'clear', spacing = 2, type = 'outer')
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

ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_density(alpha = .5)

## drop back to the default colors and layout
ggthemr_reset()

## ############################################################################
## interactive plots
## ############################################################################

## plot weight and acceleration of cars on a scatterplot
## colored by transmission
(p <- ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point())

## much easier approach, although less control
library(plotly)
ggplotly(p)

## even preserves the set ggplot theme:
ggthemr('grass', layout = 'clear', spacing = 2, type = 'outer')
ggplotly(p)
ggthemr_reset()


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
