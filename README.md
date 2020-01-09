This is the R script repository of the "[Data Visualization 2: Practical Data Visualization with R](https://courses.ceu.edu/courses/2019-2020/data-visualization-2-practical-data-visualization-r)" course in the 2019/2020 Winter term, part of the [MSc in Business Analytics](https://courses.ceu.edu/programs/ms/master-science-business-analytics) at CEU. 

## Table of Contents

* [Syllabus](https://github.com/daroczig/CEU-DV2#syllabus)
* [Technical Prerequisites](https://github.com/daroczig/CEU-DV2#technical-prerequisites)
* [Class Schedule](https://github.com/daroczig/CEU-DV2#class-schedule)

    * [Week 1](https://github.com/daroczig/CEU-DV2#week-1)

* [Contact](https://github.com/daroczig/CEU-DV2#contacts)

## Schedule

3 x 2 x 100 mins on Jan 8, 15, 22:

* 13:30 - 15:10 session 1
* 15:10 - 15:30 break
* 15:30 - 17:10 session 2

## Syllabus

Please find in the `syllabus` folder of this repository.

## Technical Prerequisites

Please bring your own laptop and make sure to install the below items **before** attending the first class:

1. Install `R` from https://cran.r-project.org
2. Install `RStudio Desktop` (Open Source License) from https://www.rstudio.com/products/rstudio/download
3. Register an account at https://github.com
4. Enter the following commands in the R console (bottom left panel of RStudio) and make sure you see a plot in the bottom right panel and no errors in the R console:

```r
install.packages(c('ggplot2', 'gganimate', 'transformr', 'gifski'))
library(ggplot2)
library(gganimate)
ggplot(diamonds, aes(cut)) + geom_bar() +
    transition_states(color, state_length = 0.1)
```

Optional steps I highly suggest to do as well before attending the class if you plan to use `git`:

4. Bookmark, watch or star this repository so that you can easily find it later
5. Install `git` from https://git-scm.com/
6. Verify that in RStudio, you can see the path of the `git` executable binary in the Tools/Global Options menu's "Git/Svn" tab -- if not, then you might have to restart RStudio (if you installed git after starting RStudio) or installed git by not adding that to the PATH on Windows. Either way, browse the "git executable" manually (in some `bin` folder look for thee `git` executable file).
7. Create an RSA key (optionally with a passphrase for increased security -- that you have to enter every time you push and pull to and from GitHub). Copy the public key and add that to you SSH keys on your GitHub profile.
8. Create a new project choosing "version control", then "git" and paste the SSH version of the repo URL copied from GitHub in the pop-up -- now RStudio should be able to download the repo. If it asks you to accept GitHub's fingerprint, say "Yes".
9. If RStudio/git is complaining that you have to set your identity, click on the "Git" tab in the top-right panel, then click on the Gear icon and then "Shell" -- here you can set your username and e-mail address in the command line, so that RStudio/git integration can work. Use the following commands:

    ```sh
    $ git config --global user.name "Your Name"
    $ git config --global user.email "Your e-mail address"
    ```
    Close this window, commit, push changes, all set.

Find more resources in Jenny Bryan's "[Happy Git and GitHub for the useR](http://happygitwithr.com/)" tutorial if in doubt or [contact me](#contact).

## Class Schedule

Will be updated from week to week.

### Week 1

1. Warm-up exercise and security reminder: [1.R](1.R#L1)
2. Intro / recap on R and ggplot2 from previous courses by introducing MDS: [1.R](1.R#L12)
3. Scaling / standardizing variables: [1.R](1.R#L72)
4. Simpson's paradox: [1.R](1.R#L150)
5. Intro to `data.table`: [1.R](1.R#L194)

Suggested reading:

* [Introduction to `data.table`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
* [`data.table` FAQ](https://rdatatable.gitlab.io/data.table/articles/datatable-faq.html)
* [Database-like ops benchmark](https://h2oai.github.io/db-benchmark/)

Homework:

1. Load `bookings` data from http://bit.ly/CEU-R-hotels-2018-prices and the hotel `features` from http://bit.ly/CEU-R-hotels-2018-features
2. Count the number of 4 stars hotels in Hungary
3. Compute the average rating of 4 and 5 star hotels in Hungary and Germany
4. Round up the previously computed average rating to 2 digits
5. Do we have any bookings in unknown hotels (as per the features dataset)?
6. Clean up the bookings dataset from bookings from unknown hotels and print the number of remaining bookings
7. What's the average distance of hotels from the city central in Budapest
8. List all neighbourhoods in Budapest
9. Compute the average distance from the city center for the neighbourhoods in Budapest
10. Count the number of bookings in Hungary

Homework extra:

1. Create a scatterplot on the `iris` dataset using the length and width of sepal + 4 linear models (3 colored lines per species, 1 black line fitted on the global dataset)

Submission: prepare an R markdown document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before Jan 14 midnight (CET)

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
