This is the R script repository of the "[Data Visualization 2: Practical Data Visualization with R](https://courses.ceu.edu/courses/2020-2021/data-visualization-2-practical-data-visualization-r)" course in the 2020/2021 Winter term, part of the [MSc in Business Analytics](https://courses.ceu.edu/programs/ms/master-science-business-analytics) at CEU. For the previous edition, see [2019/2020 Spring](https://github.com/daroczig/CEU-DV2/tree/2019-2020).

## Table of Contents

* [Syllabus](https://github.com/daroczig/CEU-DV2#syllabus)
* [Technical Prerequisites](https://github.com/daroczig/CEU-DV2#technical-prerequisites)
* [Class Schedule](https://github.com/daroczig/CEU-DV2#class-schedule)

    * [Week 1](https://github.com/daroczig/CEU-DV2#week-1)
    * [Week 2](https://github.com/daroczig/CEU-DV2#week-2)

* [Contact](https://github.com/daroczig/CEU-DV2#contacts)

## Schedule

2 x 3 x 100 mins on March 10 and 17:

* 13:30 - 15:00 session 1
* 15:00 - 15:30 break
* 15:30 - 17:00 session 2
* 17:00 - 17:30 break
* 17:30 - 19:00 session 3

## Location

This class will take place online. Find the Zoom URL shared in Moodle.

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
2. Intro / recap on R and ggplot2 from previous courses by introducing MDS: [1.R](1.R#L49)
3. Scaling / standardizing variables: [1.R](1.R#L134)
4. Simpson's paradox: [1.R](1.R#L177)
5. Intro to `data.table`: [1.R](1.R#L234)
6. Anscombe's quartett [1.R](1.R#L389)

Suggested reading:

* [Introduction to `data.table`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
* [`data.table` FAQ](https://rdatatable.gitlab.io/data.table/articles/datatable-faq.html)
* [Database-like ops benchmark](https://h2oai.github.io/db-benchmark/)
* Hadley Wickham: *ggplot2: Elegant Graphics for Data Analysis*. https://ggplot2-book.org/

Homework:

0. Load the `nycflights13` package and check what kind of datasets exist in the package, then create a copy of flights dataset into a `data.table` object, called `flight_data`.
1. Which destination had the lowest avg arrival delay from LGA with minimum 100 flight to that destination?
2. Which destination's flights were the most on time (avg arrival delay closest to zero) from LGA with minimum 100 flight to that destination?
3. Who is the manufacturer of the plane, which flights the most to CHS destination?
4. Which airline (carrier) flow the most by distance?
5. Plot the monthly number of flights with 20+ mins arrival delay!
6. Plot the departure delay of flights going to IAH and the related day's wind speed on a scaterplot! Is there any association between the two variables? Try adding a linear model.
7. Plot the airports as per their geolocation on a world map, by mapping the number flights going to that destionation to the size of the symbol!

If in doubt about the results and outputs, see [this example submission prepared by Misi](https://www.dropbox.com/s/36zifeh40m7jzza/dv2-homework2.html?dl=1).

Submission: prepare an R markdown document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before March 17 noon (CET).

### Week 2

1. Homework solutions [2.R](2.R#L1)
2. Alternatives to boxplot [2.R](2.R#L33)
3. Hierarchical clustering, dendograms [2.R](2.R#L74)
4. Datasaurus [2.R](2.R#197)
5. Multiple summaries with `data.table` [2.R](2.R#L252)
6. Tweaking `ggplot2` themes [2.R](2.R#L339)
7. Introduction to interactive plots [2.R](2.R#424)
8. PCA for image processing [2.R](2.R#467)

Suggested reading:

* Hadley Wickham: *ggplot2: Elegant Graphics for Data Analysis*. https://ggplot2-book.org/
* Thomas Lin Pedersen: *gganimate. A Grammar of Animated Graphics*. https://gganimate.com/
* `data.table` cheatsheet. https://s3.amazonaws.com/assets.datacamp.com/img/blog/data+table+cheat+sheet.pdf

~~Homework~~Final project:

Participite in the next week of #tidytuesday at https://github.com/rfordatascience/tidytuesday -- feel free to use the bundled `games.csv` dataset (or provide your own data collected elsewhere), optionally merge external dataset(s), do data transformations that seems useful and generate data visualizations that makes sense and are insightful, plus provide comments on those in plain English.

Submission: prepare an R markdown document that includes plain English text description of the dataset, problem/question you analyzed, actual R code chunks (printing both the code and its output) doing the analysis, comments and summary of results and knit to HTML or PDF and upload to Moodle before April 7, 2021 midnight (CET). Please don't leave the submission for the last minute and be sure to submit by April 2, 2021 if you would like to get some feedback before the final deadline.

Grading: reading the required data and doing some plots in an R Markdown document as per the above specswill get you pass, but please actually spend time on getting familiar with the data, and do a proper analysis for better grades (no hard-specs, though, but make use of your common sense).

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
