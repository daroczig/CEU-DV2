https://daroczig.shinyapps.io/countdown/?title=Data Visualization 2&subtitle=Practical Data Visualization with R&schedule=2021-03-10 13:30:00

This is the R script repository of the "[Data Visualization 2: Practical Data Visualization with R](https://courses.ceu.edu/courses/2020-2021/data-visualization-2-practical-data-visualization-r)" course in the 2020/2021 Winter term, part of the [MSc in Business Analytics](https://courses.ceu.edu/programs/ms/master-science-business-analytics) at CEU. For the previous editions, see [2019/2020 Spring](https://github.com/daroczig/CEU-DV2/tree/2019-2020) and [2020/2021 Winter](https://github.com/daroczig/CEU-DV2/tree/2020-2019).

## Table of Contents

* [Syllabus](https://github.com/daroczig/CEU-DV2#syllabus)
* [Technical Prerequisites](https://github.com/daroczig/CEU-DV2#technical-prerequisites)
* [Class Schedule](https://github.com/daroczig/CEU-DV2#class-schedule)
* [Contact](https://github.com/daroczig/CEU-DV2#contacts)

## Schedule

3 x 100 mins on Jan 12:

* 13:30 - 15:00 session 1
* 15:00 - 15:30 break
* 15:30 - 17:00 session 2
* 17:00 - 17:30 break
* 17:30 - 19:00 session 3

2 x 2 x 75 mins on Jan 19 and 26:

* 16:30 - 17:45 session 1
* 17:45 - 18:00 break
* 18:00 - 19:15 session 2

## Location

Hybrid: in-person at the Budapest campus and on Zoom. Zoom URL shared in Moodle.

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

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
