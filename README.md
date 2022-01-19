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

Please bring your own laptop* and make sure to install the below items **before** attending the first class:

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

(*) If you may not be able to use your own laptop, there's a shared RStudio Server set up in AWS for you. Look up the class Slack channel for how to access, or find below the steps how the service was configured:

<details><summary>💪 RStudio Server installation steps</summary>

```
echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" | sudo tee -a /etc/apt/sources.list.d/cran.list
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
sudo apt update && sudo apt upgrade
sudo apt install r-base gdebi-core r-cran-ggplot2 r-cran-gganimate
sudo apt install cargo libudunits2-dev libssl-dev libgdal-dev
wget https://download2.rstudio.org/server/bionic/amd64/rstudio-server-2021.09.2-382-amd64.deb
sudo gdebi rstudio-server-2021.09.2-382-amd64.deb
```

</details>

<details><summary>💪 Creating users</summary>

```r
secret <- 'something super secret'
users <- c('list', 'of', 'users')

library(logger)
library(glue)
for (user in users) {

  ## remove invalid character
  user <- sub('@.*', '', user)
  user <- sub('-', '_', user)
  user <- sub('.', '_', user, fixed = TRUE)
  user <- tolower(user)

  log_info('Creating {user}')
  system(glue("sudo adduser --disabled-password --quiet --gecos '' {user}"))

  log_info('Setting password for {user}')
  system(glue("echo '{user}:{secret}' | sudo chpasswd")) # note the single quotes + placement of sudo

  log_info('Adding {user} to sudo group')
  system(glue('sudo adduser {user} sudo'))

}
```

</details>

## Class Schedule

### Week 1

1. Warm-up exercise and security reminder: [1.R](1.R#L1)
2. Intro / recap on R and ggplot2 from previous courses by introducing MDS: [1.R](1.R#L52)
3. Scaling / standardizing variables: [1.R](1.R#L137)
4. Simpson's paradox: [1.R](1.R#L181)
5. Intro to `data.table`: [1.R](1.R#L238)
6. Anscombe's quartett [1.R](1.R#L339)

Suggested reading:

* [Introduction to `data.table`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
* [`data.table` FAQ](https://rdatatable.gitlab.io/data.table/articles/datatable-faq.html)
* [Database-like ops benchmark](https://h2oai.github.io/db-benchmark/)
* Hadley Wickham: *ggplot2: Elegant Graphics for Data Analysis*. https://ggplot2-book.org/

### Homework 1

0. Load the `nycflights13` package and check what kind of datasets exist in the package, then create a copy of flights dataset into a `data.table` object, called `flight_data`.
1. Which destination had the lowest avg arrival delay from LGA with minimum 100 flight to that destination?
2. Which destination's flights were the most on time (avg arrival delay closest to zero) from LGA with minimum 100 flight to that destination?
3. Who is the manufacturer of the plane, which flights the most to CHS destination?
4. Which airline (carrier) flow the most by distance?
5. Plot the monthly number of flights with 20+ mins arrival delay!
6. Plot the departure delay of flights going to IAH and the related day's wind speed on a scaterplot! Is there any association between the two variables? Try adding a linear model.
7. Plot the airports as per their geolocation on a world map, by mapping the number flights going to that destionation to the size of the symbol!

If in doubt about the results and outputs, see [this example submission prepared by Misi](https://www.dropbox.com/s/36zifeh40m7jzza/dv2-homework2.html?dl=1).

Submission: prepare an R markdown document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before Jan 19 noon (CET).

### Week 2

1. Homework format and solutions: [2.R](2.R#L1)
2. Geocoding: [2.R](2.R#L44)
3. Alternatives to boxplot: [2.R](2.R#L88)
4. Data patterns [2.R](2.R#L129)
5. Animations for hierarchical clustering: [2.R](2.R#L140)

Suggested reading:

* Hadley Wickham: *ggplot2: Elegant Graphics for Data Analysis*. https://ggplot2-book.org/
* Thomas Lin Pedersen: *gganimate. A Grammar of Animated Graphics*. https://gganimate.com/

### Week 3

Will be updated from week to week.

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
