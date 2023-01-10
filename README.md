This is the R script repository of the "[Data Visualization 2: Practical Data Visualization with R](https://courses.ceu.edu/courses/2022-2023/data-visualization-2-practical-data-visualization-r)" course in the 2022/2023 Winter term, part of the [MSc in Business Analytics](https://courses.ceu.edu/programs/ms/master-science-business-analytics) at CEU. For the previous editions, see [2019/2020 Spring](https://github.com/daroczig/CEU-DV2/tree/2019-2020), [2020/2021 Winter](https://github.com/daroczig/CEU-DV2/tree/2020-2021), and [2020/2021 Winter](https://github.com/daroczig/CEU-DV2/tree/2021-2022).

## Table of Contents

* [Schedule](https://github.com/daroczig/CEU-DV2#schedule)
* [Syllabus](https://github.com/daroczig/CEU-DV2#syllabus)
* [Technical Prerequisites](https://github.com/daroczig/CEU-DV2#technical-prerequisites)
* [Class Schedule](https://github.com/daroczig/CEU-DV2#class-schedule)
* [Contact](https://github.com/daroczig/CEU-DV2#contacts)

## Schedule

3 x 100 mins on Jan 9 and 16:

* 13:30 - 15:10 session 1
* 15:10 - 15:40 break
* 15:40 - 17:20 session 2
* 17:20 - 17:40 break
* 17:40 - 19:20 session 3

## Location

In-person at the Vienna campus (QS C-419).

## Syllabus

Please find in the `syllabus` folder of this repository.

## Technical Prerequisites

Please bring your own laptop* and make sure to install the below items **before** attending the first class:

0. Join the Slack channel dedicated to the class (`#ba-dv2-2022`)
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

(*) If you may not be able to use your own laptop, there's a shared RStudio Server set up in AWS for you - including all the required R packages already installed for you. Look up the class Slack channel for how to access, or find below the steps how the service was configured:

<details><summary>💪 RStudio Server installation steps</summary>

```
sudo apt install -y --no-install-recommends software-properties-common dirmngr
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+
sudo apt update && sudo apt upgrade
sudo apt install -y r-base gdebi-core r-cran-ggplot2 r-cran-gganimate
sudo apt install -y cargo libudunits2-dev libssl-dev libgdal-dev
sudo apt install -y r-cran-data.table r-cran-rcpp r-cran-dplyr r-cran-ggally r-cran-pander r-cran-readxl
sudo apt install -y r-cran-ggrepel r-cran-hexbin r-cran-animation r-cran-dendextend r-cran-nbclust
sudo apt install -y r-cran-ggmap r-cran-maps
wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2022.12.0-353-amd64.deb
sudo gdebi rstudio-server-2022.12.0-353-amd64.deb
# never do this in prod
echo "www-port=80" | sudo tee -a /etc/rstudio/rserver.conf
sudo rstudio-server restart
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
2. Intro / recap on R and ggplot2 from previous courses by introducing MDS: [1.R](1.R#L56)
3. Scaling / standardizing variables: [1.R](1.R#L144)
4. Simpson's paradox: [1.R](1.R#L189)
5. Intro to `data.table`: [1.R](1.R#L250)
6. Geocoding: [1.R](2.R#L353)
7. Anscombe's quartett [1.R](1.R#L391)

Suggested reading:

* [Introduction to `data.table`](https://rdatatable.gitlab.io/data.table/articles/datatable-intro.html)
* [`data.table` FAQ](https://rdatatable.gitlab.io/data.table/articles/datatable-faq.html)
* [Database-like ops benchmark](https://h2oai.github.io/db-benchmark/)
* Hadley Wickham: *ggplot2: Elegant Graphics for Data Analysis*. https://ggplot2-book.org/

### Homework

Replicate https://rpubs.com/daroczig-ceu/dv2-homework-2022 (find source dataset on Moodle).

Submission: prepare an R markdown document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before Jan 16 noon (CET). Publishing on RPubs and sharing that URL on Moodle is a plus.

To be updated from week to week.

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
