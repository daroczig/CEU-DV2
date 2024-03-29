This is the R script repository of the "[Data Visualization 3: Practical Data Visualization with R](https://courses.ceu.edu/courses/2022-2023/data-visualization-2-practical-data-visualization-r)" course in the 2023/2024 Winter term, part of the [MSc in Business Analytics](https://courses.ceu.edu/programs/ms/master-science-business-analytics) at CEU. For the previous editions, see [2019/2020 Spring](https://github.com/daroczig/CEU-DV2/tree/2019-2020), [2020/2021 Winter](https://github.com/daroczig/CEU-DV2/tree/2020-2021), [2021/2022 Winter](https://github.com/daroczig/CEU-DV2/tree/2021-2022), and [2022/2023 Winter](https://github.com/daroczig/CEU-DV2/tree/2022-2023).

## Table of Contents

* [Schedule](https://github.com/daroczig/CEU-DV2#schedule)
* [Syllabus](https://github.com/daroczig/CEU-DV2#syllabus)
* [Technical Prerequisites](https://github.com/daroczig/CEU-DV2#technical-prerequisites)
* [Class Schedule](https://github.com/daroczig/CEU-DV2#class-schedule)
* [Contact](https://github.com/daroczig/CEU-DV2#contacts)

## Schedule

3 x 100 mins on Jan 24 and 31:

* 13:30 - 15:10 session 1
* 15:10 - 15:40 break
* 15:40 - 17:20 session 2
* 17:20 - 17:40 break
* 17:40 - 19:20 session 3

## Location

In-person at the Vienna campus (QS B-421).

## Syllabus

Please find in the `syllabus` folder of this repository.

## Technical Prerequisites

Please bring your own laptop* and make sure to install the below items **before** attending the first class:

0. Join the Slack channel dedicated to the class (`#ba-dv3-2023`)
1. Install `R` from https://cran.r-project.org
2. Install `RStudio Desktop` (Open Source License) from https://posit.co/download/rstudio-desktop/
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
# most recent R builds
wget -q -O- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" | sudo tee -a /etc/apt/sources.list.d/cran_r.list
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 67C2D66C4B1D4339 51716619E084DAB9
sudo apt update && sudo apt upgrade
sudo apt install r-base
# apt builds of all CRAN packages
wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc | sudo tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
echo "deb [arch=amd64] https://r2u.stat.illinois.edu/ubuntu jammy main" | sudo tee -a /etc/apt/sources.list.d/cranapt.list
sudo apt update
# install some R packages
sudo apt install -y r-base gdebi-core r-cran-ggplot2 r-cran-gganimate
sudo apt install -y cargo libudunits2-dev libssl-dev libgdal-dev desktop-file-utils
sudo apt install -y r-cran-data.table r-cran-rcpp r-cran-dplyr r-cran-ggally r-cran-pander r-cran-readxl
sudo apt install -y r-cran-ggrepel r-cran-hexbin r-cran-animation r-cran-dendextend r-cran-nbclust
sudo apt install -y r-cran-ggmap r-cran-maps r-cran-devtools r-cran-ggraph r-cran-ggthemes
sudo apt install -y r-cran-leaflet r-cran-mapproj
wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2023.12.0-369-amd64.deb
sudo gdebi rstudio-server-*.deb
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
2. Intro / recap on R and ggplot2 from previous courses by introducing MDS: [1.R](1.R#L66)
3. Geocoding: [1.R](1.R#L156)
4. Shapefiles: [1.R](1.R#L235)
5. Scaling / standardizing variables: [1.R](1.R#L1356)
6. Simpson's paradox: [1.R](1.R#L401)
7. Anscombe's quartett [1.R](1.R#L462)
8. Intro to datasaurus [1.R](1.R#L517)

### Week 2

1. Review homework: [2.R](2.R#L1) and [homework-solutions.rmd](homework-solutions.rmd)
2. Recap on `data.table` summaries: [2.R](2.R#L22)
3. Warm-up exercises on `ggplot2` calls and EDA [2.R](2.R#L60)
4. Extract points from a plot [2.R](2.R#L120)
5. Hierarchical clustering [2.R](2.R#L156)
6. Animations [2.R](2.R#L310)
7. Themes [2.R](2.R#L384)
8. Interactive plots [2.R](2.R#L459)

To be updated from week to week.

## Homeworks

### Homework 1

0. Load the `nycflights13` package and check what datasets are bundled.
1. Visualize the distribution of arrival delays per origin airport!
2. Visualize the distribution of arrival delays per destination airport! Note that the x axis labels need to be rotated, and spend some time cleaning up the axis and plot titles! Might need to tweak `fig.height` and `fig.width` params of the code chunk in Rmd!
3. Compute and visualize the average arrival delay per destination! Make sure to handle `NA`s and order the barplot by the average delay.
4. Redo the above plot by showing the actual name of the destination airport instead of it's FAA id!
5. Color the bars by the timezone of the airport! Make sure to render the legend on the top, using a single line.
6. Geocode the destination airports, then visualize those on a worldmap with the point sizes relative to the number of flights to there from NY!
7. Compute the average departure and arrival delays, also the average air time and distance per destination, then pass the scaled dataframe through MDS to visualize the similarities/dissimilarities of the destination airports!

If in doubt about what is expected and how the results and outputs should look like, see [this example solution](https://rawcdn.githack.com/daroczig/CEU-DV2/ff034e57d118cbb7e11c0fefb63c06737e8ce734/homework-solutions.html).

Submission: prepare an R Markdown or Quartro document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before Jan 31 noon (CET).

### Final project

Use any publicly accessible dataset (preferably from the [TidyTuesday projects](https://github.com/rfordatascience/tidytuesday)), but if you don't feel creative, feel free to pick the [`palmerpenguins` dataset](https://allisonhorst.github.io/palmerpenguins/.) and demonstrate what you have learned in this class by generating different data visualizations that makes sense and are insightful, plus provide comments on those in plain English. This can totally be a continuation of your Intro to R submission.

Required items to include in your work for grade "B":
- at least 5 plots (with at least 3 different `ggplot2` geoms) not presented yet in any of your previous CEU projects
- a meaningful animation using `gganimate` (instead of presenting random stuff moving around, make sure that rendering an animation for your use-case makes sense, e.g. showing how things changed over time)
- either (1) register an account at stadiamap to fetch map tiles or (2) use shapefile(s) to present some geospatial data (e.g. points or polygons rendered on a background map)

For grade "A":
- make sure to fine-tune your plots and make those pretty by always setting proper (axis) titles, scales, custom color palettes etc.
- define a custom theme (e.g. background color, grid, font family and color) and use that on all (or at least on most) plots
- at least one interactive plot

Submission: prepare an R Markdown or Quartro document that includes the exercise as a regular paragraph then the solution in an R code chunk (printing both the code and its output) and knit to HTML or PDF and upload to Moodle before Feb 18 midnight (CET).

## Contact

File a [GitHub ticket](https://github.com/daroczig/CEU-DV2/issues).
