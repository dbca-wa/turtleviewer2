# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check()
rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()


## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile(from="rocker/geospatial:4.1.2")

# replace in Dockerfile:
# RUN apt-get update && apt-get install -y  gdal-bin git-core freetds-dev tdsodbc libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libjq-dev libpng-dev libproj-dev libprotobuf-dev libssl-dev libudunits2-dev libv8-dev libxml2-dev make pandoc pandoc-citeproc protobuf-compiler libprotoc-dev unixodbc-dev zlib1g-dev && rm -rf /var/lib/apt/lists/*
#  ...
# RUN Rscript -e 'remotes::install_github("r-spatial/sf")'
# RUN Rscript -e 'remotes::install_github("Thinkr-open/golem")'
# RUN Rscript -e 'remotes::install_github("RinteRface/bs4Dash")'
# RUN Rscript -e 'remotes::install_github("dbca-wa/wastdr", version = "0.8.6")'

system("docker build . -t dbca-wa/turtleviewer2:latest")
