FROM rocker/geospatial:4.1.2
LABEL maintainer=Florian.Mayer@dbca.wa.gov.au
LABEL description="rocker/geospatial:4.1.1 with Shiny app turtleviewer2"
RUN apt-get update && apt-get install -y  gdal-bin git-core libcurl4-openssl-dev \
  libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev \
  libproj-dev libssl-dev libudunits2-dev libxml2-dev libv8-dev libjq-dev \
  libprotobuf-dev protobuf-compiler make pandoc pandoc-citeproc \
  zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_github("Thinkr-open/golem")'
RUN Rscript -e 'remotes::install_github("RinteRface/bs4Dash")'
RUN Rscript -e 'remotes::install_github("r-spatial/sf")'
RUN Rscript -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_github("dbca-wa/etlTurtleNesting",upgrade="never")'
RUN Rscript -e 'remotes::install_github("dbca-wa/wastdr",upgrade="never")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');turtleviewer2::run_app()"
