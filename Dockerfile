FROM rocker/verse:4.2.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libfribidi-dev libharfbuzz-dev libicu-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.5")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.2")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.20")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.41")'
RUN Rscript -e 'remotes::install_version("promises",upgrade="never", version = "1.2.0.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.4.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("fresh",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.6")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("shiny.semantic",upgrade="never", version = "0.4.3")'
RUN Rscript -e 'remotes::install_version("tidytext",upgrade="never", version = "0.4.1")'
RUN Rscript -e 'remotes::install_version("visNetwork",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("ipc",upgrade="never", version = "0.1.4")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.27")'
RUN Rscript -e 'remotes::install_version("echarts4r",upgrade="never", version = "0.4.5")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.6")'
RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.30.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.5")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("quanteda",upgrade="never", version = "3.2.4")'
RUN Rscript -e 'remotes::install_version("quanteda.textplots",upgrade="never", version = "0.94.2")'
RUN Rscript -e 'remotes::install_version("fmsb",upgrade="never", version = "0.7.5")'
RUN Rscript -e 'remotes::install_github("lgnbhl/textyle@8bb208e52dcc6e364b6f903283c319f67d9980be")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
EXPOSE 8787
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');Hermione:::run_app()"
