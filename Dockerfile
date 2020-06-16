# Install R version 3.5
FROM r-base:3.5.0

MAINTAINER Bidossessi Wilfried Hounkpe "openbiotools@gmail.com"

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev 



# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb



# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'plotly', 'ggpplot2', 'dplyr', 'DBI', 'RSQLite', 'dbplyr', 'jsonlite', 'shinycssloaders'), repos='http://cran.rstudio.com/')"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /housekeepingAtlas /srv/shiny-server/
# Mount a volume with the sqlite database
ADD db /srv/shiny-server/housekeepingAtlas/db
VOLUME /srv/shiny-server/housekeepingAtlas/db
#COPY Housekeeping.sqlite /srv/shiny-server/Housekeepingapp

# Make the ShinyApp available at port 80
EXPOSE 80

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
CMD ["/usr/bin/shiny-server.sh"]
