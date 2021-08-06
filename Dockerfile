FROM openanalytics/r-base

LABEL maintainer "Bojan Hartmann <bhartmann@ukaachen.de>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    git

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the dividashboard app
RUN R -e "install.packages(c('shiny','plotly','tidyverse','lubridate','rvest',
                             'stringr','openxlsx','shinythemes','jsonlite',
                             'forecast','tidymodels','modeltime','timetk','earth',
                             'rjson','promises','future','cachem','arrow','RPostgreSQL','vroom'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN git clone "https://github.com/bogie/divi-dashboard.git" /root/diviDashboard

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/diviDashboard')"]