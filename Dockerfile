FROM openanalytics/r-base

LABEL maintainer "Bojan Hartmann <bhartmann@ukaachen.de>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    #libcurl4-gnutls-dev \
    libcurl4-openssl-dev \
    libv8-dev \
    libpq-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    git \
    cmake

#RUN apt-add-repository ppa:marutter/rrutter && apt-add-repository ppa:marutter/c2d4u && apt-get update

# system library dependency for the euler app
#RUN apt-get update && apt-get install -y \
#    libmpfr-dev r-cran-inline r-cran-rcpp rstan

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown','rstan'), repos='https://cloud.r-project.org/')"

# install dependencies of the dividashboard app
RUN R -e "install.packages(c('xgboost','RcppRoll','prophet','tidyverse','plotly','lubridate','rvest','stringr','openxlsx','shinythemes','jsonlite','forecast','tidymodels','modeltime','timetk','earth','rjson','promises','future','cachem','arrow','RPostgreSQL','vroom','zoo'))"

# copy the app to the image
RUN mkdir /root/diviDashboard
#RUN git clone "https://github.com/bogie/divi-dashboard.git" /root/diviDashboard
COPY app /root/diviDashboard

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/diviDashboard')"]
