# COVID-19 Dashboard

During the early pandemic there was a lack of COVID-19 related dashboards that unified German COVID-19 datasources.
As such, this dashboard uses data from the [German Interdisciplinary Association for Intensive Care and Emergency Medicine (DIVI)](https://www.divi.de/) and the [Robert Koch Institute (RKI)](https://www.rki.de/EN/Home/homepage_node.html).

This dashboard is available at [shiny.bawki.de/covid](https://shiny.bawki.de/covid/)

# Open data sources

### Intensive care registry

- JSON API with real time hospital data from `https://www.intensivregister.de/api/public/intensivregister`
- Daily DIVI report as csv from `https://diviexchange.blob.core.windows.net/%24web/DIVI_Intensivregister_Auszug_pro_Landkreis.csv`
### RKI
- RKI report from `https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data`
- Historical RKI data from the arcgis REST API `https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/rki_history_hubv/FeatureServer/0/query`


# Installation

## Build the docker image

1. Download this repository
`git clone git@github.com:bogie/divi-dashboard.git`
2. Generate a Mapbox Token from [mapbox.com](https://docs.mapbox.com/help/glossary/access-token/) and save to
```
app/mapBoxToken
```
3. Build the docker image, make sure to substitute `<image name>` with your desired name for the image, I personally use `bawki/divi-dashboard`
```
sudo docker build -t <image name> .
```

## Configuration

This dashboard uses a cron service to download new data at

- 04:00 GMT+1 from RKI sources
- 14:00 GMT+1 from DIVI sources

These times are chosen, because new data will be made available at ~ 03:00 and 11:30 GMT+1 respectively, sometimes uploads will be 1-2 hours delayed, therefore I use a generous buffer.

In order for data updates to work, you will need to setup a data folder on your device, which will be mounted/bound by the docker container.

1. Create the data directories
```
mkdir -p /srv/shiny/dashboard/data/rawData
mkdir -p /srv/shiny/dashboard/data/json_data
mkdir -p /srv/shiny/dashboard/data/rkiData
```
2. Copy or link the update scripts from the git folder
```
cd /srv/shiny/dashboard/
ln -s /path/to/app/updateRKI.R
ln -s /path/to/app/updateDIVIdata.R
```
3. make sure the folders are owned by the user, which will run the R scripts and has read/write access to the data folder
4. create cron jobs, run `crontab -e` as the aforementioned user
```
# Optional, to get notified each time any of the jobs are running
MAILTO=your@email.tld
0 14 * * * cd /srv/shiny/dashboard/data && Rscript updateDIVIdata.R
0 4 * * * cd /srv/shiny/dashboard/data && Rscript updateRKI.R
```



## shinyproxy.io

This docker image is used in conjunction with [shinyproxy.io](https://shinyproxy.io/), make sure you have a working instance and use the following entry in your `application.yml`.

```
specs:
  - id: dashboard
    display-name: divi dashboard
    description: Dashboard of RKI and DIVI data
    container-cmd: ["R","-e","shiny::runApp('/root/diviDashboard')"]
    container-image: <image name>
    container-volumes: ["/srv/shiny/dashboard/data:/root/diviDashboard/data"]
    user: "<UID>:<GID>"
    max-lifetime: -1
    heartbeat-timeout: -1
```

Make sure you substitute `<UID>` and `<GID>` as the data user ids, and `<image name>` with the build name you used earlier to build the docker image.
