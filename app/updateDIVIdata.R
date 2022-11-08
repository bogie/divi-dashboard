source("diviFunctions.R")

# Load ZipCodes

if(!file.exists("data/zips.feather")) {
    zips <- read.csv("zipcodes.de.csv",
                     fileEncoding = "UTF-8",
                     colClasses = c("character","character","character",
                                    "character","character","character","character",
                                    "character","character","numeric","numeric"))
    
    filteredZipcodes <- zips %>% distinct(zipcode,.keep_all = TRUE)
    arrow::write_feather(filteredZipcodes,"data/zips.feather", compression = "uncompressed")
}

kreise <- read.xlsx("04-kreise.xlsx",sheet = 2, startRow = 6)
colnames(kreise) <- c("key","type","name","NUTS3","area","pop_all","pop_male","pop_female","pop_per_km2")
kreise <- kreise %>% filter(!is.na(name) & !is.na(key))

# Divi Altersverteilung

divi_age_url <- "https://diviexchange.blob.core.windows.net/%24web/bund-covid-altersstruktur-zeitreihe_ab-2021-04-29.csv"

# Download data from 
date <- getLatestDIVIdata()

print(str_c("Downloaded new DIVI data with date: ",str_remove(date,".csv")))

fileList <- list.files("./data/rawData")

diviData <- lapply(fileList, function(file) {
    dateString <- str_sub(file,23) %>% str_replace(.,"-2.csv","")
    fileDate <- as_datetime(dateString,format="%Y-%m-%d-%H-%M")
    csv <- read.csv(paste0("./data/rawData/",file),colClasses = "character")
    csv$date <- fileDate
    csv$X <- NULL
    return(csv)
}) %>% bind_rows(.)

diviData$gemeindeschluessel <- ifelse(
    str_length(diviData$gemeindeschluessel) == 4 & !str_starts(diviData$gemeindeschluessel,"0"),
    str_c("0",diviData$gemeindeschluessel),
    diviData$gemeindeschluessel)
diviData$gemeinde <- ifelse(is.na(diviData$gemeindeschluessel),diviData$kreis,diviData$gemeindeschluessel)
diviData$bundesland <- as.numeric(diviData$bundesland)
diviData$kreis <- NULL
diviData$gemeindeschluessel <- NULL
diviData$faelle_covid_aktuell_im_bundesland <- NULL
diviData$faelle_covid_aktuell_beatmet <- ifelse(is.na(diviData$faelle_covid_aktuell_beatmet),diviData$faelle_covid_aktuell_invasiv_beatmet,diviData$faelle_covid_aktuell_beatmet)
diviData$faelle_covid_aktuell_invasiv_beatmet <- NULL

diviData <- diviData %>%
    mutate_at(c("anzahl_standorte","betten_frei","betten_belegt",
                "anzahl_meldebereiche","faelle_covid_aktuell","faelle_covid_aktuell_beatmet"),
              as.numeric) %>%
    mutate(date=as.Date(date)) %>%
    left_join(kreise,by=c("gemeinde"="key"))


diviData <- diviData %>%
    mutate(
        auslastung = round(betten_belegt/(betten_frei+betten_belegt)*100,1),
        pct_covid = round(faelle_covid_aktuell/betten_belegt*100,1),
        covid_per_100k = round(faelle_covid_aktuell/(pop_all/100000)),
        covid_per_100k_intubated = round(faelle_covid_aktuell_beatmet/(pop_all/100000))
    )

rkiHistory <- arrow::read_feather("data/rkiHistory.feather")

diviData <- diviData %>%
    left_join(select(rkiHistory,gemeinde,BundeslandId,date,AnzFallNeu,AnzFallVortag,AnzFallErkrankung,AnzFallMeldung,KumFall,Fall7d,Incidence_7d_per_100k),by=c("date","gemeinde"))

gemeindeNamen <- diviData %>% dplyr::select(gemeinde, name) %>% distinct(gemeinde,.keep_all = TRUE)

diviSummed <- createForecastData()

divi_forecast_total <- createForecasts(diviSummed,"sum_covid")
divi_forecast_intub <- createForecasts(diviSummed,"sum_intub")

diviForecast <- tibble(total=divi_forecast_total$data,intub=divi_forecast_intub$data)
accuracyTables <- tibble(total=divi_forecast_total$accuracy,intub=divi_forecast_intub$accuracy)

## Geojson
kreise.geojson <- rjson::fromJSON(file = "Kreisgrenzen_2017_mit_Einwohnerzahl.geojson")

kreise.features <- lapply(kreise.geojson$features, function(feature) {
    props <- diviData %>% filter(gemeinde==feature$properties$RS & date==max(date)) %>%
        select(anzahl_standorte,betten_frei,betten_belegt,anzahl_meldebereiche, faelle_covid_aktuell, faelle_covid_aktuell_beatmet,
               betten_belegt_nur_erwachsen,betten_frei_nur_erwachsen) %>% append(feature$properties,.)
    feature$properties <- props
    return(feature)
})

kreise.geojson$features <- kreise.features
jsonlite::write_json(kreise.geojson,path = "data/json_data/divi.geojson")

arrow::write_feather(diviData,"data/divi.feather", compression = "uncompressed")
arrow::write_feather(gemeindeNamen,"data/gemeinden.feather", compression = "uncompressed")
arrow::write_feather(diviForecast,"data/diviForecast.feather", compression = "uncompressed")
arrow::write_feather(accuracyTables,"data/diviForecastAccuracy.feather", compression = "uncompressed")

