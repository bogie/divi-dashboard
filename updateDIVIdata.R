suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(modeltime))
suppressPackageStartupMessages(library(timetk))
suppressPackageStartupMessages(library(earth))

getDiviDataArchiveUrls <- function(start=0,end=60) {
    urlSeq <- seq(start,end,by=20)
    diviArchive <- "https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table&start="
    pageUrls <- paste0(diviArchive,urlSeq)
    csvUrls <- lapply(pageUrls, function(pUrl) {
        diviPage <- read_html(pUrl)
        urlList <- html_nodes(diviPage,"#table-document a")
        urlList <- lapply(urlList,function(x) { html_attr(x,"href")})
        urlList <- unlist(urlList, recursive = FALSE,use.names = FALSE)
        return(urlList)
    })
    csvUrls <- unlist(csvUrls,recursive = FALSE,use.names = FALSE)
    return(paste0("https://www.divi.de",csvUrls))
}

downloadDIVIdata <- function(start=0,end=60) {
    diviUrls <- getDiviDataArchiveUrls(start,end)
    newFiles <- lapply(diviUrls, function(x) {
        splts <- str_split(x,"/",simplify = TRUE)
        filename <- paste0("./rawData/",splts[7],".csv")
        if(!file.exists(filename)) {
            download.file(x,filename)
            return(filename)
        }
        return()
    })
    print(paste0("Downloaded divi data: ",length(newFiles)," new entries retrieved. Files: "))
    print(newFiles)
}

getLatestDIVIdata <- function() {
    csv <- read.csv("https://diviexchange.blob.core.windows.net/%24web/DIVI_Intensivregister_Auszug_pro_Landkreis.csv")
    fname <- ifelse(is.factor(csv$daten_stand),levels(csv$daten_stand),max(csv$daten_stand)) %>%
        as_datetime(.,format="%Y-%m-%d %H:%M:%S") %>%
        as.character.Date(.,format="%Y-%m-%d-%H-%M")
    fname <- str_c(fname,".csv")
    print(str_c("Downloaded new DIVI data with filename: ",fname))
    write.csv(csv,file=str_c("./rawData/divi-intensivregister-",fname))
}

if(!file.exists("zips.feather") | !file.exists("zips.rds")) {
    zips <- read.csv("zipcodes.de.csv",
                     fileEncoding = "UTF-8",
                     colClasses = c("character","character","character",
                                    "character","character","character","character",
                                    "character","character","numeric","numeric"))
    
    filteredZipcodes <- zips %>% distinct(zipcode,.keep_all = TRUE)
    saveRDS(filteredZipcodes,"zips.rds")
    arrow::write_feather(filteredZipcodes,"zips.feather")
}


createForecastData <- function() {
    print("Creating data for forecasting")
    diviSummed <- diviData %>% group_by(date) %>%
        summarise(sum_intub = sum(faelle_covid_aktuell_beatmet),
                  sum_covid = sum(faelle_covid_aktuell)) %>%
        ungroup() %>%
        filter(!is.na(sum_intub) & !is.na(sum_covid)) %>%
        mutate(lockdown_level = factor(
            case_when(
                date <= ymd("2020-03-23") ~ 0,
                date <= ymd("2020-05-03") ~ 3,
                date <= ymd("2020-11-02") ~ 1,
                date <= ymd("2020-12-16") ~ 2,
                date <= ymd("2021-03-04") ~ 3,
                date <= ymd("2021-04-19") ~ 2,
                TRUE ~ 3
            )
        )
        )
    return(diviSummed)
}

createForecasts <- function(df, varname) {
    splits <- df %>% time_series_split(date_var = date,assess = "4 weeks", cumulative = T)
    
    # ARIMA
    model_fit_arima_no_boost <- arima_reg() %>%
        set_engine(engine="auto_arima") %>%
        fit(as.formula(paste0(varname, "~ date + lockdown_level")), data= training(splits))
    
    model_fit_arima_boosted <- arima_boost(
        min_n = 2,
        learn_rate = 0.015
    ) %>%
        set_engine(engine = "auto_arima_xgboost") %>%
        fit(as.formula(paste0(varname, "~ date +
                as.numeric(date) +
                factor(month(date, label = TRUE), ordered = F, levels=c('Jan','Feb','Mrz','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Dez')) +
                lockdown_level")),
            data = training(splits))
    # ETS
    model_fit_ets <- exp_smoothing() %>%
        set_engine(engine = "ets") %>%
        fit(as.formula(paste0(varname, "~ date + lockdown_level")), data = training(splits))
    
    # Prophet
    model_fit_prophet <- prophet_reg() %>%
        set_engine(engine = "prophet") %>%
        fit(as.formula(paste0(varname, "~ date + lockdown_level")), data = training(splits))
    
    # LM
    model_fit_lm <- linear_reg() %>%
        set_engine("lm") %>%
        fit(as.formula(paste0(varname, "~ as.numeric(date) +
        factor(month(date, label = TRUE), ordered = FALSE, levels=c('Jan','Feb','Mrz','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Dez')) +
                              lockdown_level")),
            data = training(splits))
    
    # MARS
    model_spec_mars <- mars(mode = "regression") %>%
        set_engine("earth")
    
    recipe_spec <- recipe(as.formula(paste0(varname, "~ date + lockdown_level")), data = training(splits)) %>%
        step_date(date, features = "month", ordinal = FALSE) %>%
        step_mutate(date_num = as.numeric(date)) %>%
        step_normalize(date_num) %>%
        step_rm(date)
    
    wflw_fit_mars <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec_mars) %>%
        fit(training(splits))
    
    
    models_tbl <- modeltime_table(
        model_fit_arima_no_boost,
        model_fit_arima_boosted,
        model_fit_ets,
        model_fit_prophet,
        model_fit_lm,
        wflw_fit_mars
    )
    
    calibration_tbl <- models_tbl %>%
        modeltime_calibrate(testing(splits),quiet = FALSE)
    
    
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()
    
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(df)
    
    future_tbl <- df %>% future_frame(.length_out="2 weeks") %>% mutate(lockdown_level = factor(3))
    
    forecast_tbl <- refit_tbl %>% modeltime_forecast(actual_data=df,new_data = future_tbl)
    forecast_tbl <- forecast_tbl %>%
        left_join(df %>% select(date,lockdown_level) %>%
                      rbind(future_tbl),by=c(".index"="date"))
    
    model_id_best <- accuracy_tbl %>% filter(.model_id != 4) %>%
        filter(mase==min(mase,na.rm=T)) %>% .$.model_id
    
    out_df <- forecast_tbl %>% filter(.model_id==model_id_best) %>%
        mutate(variable=varname) %>%
        pivot_wider(names_from=variable,values_from=c(.value,.conf_lo,.conf_hi))
    return(list(data=out_df,accuracy=accuracy_tbl))
}



kreise <- read.xlsx("04-kreise.xlsx",sheet = 2, startRow = 6)
colnames(kreise) <- c("key","type","name","NUTS3","area","pop_all","pop_male","pop_female","pop_per_km2")
kreise <- kreise %>% filter(!is.na(name) & !is.na(key))


getLatestDIVIdata()
fileList <- list.files("./rawData")

diviData <- lapply(fileList, function(file) {
    dateString <- str_sub(file,23) %>% str_replace(.,"-2.csv","")
    fileDate <- as_datetime(dateString,format="%Y-%m-%d-%H-%M")
    csv <- read.csv(paste0("./rawData/",file),colClasses = "character")
    csv$date <- fileDate
    csv$X <- NULL
    return(csv)
}) %>% bind_rows(.)

diviData$gemeinde <- ifelse(is.na(diviData$gemeindeschluessel),diviData$kreis,diviData$gemeindeschluessel)
diviData$bundesland <- as.numeric(diviData$bundesland)
diviData$kreis <- NULL
diviData$gemeindeschluessel <- NULL
diviData$faelle_covid_aktuell_im_bundesland <- NULL
diviData$faelle_covid_aktuell_beatmet <- ifelse(is.na(diviData$faelle_covid_aktuell_beatmet),diviData$faelle_covid_aktuell_invasiv_beatmet,diviData$faelle_covid_aktuell_beatmet)
diviData$faelle_covid_aktuell_invasiv_beatmet <- NULL

diviData <- diviData %>%
    mutate(gemeinde = ifelse(str_length(gemeinde) == 4, str_c("0",gemeinde),gemeinde))

diviData <- diviData %>%
    mutate_at(c("anzahl_standorte","betten_frei","betten_belegt",
                "anzahl_meldebereiche","faelle_covid_aktuell","faelle_covid_aktuell_beatmet"),
              as.numeric) %>%
    # mutate(gemeinde=as.numeric(gemeinde)) %>%
    left_join(kreise,by=c("gemeinde"="key"))


diviData <- diviData %>%
    mutate(
        auslastung = round(betten_belegt/(betten_frei+betten_belegt)*100,1),
        pct_covid = round(faelle_covid_aktuell/betten_belegt*100,1),
        covid_per_100k = round(faelle_covid_aktuell/(pop_all/100000)),
        covid_per_100k_intubated = round(faelle_covid_aktuell_beatmet/(pop_all/100000))
        )

gemeindeNamen <- diviData %>% dplyr::select(gemeinde, name) %>% distinct(gemeinde,.keep_all = TRUE)

# saveRDS(diviData,file="divi.rds")
# saveRDS(gemeindeNamen, file="gemeinden.rds")

diviSummed <- createForecastData()

divi_forecast_total <- createForecasts(diviSummed,"sum_covid")
divi_forecast_intub <- createForecasts(diviSummed,"sum_intub")

diviForecast <- tibble(total=divi_forecast_total$data,intub=divi_forecast_intub$data)
accuracyTables <- tibble(total=divi_forecast_total$accuracy,intub=divi_forecast_intub$accuracy)

arrow::write_feather(diviData,"divi.feather")
arrow::write_feather(gemeindeNamen,"gemeinden.feather")
arrow::write_feather(diviForecast,"diviForecast.feather")
arrow::write_feather(accuracyTables,"diviForecastAccuracy.feather")

