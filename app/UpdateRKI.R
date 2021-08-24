tryCatch({
    success <- download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","data/rkiData/RKI_COVID19.csv")
    },
    warning = function(war) {
        print(paste("Downloading file yielded warning: ",war))
    },
    error = function(err) {
        print(paste("Downloading file yielded err: ", err))
        Sys.sleep(60)
        success <- download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","data/rkiData/RKI_COVID19.csv")
    },
    finally = function(f) {
        print(paste("Downloading file yielded finally: ", f))
    }
)

print(paste("File downloaded finished with code: ", success))

if(success!=0) {
    print(paste("Download did not succeed, waiting 60 seconds and trying again"))
    Sys.sleep(60)
    download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","data/rkiData/RKI_COVID19.csv")
}

suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(vroom))
suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(RcppRoll))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(jsonlite))

# 
# drv <- dbDriver("PostgreSQL")
# 
# con <- dbConnect(drv, dbname = "")


## RKI R values
url_rkiRvalue <- "https://raw.githubusercontent.com/robert-koch-institut/SARS-CoV-2-Nowcasting_und_-R-Schaetzung/main/Nowcast_R_aktuell.csv"
download.file(url_rkiRvalue, "data/rkiData/RKI_R.csv")

rkiR <- vroom::vroom("data/rkiData/RKI_R.csv", delim=",",
                     locale = vroom::locale(decimal_mark=","),
                     col_types = "cnnnnnnnnn")
rkiR$Datum <- ymd(rkiR$Datum)
rkiR <- rkiR %>% filter(!is.na(Datum))
colnames(rkiR) <- c("Date",
                    "PredictedNew","PredictedNew_Low","PredictedNew_High",
                    "PredictedNew_Interpolated","PredictedNew_Interpolated_Low","PredictedNew_Interpolated_High",
                    #"R4d","R4d_Low","R4d_High",
                    "R7d","R7d_Low","R7d_High")

## RKI key data

# url_rkiKey <- "https://opendata.arcgis.com/api/v3/datasets/c2f3c3b935a242169c6bec82e1fa573e_0/downloads/data?format=csv&spatialRefId=4326"
# download.file(url_rkiKey, "data/rkiData/RKI_Key_Data.csv")
# 
# 
# rkiKeyData <- vroom::vroom("data/rkiData/RKI_Key_Data.csv")

## RKI History
# url_rkiHistory <- "https://opendata.arcgis.com/api/v3/datasets/6d78eb3b86ad4466a8e264aa2e32a2e4_0/downloads/data?format=csv&spatialRefId=4326"
# download.file(url_rkiHistory, "data/rkiData/RKI_History.csv")
rkiHistory.count <- jsonlite::read_json("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/rki_history_hubv/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=standard&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=true&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=",simplifyVector = T)

rki.seq <- seq(0,rkiHistory.count$count,by=32000)

sapply(rki.seq, function(offset) {
    fname <- paste0("data/rkiData/RKI_History_",offset,".json")
    download.file(paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/rki_history_hubv/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=standard&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=",offset,"&resultRecordCount=32000&sqlFormat=none&f=pjson&token="),fname)
})

history.files <- paste0("data/rkiData/RKI_History_",rki.seq,".json")

rkiHistory.jsonList <- lapply(history.files, function(file) {
    js <- read_json(file,simplifyVector = T)
    return(js$features$attributes)
})


rkiHistory <- rkiHistory.jsonList %>% bind_rows()
rkiHistory <- rkiHistory %>% mutate(Datum = as.POSIXct(Datum/1000,origin="1970-01-01")) %>%
    mutate(Datum = as.Date(Datum))
#rkiHistory <- vroom::vroom("data/rkiData/RKI_History.csv",delim = ",",col_types = "ccncnnnnn")
#rkiHistory$Datum <- ymd_hms(rkiHistory$Datum)
rkiHistory$AdmUnitId <- ifelse(str_length(rkiHistory$AdmUnitId)==4,str_c("0",rkiHistory$AdmUnitId),rkiHistory$AdmUnitId)
rkiHistory <- rkiHistory %>% rename(date=Datum,gemeinde=AdmUnitId)

rkiHistory <- rkiHistory %>% group_by(gemeinde) %>% arrange(date) %>% ungroup()

rkiHistory <- rkiHistory %>% ungroup() %>%
    group_by(gemeinde) %>%
    arrange(gemeinde,date) %>%
    mutate(Fall7d=roll_sum(AnzFallErkrankung,n = 7,align="right",fill=NA)) %>%
    ungroup()

kreise <- read.xlsx("04-kreise.xlsx",sheet = 2, startRow = 6)
colnames(kreise) <- c("key","type","name","NUTS3","area","pop_all","pop_male","pop_female","pop_per_km2")
kreise <- kreise %>% filter(!is.na(name) & !is.na(key))

kreise <- kreise %>% summarise_at(c(5:9),sum) %>%
    mutate(key=0,type="Land",name="Deutschland",NUTS3="DE") %>%
    relocate(key,type,name,NUTS3,pop_all,pop_male,pop_female,pop_per_km2) %>%
    rbind(.,kreise)

rkiHistory <- rkiHistory %>%
    left_join(kreise,by=c("gemeinde"="key"))

rkiHistory <- rkiHistory %>%
    mutate(Incidence_7d_per_100k = Fall7d/(pop_all/100000))

## RKI Data
#rkiData <- read.csv("rkiData/RKI_COVID19.csv",encoding = "UTF-8",colClasses = "character")
rkiData <- vroom::vroom("data/rkiData/RKI_COVID19.csv",col_types = "nnccccnncccnncnnnc")

rkiData$Meldedatum <- ymd_hms(rkiData$Meldedatum)
rkiData$Datenstand <- dmy_hm(rkiData$Datenstand)
rkiData$Refdatum <- ymd_hms(rkiData$Refdatum)

rkiData[rkiData$IdLandkreis %in% c(11001:11012),]$IdLandkreis <- "11000"
rkiData[rkiData$IdLandkreis == "09473",]$IdLandkreis <- "9463"
rkiData[rkiData$IdLandkreis == "09573",]$IdLandkreis <- "9563"

rkiData <- rkiData %>%
    group_by(IdBundesland,IdLandkreis,Refdatum, Altersgruppe) %>%
    summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
    group_by(IdBundesland,IdLandkreis, Altersgruppe) %>%
    mutate(cumCases=cumsum(cases),cumDeaths=cumsum(deaths)) %>% ungroup()

# test %>%
#     filter(IdLandkreis==5334) %>% group_by(Refdatum) %>% summarise(cases=sum(cases),deaths=sum(deaths)) %>% ungroup() %>% arrange(Refdatum) %>%
#     mutate(cumCases=cumsum(cases),cumDeaths=cumsum(deaths)) %>%
#     plot_ly(type="scatter",mode="lines") %>%
#     add_trace(x=~Refdatum,
#               y=~cumCases,
#               hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
#               name="Gesamte Fälle",
#               yaxis="y",
#               line=list(color=toRGB("black"))) %>%
#     add_trace(x=~Refdatum,
#               y=~cumDeaths,
#               hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
#               name="Tote",
#               yaxis="y2",
#               line=list(color=toRGB("red"))) %>%
#     plotly::layout(
#         xaxis=list(title="Datum"),
#         yaxis=list(title="Fallzahl",side="left"),
#         yaxis2=list(title="Tote",overlaying="y",side="right"),
#         hovermode="x unified")

#saveRDS(rkiData,file="rkiData/rki.rds")
arrow::write_feather(rkiData,"data/rki.feather", compression = "uncompressed")
arrow::write_feather(rkiHistory, "data/rkiHistory.feather", compression = "uncompressed")
arrow::write_feather(rkiR, "data/rkiR.feather", compression = "uncompressed")
#arrow::write_feather(rkiKeyData,"data/rkiKeyData.feather", compression = "uncompressed")
