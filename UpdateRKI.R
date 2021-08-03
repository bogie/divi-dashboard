tryCatch({
    success <- download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","rkiData/RKI_COVID19.csv")
    },
    warning = function(war) {
        print(paste("Downloading file yielded warning: ",war))
    },
    error = function(err) {
        print(paste("Downloading file yielded err: ", err))
        Sys.sleep(60)
        success <- download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","rkiData/RKI_COVID19.csv")
    },
    finally = function(f) {
        print(paste("Downloading file yielded finally: ", f))
    }
)

print(paste("File downloaded finished with code: ", success))

if(success!=0) {
    print(paste("Download did not succeed, waiting 60 seconds and trying again"))
    Sys.sleep(60)
    download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","rkiData/RKI_COVID19.csv")
}

suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(vroom))
suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(stringr))

# 
# drv <- dbDriver("PostgreSQL")
# 
# con <- dbConnect(drv, dbname = "")


## RKI R values
url_rkiRvalue <- "https://raw.githubusercontent.com/robert-koch-institut/SARS-CoV-2-Nowcasting_und_-R-Schaetzung/main/Nowcast_R_aktuell.csv"
download.file(url_rkiRvalue, "rkiData/RKI_R.csv")

rkiR <- vroom::vroom("rkiData/RKI_R.csv", delim=",",
                     locale = vroom::locale(decimal_mark=","),
                     col_types = "cnnnnnnnnnnnn")
rkiR$Datum <- ymd(rkiR$Datum)
rkiR <- rkiR %>% filter(!is.na(Datum))
colnames(rkiR) <- c("Date",
                    "PredictedNew","PredictedNew_Low","PredictedNew_High",
                    "PredictedNew_Interpolated","PredictedNew_Interpolated_Low","PredictedNew_Interpolated_High",
                    #"R4d","R4d_Low","R4d_High",
                    "R7d","R7d_Low","R7d_High")

## RKI key data

url_rkiKey <- "https://opendata.arcgis.com/api/v3/datasets/c2f3c3b935a242169c6bec82e1fa573e_0/downloads/data?format=csv&spatialRefId=4326"
download.file(url_rkiKey, "rkiData/RKI_Key_Data.csv")


rkiKeyData <- vroom::vroom("rkiData/RKI_Key_Data.csv")

## RKI History
url_rkiHistory <- "https://opendata.arcgis.com/api/v3/datasets/6d78eb3b86ad4466a8e264aa2e32a2e4_0/downloads/data?format=csv&spatialRefId=4326"
download.file(url_rkiHistory, "rkiData/RKI_History.csv")

rkiHistory <- vroom::vroom("rkiData/RKI_History.csv",delim = ",",col_types = "ccncnnnnn")
rkiHistory$Datum <- ymd_hms(rkiHistory$Datum)
rkiHistory$AdmUnitId <- ifelse(str_length(rkiHistory$AdmUnitId)==4,str_c("0",rkiHistory$AdmUnitId),rkiHistory$AdmUnitId)
rkiHistory <- rkiHistory %>% rename(date=Datum,gemeinde=AdmUnitId)

rkiHistory <- rkiHistory %>% group_by(gemeinde) %>% arrange(date) %>% ungroup()

## RKI Data
#rkiData <- read.csv("rkiData/RKI_COVID19.csv",encoding = "UTF-8",colClasses = "character")
rkiData <- vroom::vroom("rkiData/RKI_COVID19.csv",col_types = "nnccccnncccnncnnnc")

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
arrow::write_feather(rkiData,"rkiData/rki.feather")
arrow::write_feather(rkiHistory, "rkiData/rkiHistory.feather")
arrow::write_feather(rkiR, "rkiData/rkiR.feather")
arrow::write_feather(rkiKeyData,"rkiData/rkiKeyData.feather")
