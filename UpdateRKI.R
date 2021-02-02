
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
