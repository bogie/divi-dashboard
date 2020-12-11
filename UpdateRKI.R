download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","rkiData/RKI_COVID19.csv")

library(lubridate)
rkiData <- read.csv("rkiData/RKI_COVID19.csv",encoding = "UTF-8")

rkiData$Meldedatum <- ymd_hms(rkiData$Meldedatum)
rkiData$Datenstand <- ymd_hms(rkiData$Datenstand)
rkiData$Refdatum <- ymd_hms(rkiData$Refdatum)

saveRDS(rkiData,file="rkiData/rki.rds")
