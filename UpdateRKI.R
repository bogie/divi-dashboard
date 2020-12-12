download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","rkiData/RKI_COVID19.csv")

library(lubridate)
rkiData <- read.csv("rkiData/RKI_COVID19.csv",encoding = "UTF-8")

rkiData$Meldedatum <- ymd_hms(rkiData$Meldedatum)
rkiData$Datenstand <- dmy_hm(rkiData$Datenstand)
rkiData$Refdatum <- ymd_hms(rkiData$Refdatum)

rkiData[rkiData$IdLandkreis %in% c(11001:11012),]$IdLandkreis <- 11000
rkiData[rkiData$IdLandkreis == 9473,]$IdLandkreis <- 9463
rkiData[rkiData$IdLandkreis == 9573,]$IdLandkreis <- 9563

saveRDS(rkiData,file="rkiData/rki.rds")
