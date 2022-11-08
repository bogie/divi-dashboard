library(rjson)
library(dplyr)

getDIVIapi <- function() {
    apiData <- fromJSON(file = "https://www.intensivregister.de/api/public/intensivregister",simplify = TRUE)
    apiDFs <- lapply(apiData$data, function(entry) {
        df <- data.frame(id=entry$krankenhausStandort$id,
                   desc=entry$krankenhausStandort$bezeichnung,
                   road=entry$krankenhausStandort$strasse,
                   nr=entry$krankenhausStandort$hausnummer,
                   plz=entry$krankenhausStandort$plz,
                   city=entry$krankenhausStandort$ort,
                   state=entry$krankenhausStandort$bundesland,
                   ikNr=entry$krankenhausStandort$ikNummer,
                   lat=entry$krankenhausStandort$position["latitude"],
                   lon=entry$krankenhausStandort$position["longitude"],
                   reportDate=entry$letzteMeldezeitpunkt,
                   statusLowCare=entry$maxBettenStatusEinschaetzungLowCare,
                   statusHighCare=entry$maxBettenStatusEinschaetzungHighCare,
                   statusECMO=entry$maxBettenStatusEinschaetzungEcmo
        )
        
        reportingUnits <- lapply(entry$meldebereiche, function(meldebereich) {
            uDf <- data.frame(
                unitId = meldebereich$meldebereichId,
                ardsNetwork = meldebreich$ardsNetzwerkMitglied,
                unitName = meldebereich$meldebereichBezeichnung,
                
            )
        })
        
        return(df)
    })
    apiDFs <- bind_rows(apiDFs)
    return(apiDFs)
}

raw_hospitals <- getDIVIapi()
hospitals <- raw_hospitals %>% mutate_at(c(2:8,11:15), as.character)

zips <- read.csv("zipcodes.de.csv",
                 fileEncoding = "UTF-8",
                 colClasses = c("character","character","character",
                                "character","character","character","character",
                                "character","character","numeric","numeric"))

filteredZipcodes <- zips %>% distinct(zipcode,.keep_all = TRUE)

hospitals <- hospitals %>%
    left_join(dplyr::select(filteredZipcodes,zipcode,community_code),by=c("plz"="zipcode"))

hospitals <- hospitals %>% mutate(community_code = case_when(
    plz == "19049" ~ "13004",
    plz == "59870" ~ "05958",
    plz == "95693" ~ "03355",
    plz == "99437" ~ "16071",
    TRUE ~ as.character(community_code)
)) %>%
    mutate(community_code = as.numeric(community_code)) %>%
    mutate_at(c("statusLowCare","statusHighCare","statusECMO"),
              ~factor(., levels=c("VERFUEGBAR","BEGRENZT","NICHT_VERFUEGBAR","KEINE_ANGABE"), labels = c("Verfügbar","Begrenzt","Nicht verfügbar","Keine Angabe"))
    )

saveRDS(hospitals,file="hospitals.rds")
