suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(arrow))
suppressPackageStartupMessages(library(vroom))
suppressPackageStartupMessages(library(RPostgreSQL))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(zoo))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(tidyr))

kreise <- read.xlsx("04-kreise.xlsx",sheet = 2, startRow = 6)
colnames(kreise) <- c("key","type","name","NUTS3","area","pop_all","pop_male","pop_female","pop_per_km2")
kreise <- kreise %>% filter(!is.na(name) & !is.na(key))

kreise <- kreise %>% summarise_at(c(5:9),sum) %>%
    mutate(key=0,type="Land",name="Deutschland",NUTS3="DE") %>%
    relocate(key,type,name,NUTS3,pop_all,pop_male,pop_female,pop_per_km2) %>%
    rbind(.,kreise)

## RKI R values
updateRKIRvalues <- function() {
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
    arrow::write_feather(rkiR, "data/rkiR.feather", compression = "uncompressed")
}
## RKI key data

# url_rkiKey <- "https://opendata.arcgis.com/api/v3/datasets/c2f3c3b935a242169c6bec82e1fa573e_0/downloads/data?format=csv&spatialRefId=4326"
# download.file(url_rkiKey, "data/rkiData/RKI_Key_Data.csv")
# 
# 
# rkiKeyData <- vroom::vroom("data/rkiData/RKI_Key_Data.csv")

## RKI History
# url_rkiHistory <- "https://opendata.arcgis.com/api/v3/datasets/6d78eb3b86ad4466a8e264aa2e32a2e4_0/downloads/data?format=csv&spatialRefId=4326"
# download.file(url_rkiHistory, "data/rkiData/RKI_History.csv")

updateRKIhistory <- function() {
    no_cores <- detectCores()
    clust <- makeCluster(no_cores)
    
    rkiHistory.count <- jsonlite::read_json("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/rki_history_hubv/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=standard&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=true&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=",simplifyVector = T)
    
    rki.seq <- seq(0,rkiHistory.count$count,by=32000)
    
    parSapply(clust, rki.seq, function(offset) {
        fname <- paste0("data/rkiData/RKI_History_",offset,".json")
        download.file(paste0("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/rki_history_hubv/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=standard&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=",offset,"&resultRecordCount=32000&sqlFormat=none&f=pjson&token="),fname)
    })
    
    stopCluster(clust)
    
    history.files <- paste0("data/rkiData/RKI_History_",rki.seq,".json")
    
    rkiHistory.jsonList <- lapply(history.files, function(file) {
        js <- read_json(file,simplifyVector = T)
        return(js$features$attributes)
    })
    
    
    rkiHistory <- rkiHistory.jsonList %>% bind_rows()
    rkiHistory <- rkiHistory %>% mutate(Datum = as.POSIXct(Datum/1000,origin="1970-01-01")) %>%
        mutate(Datum = as.Date(Datum))
    rkiHistory$AdmUnitId <- ifelse(str_length(rkiHistory$AdmUnitId)==4,str_c("0",rkiHistory$AdmUnitId),rkiHistory$AdmUnitId)
    rkiHistory <- rkiHistory %>% rename(date=Datum,gemeinde=AdmUnitId)
    
    rkiHistory <- rkiHistory %>% group_by(gemeinde) %>% arrange(date) %>% ungroup()
    rkiHistory <- rkiHistory %>% group_by(gemeinde) %>% mutate(FallNeu=KumFall-lag(KumFall,default=0)) %>% ungroup()
    
    rkiHistory <- rkiHistory %>% ungroup() %>%
        group_by(gemeinde) %>%
        arrange(gemeinde,date) %>%
        mutate(Fall7d=rollapplyr(FallNeu,7,sum,partial=T)) %>%
        ungroup()
    
    rkiHistory <- rkiHistory %>%
        left_join(kreise,by=c("gemeinde"="key"))
    
    rkiHistory <- rkiHistory %>%
        mutate(Incidence_7d_per_100k = Fall7d/(pop_all/100000))
    arrow::write_feather(rkiHistory, "data/rkiHistory.feather", compression = "uncompressed")
}

updateRKIdataNew <- function() {
    prefix <- "https://opendata.arcgis.com/api/v3/datasets/"
    suffix <- "/downloads/data?format=csv&spatialRefId=4326&where=1%3D1"
    ids <- c(
        "8a0b7d7c9fb442ffaa512221cf11366e_0",
        "45258e51f57d43efb612f700a876ae8f_0",
        "3949d6fd2dc74386b763e451f4c6e384_0",
        "5f81692e203a4888a64cb1976aafbd34_0",
        "f7bdcbe7188545daabe65e6c9e2a4379_0",
        "ab2c1b9c36734faf937cd83dee339517_0",
        "3ed997d4a8a447f09ab122a1a432b070_0",
        "d6c27576ee034bb78621012738615598_0",
        "14d82a9addf841789cd6ef5c1f67476a_0",
        "a99afefd4258435f8af660b6cbed9bf7_0",
        "57e385f51a07495cb0a1e00a55ee1b5b_0",
        "0e59e1262dba4f5f8d6a904113bf7c99_0",
        "3d3235c08d4f44a2afd088546b704902_0",
        "06a1c943a9b845968b5ad0607f5f48f5_0",
        "4a648483aedd49b8a6655290181d4c2a_0",
        "790f5423e03e49c4baec55a1a232c136_0"
    )
    
    sapply(ids, function(id) {
        url <- paste0(prefix,id,suffix)
        fname <- paste0("data/rkiData/RKI_state_",id,".csv")
        print(paste0("Downloading file: ",fname))
        download.file(url,fname)
        Sys.sleep(30)
    })

    rki.files <- paste0("data/rkiData/RKI_state_",ids,".csv")
    
    rkiData <- vroom::vroom(rki.files, col_types = "nccccccccnnnnnnncn")
    
    rkiData$Meldedatum <- ymd_hms(rkiData$Meldedatum)
    rkiData$Datenstand <- dmy_hm(rkiData$Datenstand)
    rkiData$Refdatum <- ymd_hms(rkiData$Refdatum)
    
    rkiData$IdLandkreis <- ifelse(str_length(rkiData$IdLandkreis)==4,str_c("0",rkiData$IdLandkreis),rkiData$IdLandkreis)
    
    rkiData[rkiData$IdLandkreis %in% c(11001:11012),]$IdLandkreis <- "11000"
    rkiData[rkiData$IdLandkreis == "09473",]$IdLandkreis <- "09463"
    rkiData[rkiData$IdLandkreis == "09573",]$IdLandkreis <- "09563"
    
    rkiData <- rkiData %>%
        group_by(IdBundesland,IdLandkreis,Refdatum, Altersgruppe) %>%
        summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
        group_by(IdBundesland,IdLandkreis, Altersgruppe) %>%
        mutate(cumCases=cumsum(cases),cumDeaths=cumsum(deaths)) %>% ungroup()
    arrow::write_feather(rkiData,"data/rki.feather", compression = "uncompressed")
    
    # Possible future code, when arcgis stops randomly failing concurrent downloads
    # no_cores <- detectCores()
    # clust <- makeCluster(no_cores)
    # clusterExport(clust, c("prefix","suffix"))
    # parSapply(clust, ids, function(id) {
    #     url <- paste0(prefix,id,suffix)
    #     fname <- paste0("data/rkiData/RKI_state_",id,".csv")
    #     download.file(url,fname)
    # })
    # 
    # stopCluster(clust)
}

## RKI Data
updateRKIdata <- function() {
    tryCatch({
        success <- download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data","data/rkiData/RKI_COVID19.csv")
    },
    warning = function(war) {
        success = 0
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
    
    if(success!=0) {
        print(paste("Download did not succeed, waiting 60 seconds and trying again"))
        return()
    }
    rkiData <- vroom::vroom("data/rkiData/RKI_COVID19.csv",col_types = "nnccccnncccnncnnnc")
    
    rkiData$Meldedatum <- ymd_hms(rkiData$Meldedatum)
    rkiData$Datenstand <- dmy_hm(rkiData$Datenstand)
    rkiData$Refdatum <- ymd_hms(rkiData$Refdatum)
    
    rkiData[rkiData$IdLandkreis %in% c(11001:11012),]$IdLandkreis <- "11000"
    rkiData[rkiData$IdLandkreis == "09473",]$IdLandkreis <- "09463"
    rkiData[rkiData$IdLandkreis == "09573",]$IdLandkreis <- "09563"
    
    rkiData <- rkiData %>%
        group_by(IdBundesland,IdLandkreis,Refdatum, Altersgruppe) %>%
        summarise(cases=sum(AnzahlFall), deaths=sum(AnzahlTodesfall)) %>%
        group_by(IdBundesland,IdLandkreis, Altersgruppe) %>%
        mutate(cumCases=cumsum(cases),cumDeaths=cumsum(deaths)) %>% ungroup()
    arrow::write_feather(rkiData,"data/rki.feather", compression = "uncompressed")
}


## RKI Vaccination data
updateRKIvaccination <- function() {
    url_vaccination <- "https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/raw/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv"
    download.file(url_vaccination,"data/rkiData/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")
    
    rkiVac <- vroom::vroom("data/rkiData/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv", col_types = "cccnn") %>%
        rename(date=Impfdatum,IdLandkreis = LandkreisId_Impfort, vcCount = Impfschutz, count = Anzahl) %>%
        mutate(date = ymd(date)) %>%
        pivot_wider(names_from="Altersgruppe",values_from="count") %>%
        mutate_at(c(4:7),~ifelse(is.na(.x),0,as.numeric(.x))) %>%
        pivot_longer(-c(date,IdLandkreis,vcCount),names_to="Altersgruppe",values_to = "count") %>%
        group_by(IdLandkreis,Altersgruppe,vcCount) %>%
        mutate(cum_count=cumsum(count)) %>%
        ungroup()
    arrow::write_feather(rkiVac, "data/rkiVac.feather", compression = "uncompressed")
}

updateRKI <- function() {
    updateRKIdataNew()
    updateRKIhistory()
    updateRKIvaccination()
    updateRKIRvalues()
}
