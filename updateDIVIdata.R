library(lubridate)
library(rvest)
library(stringr)
library(dplyr)
library(openxlsx)


getDiviDataArchiveUrls <- function() {
    urlSeq <- seq(0,200,by=20)
    diviArchive <- "https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table&start="
    pageUrls <- paste0(diviArchive,urlSeq)
    csvUrls <- sapply(pageUrls, function(pUrl) {
        diviPage <- read_html(pUrl)
        urlList <- html_nodes(diviPage,"#table-document a")
        urlList <- lapply(urlList,function(x) { html_attr(x,"href")})
        urlList <- unlist(urlList, recursive = FALSE,use.names = FALSE)
        return(urlList)
    })
    csvUrls <- unlist(csvUrls,recursive = FALSE,use.names = FALSE)
    return(paste0("https://www.divi.de",csvUrls))
}

downloadDIVIdata <- function() {
    diviUrls <- getDiviDataArchiveUrls()
    newFiles <- lapply(diviUrls, function(x) {
        splts <- str_split(x,"/",simplify = TRUE)
        filename <- paste0("./rawData/",splts[5],".csv")
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


kreise <- read.xlsx("04-kreise.xlsx",sheet = 2, startRow = 6)
colnames(kreise) <- c("key","type","name","NUTS3","area","pop_all","pop_male","pop_female","pop_per_km2")
kreise$key <- as.numeric(kreise$key)

getLatestDIVIdata()
fileList <- list.files("./rawData")

diviData <- lapply(fileList, function(file) {
    dateString <- str_sub(file,23) %>% str_replace(.,"-2.csv","")
    fileDate <- as_datetime(dateString,format="%Y-%m-%d-%H-%M")
    csv <- read.csv(paste0("./rawData/",file))
    csv$date <- fileDate
    csv$X <- NULL
    return(csv)
})
diviData <- bind_rows(diviData)

diviData$gemeinde <- ifelse(is.na(diviData$gemeindeschluessel),diviData$kreis,diviData$gemeindeschluessel)
diviData$kreis <- NULL
diviData$gemeindeschluessel <- NULL
diviData$faelle_covid_aktuell_im_bundesland <- NULL

diviData <- diviData %>%
    mutate(gemeinde=as.numeric(gemeinde)) %>%
    left_join(kreise,by=c("gemeinde"="key"))


diviData <- diviData %>%
    mutate(
        auslastung = round(betten_belegt/(betten_frei+betten_belegt)*100,1),
        pct_covid = round(faelle_covid_aktuell/betten_belegt*100,1),
        covid_per_100k = round(faelle_covid_aktuell/(pop_all/100000)),
        covid_per_100k_intubated = round(faelle_covid_aktuell_beatmet/(pop_all/100000))
        )

gemeindeNamen <- diviData %>% dplyr::select(gemeinde, name) %>% distinct(gemeinde,.keep_all = TRUE)

saveRDS(diviData,file="divi.rds")
saveRDS(gemeindeNamen, file="gemeinden.rds")

