library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)
library(openxlsx)

getDiviDataArchiveUrls <- function() {
    urlSeq <- seq(0,200,by=20)
    diviArchive <- "https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table&start="
    pageUrls <- paste0(diviArchive,urlSeq)
    csvUrls <- sapply(pageUrls, function(pUrl) {
        diviPage <- read_html(pUrl)
        urlList <- html_nodes(diviPage,"#table-document a")
        #return(urlList)
        urlList <- lapply(urlList,function(x) { html_attr(x,"href")})
        urlList <- unlist(urlList, recursive = FALSE,use.names = FALSE)
        return(urlList)
    })
    csvUrls <- unlist(csvUrls,recursive = FALSE,use.names = FALSE)
    return(paste0("https://www.divi.de",csvUrls))
}

downloadDIVIdata <- function() {
    diviUrls <- getDiviDataArchiveUrls()
    lapply(diviUrls, function(x) {
        splts <- str_split(x,"/",simplify = TRUE)
        filename <- paste0("./rawData/",splts[5],".csv")
        if(!file.exists(filename))
            download.file(x,filename)
        return(filename)
    })
}

downloadDIVIdata()

gemeinden <- read.xlsx("Gemeinden.xlsx")

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

diviData$gemeinde <- ifelse(str_ends(diviData$gemeinde,"000"),str_replace_all(diviData$gemeinde,"0",""),diviData$gemeinde)

diviData <- diviData %>% mutate(auslastung = round(betten_belegt/(betten_frei+betten_belegt)*100),1) %>%
    mutate(pct_covid = round(faelle_covid_aktuell_beatmet/betten_belegt*100),1)

gemeindeNamen <- diviData %>% select(gemeinde) %>% distinct(gemeinde) %>% mutate(gemeinde=as.numeric(gemeinde)) %>%
    left_join(select(gemeinden,Gemeindeschluessel,Name),by = c("gemeinde"="Gemeindeschluessel"))
gemeindeNamen$Name <- str_squish(gemeindeNamen$Name)

saveRDS(diviData,file="divi.rds")
saveRDS(gemeindeNamen, file="gemeinden.rds")