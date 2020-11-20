library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)

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