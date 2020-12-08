#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(rvest)
library(stringr)
library(tidyverse)
library(openxlsx)
library(shinythemes)
library(jsonlite)

Sys.setlocale("LC_CTYPE","german")
if(!file.exists("divi.rds") || !file.exists("gemeinden.rds")) {
    source("./updateDIVIdata.R",encoding = "utf-8")
}

fix.encoding <- function(df, originalEncoding = "UTF-8") {
    numCols <- ncol(df)
    df <- data.frame(df)
    for (col in 1:numCols)
    {
        if(class(df[, col]) == "character"){
            Encoding(df[, col]) <- originalEncoding
        }
        
        if(class(df[, col]) == "factor"){
            Encoding(levels(df[, col])) <- originalEncoding
        }
    }
    return(as_data_frame(df))
}

checkFileCache <- function(fname, cacheTime = hours(1)) {
    mtime <- file.info(fname)$mtime
    
    if(!file.exists(fname) | (mtime+cacheTime)<now()) {
        print(paste("File: ",fname," is not up to date. Fetching new version"))
        return(TRUE)
    } else
        return(FALSE)
}

loadHospitalData <- function() {
    download.file("https://www.intensivregister.de/api/public/intensivregister","json_data/hospitals.json")
    hospitals <- jsonlite::fromJSON("json_data/hospitals.json",flatten = TRUE)$data
    hospitals <- hospitals %>%
        left_join(dplyr::select(filteredZipcodes,zipcode,community_code),by=c("krankenhausStandort.plz"="zipcode"))
    
    hospitals <- hospitals %>% mutate(community_code = case_when(
        krankenhausStandort.plz == "19049" ~ "13004",
        krankenhausStandort.plz == "59870" ~ "05958",
        krankenhausStandort.plz == "95693" ~ "03355",
        krankenhausStandort.plz == "99437" ~ "16071",
        TRUE ~ as.character(community_code)
    )) %>%
        mutate(community_code = as.numeric(community_code)) %>%
        mutate_at(c("bettenStatus.statusLowCare","bettenStatus.statusHighCare","bettenStatus.statusECMO"),
                  ~factor(., levels=c("VERFUEGBAR","BEGRENZT","NICHT_VERFUEGBAR","KEINE_ANGABE"), labels = c("Verfügbar","Begrenzt","Nicht verfügbar","Keine Angabe"))
        )
    return(hospitals)
}

getReportingSections <- function(hospital) {
    url <- str_c("https://www.intensivregister.de/api/public/stammdaten/krankenhausstandort/",
                 hospital,
                 "/meldebereiche")
    fname <- str_c("json_data/meldebereiche/",hospital,".json")
    mtime <- file.info(fname)$mtime
    
    if(checkFileCache(fname)) {
        download.file(url,fname)
    }
    
    json <- jsonlite::fromJSON(fname)
    json %>% dplyr::select(bezeichnung, faelleEcmoJahr,bettenPlankapazitaet)
}

filteredZipcodes <- readRDS("zips.rds")
blNames <- c("Schleswig-Holstein","Hamburg","Niedersachsen","Bremen","Nordrhein-Westfalen","Hessen","Rheinland-Pfalz","Baden-Würrtemberg","Bayern","Saarland","Berlin","Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen")

hospitals <- loadHospitalData()

diviData <- readRDS("divi.rds")
gemeindeNamen <- readRDS("gemeinden.rds")
diviData <- fix.encoding(diviData)
gemeindeNamen <- fix.encoding(gemeindeNamen)

divi.mtime <- file.info("divi.rds")$mtime
choices <- setNames(gemeindeNamen$gemeinde,gemeindeNamen$name)

mapBoxToken <- paste(readLines("./mapBoxToken"), collapse="")

ui <- navbarPage(id = "page", theme=shinytheme("darkly"),
    # Application title
    title="DIVI Dashboard",
    tabPanel(title="Gemeinden",value="gemeinde",
        fluidRow(
            column(width = 4, uiOutput("filterUI"),
            selectInput("mapStatus",h3("Farbkodierung Karte wählen"),
                                          choices = c("Low Care"="bettenStatus.statusLowCare",
                                                      "High Care"="bettenStatus.statusHighCare",
                                                      "ECMO"="bettenStatus.statusECMO"),
                                          selected = "bettenStatus.statusHighCare", selectize = TRUE),
            htmlOutput("stats")),
            column(width = 8, tableOutput("plotlyClick"))
        ),
        # fluidRow(
        #     column(width = 4, ),
        #     
        # ),
        fluidRow(
            column(width = 12, plotlyOutput("map",height = "600px"))
        ),
        fluidRow(
            column(width=12,
            plotlyOutput("diviAuslastung"),
            plotlyOutput("diviBetten"),
            plotlyOutput("diviPop"))
        ),
        fluidRow(
            column(width = 12,
            tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Quelle: divi.de"))
        )
    ),
    tabPanel(title="Deutschland",value="deutschland",
             fluidRow(
                 column(width = 4,
                    selectInput("overallMapStatus",h3("Farbkodierung Karte wählen"),
                                  choices = c("Low Care"="bettenStatus.statusLowCare",
                                              "High Care"="bettenStatus.statusHighCare",
                                              "ECMO"="bettenStatus.statusECMO"),
                                  selected = "bettenStatus.statusHighCare", selectize = TRUE),
                    htmlOutput("overallStats")
                 ),
                 column(width = 8, tableOutput("overallPlotlyClick"))
             ),
             fluidRow(
                 column(width = 12,
                 plotlyOutput("deutschlandMap", height="720px"))
             ),
             fluidRow(
                 column(width = 12,
                 plotlyOutput("overallBetten"),
                 plotlyOutput("overallAuslastung"),
                 plotlyOutput("bundeslandBetten"))
             )
    )
)

server <- function(input, output, session) {
    gemeinde <- reactiveVal(value = 5334)
    tab <- reactiveVal(value = "gemeinde")
    
    if(file.info("divi.rds")$mtime>divi.mtime) {
        print("divi.rds changed on disk, reloading")
        diviData <- readRDS("divi.rds")
        gemeindeNamen <- readRDS("gemeinden.rds")
        divi.mtime <- file.info("divi.rds")$mtime
    }

    if(checkFileCache("json_data/hospitals.json")) {
        hospitals <- loadHospitalData()
    }

    observeEvent(getQueryString(session), {
        qry <- getQueryString()
        print(qry)
        if(!is.null(qry$tab)) {
            if(!is.na(qry$tab)) {
                tab(qry$tab)
            } else
                tab("gemeinde")
        } else
            tab("gemeinde")
        
        if(!is.null(qry$gemeinde)) {
            if(is.na(qry$gemeinde))
                gemeinde(5334)
            else
                gemeinde(qry$gemeinde)
        } else {
            gemeinde(5334)
        }
    },priority = 5)
    
    observeEvent(input$gemeinde, {
        gemeinde(input$gemeinde)
    })
    
    observeEvent(input$page, {
        updateQueryString(paste0("?tab=",input$page,"&gemeinde=",gemeinde()),mode = "push",session = session)
    })
    
    observeEvent(tab(), {
        updateNavbarPage(session,inputId = "page",selected = tab())
    })
    
    observeEvent(gemeinde(),{
        updateQueryString(paste0("?tab=",tab(),"&gemeinde=",gemeinde()),mode = "push",session = session)
        updateSelectInput(session,"gemeinde",selected=gemeinde())
    })
    
    observeEvent(input$navigatedTo, {
        restore(input$navigatedTo)
    })
    
    output$filterUI <- renderUI({
        selectInput("gemeinde",h3("Gemeinde auswählen"),
                                     choices = choices, selected = isolate(gemeinde()), selectize = TRUE)
    })

    output$desc <- renderUI({
        tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Quelle: divi.de")
    })
    
    output$hospitals <- renderTable({
        df <- hospitals %>% filter(community_code == gemeinde())
        hs <- lapply(df$id, function(id) {
            sections <- getReportingSections(id) %>% bind_rows()
            sections$id <- id
            return(sections)
        }) %>% bind_rows() %>% group_by(id) %>%
            summarise(bettenPlankapazitaet = sum(bettenPlankapazitaet),faelleEcmoJahr = sum(faelleEcmoJahr))
    })
    
    output$map <- renderPlotly({
        df <- subset(hospitals,community_code==gemeinde())
        center.lon <- (max(df$krankenhausStandort.position.longitude)+min(df$krankenhausStandort.position.longitude))/2
        center.lat <- (max(df$krankenhausStandort.position.latitude)+min(df$krankenhausStandort.position.latitude))/2
        
        
        zoom_lat <- abs(abs(max(df$krankenhausStandort.position.latitude)) - abs(min(df$krankenhausStandort.position.latitude)))
        zoom_long <- abs(abs(max(df$krankenhausStandort.position.longitude)) - abs(min(df$krankenhausStandort.position.longitude)))
        
        zoom_factor <- max(zoom_lat,zoom_long)
        auto_zoom <- -1.35 * log(zoom_factor) + 8
        
        print(paste("Zoom: ",auto_zoom))
        print(paste("Center: ",center.lon, center.lat))

        fig <- df %>%
            plot_ly(
                lat = ~krankenhausStandort.position.latitude,
                lon = ~krankenhausStandort.position.longitude,
                customdata = ~krankenhausStandort.id,
                mode = "markers",
                color = df[,input$mapStatus],
                colors = c("green","orange","red","grey"),
                type = 'scattermapbox',
                hoverinfo="text",
                source = "diviMap",
                hovertext = ~paste(
                    paste0("<b>",krankenhausStandort.bezeichnung,"</b>"),
                    paste0("Address: ",krankenhausStandort.strasse," ",
                           krankenhausStandort.hausnummer,", ",
                           krankenhausStandort.plz," ",
                           krankenhausStandort.ort),
                    paste("Low Care:",bettenStatus.statusLowCare),
                    paste("High Care:",bettenStatus.statusHighCare),
                    paste("ECMO:",bettenStatus.statusECMO),
                    sep="<br />")
                )
        fig <- fig %>%
            layout(
                mapbox = list(
                    style = 'dark',
                    zoom = auto_zoom,
                    center = list(lon = center.lon, lat = center.lat)
                    )
                )
        fig <- fig %>%
            config(mapboxAccessToken = mapBoxToken)
        
        fig
    })
    
    output$deutschlandMap <- renderPlotly({
        center.lon <- median(hospitals$krankenhausStandort.position.longitude)
        center.lat <- median(hospitals$krankenhausStandort.position.latitude)
        
        print(paste("Center: ",center.lon, center.lat))
        
        fig <- hospitals %>%
            plot_ly(
                lat = ~krankenhausStandort.position.latitude,
                lon = ~krankenhausStandort.position.longitude,
                customdata = ~krankenhausStandort.id,
                mode = "markers",
                color = hospitals[,input$overallMapStatus],
                colors = c("green","orange","red","grey"),
                type = 'scattermapbox',
                hoverinfo="text",
                source = "diviMap",
                hovertext = ~paste(
                    paste0("<b>",krankenhausStandort.bezeichnung,"</b>"),
                    paste0("Address: ",krankenhausStandort.strasse," ",
                           krankenhausStandort.hausnummer,", ",
                           krankenhausStandort.plz," ",
                           krankenhausStandort.ort),
                    paste("Low Care:",bettenStatus.statusLowCare),
                    paste("High Care:",bettenStatus.statusHighCare),
                    paste("ECMO:",bettenStatus.statusECMO),
                    sep="<br />")
            )
        fig <- fig %>%
            layout(
                mapbox = list(
                    style = 'dark',
                    zoom = 5,
                    center = list(lon = center.lon, lat = center.lat))) 
        fig <- fig %>%
            config(mapboxAccessToken = mapBoxToken)
        
        fig
    })
    
    output$plotlyClick <- renderTable({
        data <- event_data("plotly_click",source = "diviMap")
        
        if(!is.null(data)) {
            id <- data$customdata
            json <- getReportingSections(id) %>%
                bind_rows() %>%
                dplyr::select(bezeichnung, faelleEcmoJahr,bettenPlankapazitaet)
            rbind(json,
                  summarise(json, bezeichnung="Summe",
                            faelleEcmoJahr = sum(faelleEcmoJahr),
                            bettenPlankapazitaet = sum(bettenPlankapazitaet))) %>%
                rename(Bereich=bezeichnung, `ECMO Fälle/Jahr`=faelleEcmoJahr, `Bettenplankapazität`=bettenPlankapazitaet)
        }
    })
    
    output$overallPlotlyClick <- renderTable({
        data <- event_data("plotly_click",source = "diviMap")
        
        if(!is.null(data)) {
            id <- data$customdata
            json <- getReportingSections(id) %>%
                bind_rows() %>%
                dplyr::select(bezeichnung, faelleEcmoJahr,bettenPlankapazitaet)
            rbind(json,
                  summarise(json, bezeichnung="Summe",
                            faelleEcmoJahr = sum(faelleEcmoJahr),
                            bettenPlankapazitaet = sum(bettenPlankapazitaet))) %>%
                rename(Bereich=bezeichnung, `ECMO Fälle/Jahr`=faelleEcmoJahr, `Bettenplankapazität`=bettenPlankapazitaet)
        }
    })
    
    output$stats <- renderUI({
        krStats <- diviData %>% filter(date==max(date) & gemeinde==gemeinde())
        strName <- paste("Name:",krStats$name)
        strType <- paste("Bezeichnung:",krStats$type)
        strArea <- paste0("Fläche: ",format(krStats$area,big.mark = ".",decimal.mark = ",",trim=TRUE),"km²")
        strPop <- paste("Population:",format(krStats$pop_all,big.mark = ".",decimal.mark=",",trim = TRUE))
        strStd <- paste("Standorte:",krStats$anzahl_standorte)
        strBetten <- paste0("Betten(frei/gesamt): ",krStats$betten_frei,"/",krStats$betten_frei+krStats$betten_belegt)
        strDate <- paste("Aktualisiert:",krStats$daten_stand)
        HTML(paste(strName,strType,strArea,strPop,strStd,strBetten,strDate,sep="<br />"))
    })
    
    output$overallStats <- renderUI({
        krStats <- diviData %>% group_by(date) %>% summarise(sum_area = sum(area),
                                          sum_pop = sum(pop_all),
                                          sum_standorte = sum(anzahl_standorte),
                                          sum_free_beds = sum(betten_frei),
                                          sum_occup_beds = sum(betten_belegt))
        HTML(
            with(subset(krStats, date==max(date)),paste(
                paste0("Deutschland"),
                paste0("Fläche: ", sum_area,"km²"),
                paste0("Einwohner: ", sum_pop),
                paste0("Standorte: ",sum_standorte),
                paste0("Betten: ",sum_free_beds,"/",sum_free_beds+sum_occup_beds),
                sep="<br />"
            ))
        )
    })
    
    output$overallBetten <- renderPlotly({
        dt <- diviData %>% group_by(date) %>% summarise(sum_area = sum(area),
                                                        sum_pop = sum(pop_all),
                                                        sum_standorte = sum(anzahl_standorte),
                                                        sum_free_beds = sum(betten_frei),
                                                        sum_occup_beds = sum(betten_belegt),
                                                        sum_faelle_covid_aktuell = sum(faelle_covid_aktuell),
                                                        sum_faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet))
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date, y=~sum_faelle_covid_aktuell, name="Aktuelle COVID Fälle") %>%
            add_trace(x=~date,y=~sum_faelle_covid_aktuell_beatmet, name="Aktuelle COVID Fälle(beatmet)") %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle"), hovermode="x unified")
    })
    
    output$bundeslandBetten <- renderPlotly({
        dt <- diviData %>% filter(!is.na(bundesland)) %>%
            group_by(date,bundesland) %>% summarise(sum_area = sum(area),
                                                        sum_pop = sum(pop_all),
                                                        sum_standorte = sum(anzahl_standorte),
                                                        sum_free_beds = sum(betten_frei),
                                                        sum_occup_beds = sum(betten_belegt),
                                                        sum_faelle_covid_aktuell = sum(faelle_covid_aktuell),
                                                        sum_faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet))
        plot_ly(dt,
                x=~date,
                y=~sum_faelle_covid_aktuell,
                color=~as.factor(bundesland),
                name=~blNames[bundesland]) %>%
            group_by(bundesland) %>%
            add_lines() %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle"))
    })
    
    
    output$overallAuslastung <- renderPlotly({
        dt <- diviData %>% group_by(date) %>% summarise(sum_betten_frei = sum(betten_frei),
                                                        sum_betten_belegt = sum(betten_belegt),
                                                        sum_faelle_covid_aktuell = sum(faelle_covid_aktuell),
                                                        pct_covid = round(sum_faelle_covid_aktuell/sum_betten_belegt*100,1),
                                                        auslastung = round(sum_betten_belegt/(sum_betten_frei+sum_betten_belegt)*100,1))
        ylbl <- list(
            title="Prozent"
        )
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date,y=~pct_covid,name="Anteil COVID-19",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            add_trace(x=~date,y=~auslastung,name="Auslastung",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=ylbl, hovermode="x unified")
    })
    
    output$diviAuslastung <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde()) %>%
            pivot_longer(cols=c(betten_frei,betten_belegt), names_to="parameter",values_to="value")
        ylbl <- list(
            title="Prozent"
        )
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date,y=~pct_covid,name="Anteil COVID-19",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            add_trace(x=~date,y=~auslastung,name="Auslastung",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=ylbl, hovermode="x unified")
    })
    
    output$diviBetten <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde())
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date, y=~faelle_covid_aktuell, name="Aktuelle COVID Fälle") %>%
            add_trace(x=~date,y=~faelle_covid_aktuell_beatmet, name="Aktuelle COVID Fälle(beatmet)") %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle"), hovermode="x unified")
    })
    
    output$diviPop <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde())
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date, y=~covid_per_100k, name="COVID Fälle/100k Einwohner") %>%
            add_trace(x=~date, y=~covid_per_100k_intubated, name="COVID Fälle/100k Einwohner(beatmet)") %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle/100k Einwohner"), hovermode="x unified")
    })
}

# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")
