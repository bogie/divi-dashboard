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

if(!file.exists("divi.rds") || !file.exists("gemeinden.rds")) {
    source("./updateDIVIdata.R")
}

updatedToday <- FALSE

diviData <- readRDS("divi.rds")
gemeindeNamen <- readRDS("gemeinden.rds")
hospitals <- readRDS("hospitals.rds")
choices <- setNames(gemeindeNamen$gemeinde,gemeindeNamen$name)

divi.mtime <- file.info("divi.rds")$mtime

mapBoxToken <- paste(readLines("./mapBoxToken"), collapse="")

ui <- fixedPage(theme=shinytheme("darkly"),

    # Application title
    title="DIVI Dashboard",
    fixedRow(
        column(width = 4,selectInput("gemeinde",h3("Gemeinde auswählen"),
                    choices = choices, selected = "5334", selectize = TRUE)),
        column(width=6,
        htmlOutput("stats"))
    ),
    fixedRow(
        plotlyOutput("map")
        
    ),
    # fixedRow(
    #     dataTableOutput("hospitals")
    # ),
    fixedRow(
        plotlyOutput("diviAuslastung"),
        plotlyOutput("diviBetten"),
        plotlyOutput("diviPop")
    ),
    fixedRow(
        tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Quelle: divi.de")
    )
        # sidebarLayout(
    #     sidebarPanel(
    #         selectInput("gemeinde",h3("Gemeinde auswählen"),
    #                     choices = choices, selected = "5334", selectize = TRUE)
    #     ),
    # 
    #     mainPanel(
    #         fillRow(
    #             plotlyOutput("diviAuslastung"),
    #             plotlyOutput("diviBetten")   
    #         ),
    #        tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Quelle: divi.de")
    #     )
    # )
)

server <- function(input, output, session) {
    gemeinde <- reactiveVal()
    
    if(file.info("divi.rds")$mtime>divi.mtime) {
        print("divi.rds changed on disk, reloading")
        diviData <- readRDS("divi.rds")
        gemeindeNamen <- readRDS("gemeinden.rds")
        divi.mtime <- file.info("divi.rds")$mtime
    }
    
    observeEvent(input$gemeinde, {
        gemeinde(input$gemeinde)
    })
    
    observeEvent(gemeinde(),{
        updateQueryString(paste0("?gemeinde=",gemeinde()),mode="replace")
        updateSelectInput(session,"gemeinde",selected=gemeinde())
    })
    
    observeEvent(getQueryString(session), {
        if(length(getQueryString())>0) {
            qry <- getQueryString()
            print(qry$gemeinde)

            if(!is.na(qry$gemeinde)) {
                gemeinde(qry$gemeinde)
            }
        }
    })
    
    output$desc <- renderUI({
        tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Quelle: divi.de")
    })
    
    output$hospitals <- renderDataTable({
        hospitals %>% filter(community_code == gemeinde())
    })
    
    output$map <- renderPlotly({
        df <- subset(hospitals,community_code==gemeinde())
        center.lon <- median(df$longitude)
        center.lat <- median(df$latitude)
        
        print(paste("Center: ",center.lon, center.lat))

        fig <- df %>%
            plot_ly(
                lat = ~latitude,
                lon = ~longitude,
                mode = "markers",
                color = ~statusHighCare,
                colors = c("red","orange","green"),
                type = 'scattermapbox',
                hoverinfo="text",
                hovertext = ~paste(
                    paste0("<b>",desc,"</b>"),
                    paste0(road," ",nr,", ",plz," ",city),
                    paste("Low Care:",statusLowCare),
                    paste("High Care:",statusHighCare),
                    paste("ECMO:",statusECMO),
                    sep="<br />")) 
        fig <- fig %>%
            layout(
                mapbox = list(
                    style = 'dark',
                    zoom = 8,
                    center = list(lon = center.lon, lat = center.lat))) 
        fig <- fig %>%
            config(mapboxAccessToken = mapBoxToken)
        
        fig
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
    
    output$diviAuslastung <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde()) %>%
            pivot_longer(cols=c(betten_frei,betten_belegt), names_to="parameter",values_to="value")
        ylbl <- list(
            title="Prozent"
        )
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_lines(x=~date,y=~pct_covid,name="Anteil COVID-19",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            add_lines(x=~date,y=~auslastung,name="Auslastung",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}')) %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=ylbl)
    })
    
    output$diviBetten <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==input$gemeinde)
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_lines(x=~date, y=~faelle_covid_aktuell, name="Aktuelle COVID Fälle") %>%
            add_lines(x=~date,y=~faelle_covid_aktuell_beatmet, name="Aktuelle COVID Fälle(beatmet)") %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle"))
    })
    
    output$diviPop <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==input$gemeinde)
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_lines(x=~date, y=~covid_per_100k, name="COVID Fälle/100k Einwohner") %>%
            add_lines(x=~date, y=~covid_per_100k_intubated, name="COVID Fälle/100k Einwohner(beatmet)") %>%
            plotly::layout(xaxis=list(title="Datum"),yaxis=list(title="Fälle/100k Einwohner"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
