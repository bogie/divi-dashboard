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
choices <- setNames(gemeindeNamen$gemeinde,gemeindeNamen$Name)

divi.mtime <- file.info("divi.rds")$mtime

ui <- fixedPage(theme=shinytheme("darkly"),

    # Application title
    title="DIVI Dashboard",
    fixedRow(
        selectInput("gemeinde",h3("Gemeinde auswählen"),
                    choices = choices, selected = "5334", selectize = TRUE)
    ),
    fixedRow(
        plotlyOutput("diviAuslastung"),
        plotlyOutput("diviBetten")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
