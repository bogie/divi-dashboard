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
choices <- setNames(gemeindeNamen$gemeinde,gemeindeNamen$Name)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DIVI Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("gemeinde",h3("Gemeinde auswählen"),
                        choices = choices, selected = "5334", selectize = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("diviAuslastung"),
           plotlyOutput("diviBetten")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    gemeinde <- reactiveVal()
    
    observeEvent(input$gemeinde, {
        gemeinde(input$gemeinde)
        #updateQueryString(paste0("?gemeinde=",input$gemeinde), mode = "replace")
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
