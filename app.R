loadPackages <- function(package = NULL, packageList = NULL, silent = TRUE) {
    inst_pkgs <- installed.packages()
    if(!is.null(packageList)) {
        ret <- lapply(packageList, function(pkg) {
            if(!pkg %in% inst_pkgs){
                print(paste("Installing package: ",pkg))
                install.packages(pkg)
            }
            
            if(silent)
                suppressPackageStartupMessages(require(pkg,character.only = TRUE, quietly = T))
            else
                require(pkg, character.only=TRUE)
        })
    } else if(!is.null(package)) {
        if(!package %in% inst_pkgs) {
            print(paste("Installing package: ", package))
            install.packages(pkg)
        }
        if(silent)
            suppressPackageStartupMessages(require(package,character.only = TRUE, quietly = T))
        else
            require(package, character.only=TRUE)
    }
}

loadPackages(packageList = c("shiny","plotly","tidyverse","lubridate","rvest",
                             "stringr","openxlsx","shinythemes","jsonlite",
                             "forecast","tidymodels","modeltime","timetk","earth",
                             "rjson","promises","future","cachem"))

plan(multisession)

options(Ncpus = 6,encoding = "UTF-8")
shinyOptions(cache = cachem::cache_mem(max_size=1000*1024^2))

Sys.setlocale("LC_CTYPE","german")
if(!file.exists("divi.feather") || !file.exists("gemeinden.feather") ||
   !file.exists("diviForecast.feather") || !file.exists("diviForecastAccuracy.feather")) {
    source("./updateDIVIdata.R",encoding = "UTF-8")
}

if(!file.exists("rkiData/rki.feather")) {
    source("./UpdateRKI.R", encoding = "UTF-8")
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
    if(checkFileCache("json_data/hospitals.json",hours(2))) {
        download.file("https://www.intensivregister.de/api/public/intensivregister","json_data/hospitals.json")
    }
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
        mutate_at(c("maxBettenStatusEinschaetzungLowCare","maxBettenStatusEinschaetzungHighCare","maxBettenStatusEinschaetzungEcmo"),
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
    json %>% dplyr::select(bezeichnung)
}

#filteredZipcodes <- readRDS("zips.rds")
filteredZipcodes <- arrow::read_feather("zips.feather")
blNames <- c("Schleswig-Holstein",
             "Hamburg",
             "Niedersachsen",
             "Bremen",
             "Nordrhein-Westfalen",
             "Hessen",
             "Rheinland-Pfalz",
             "Baden-Würrtemberg",
             "Bayern",
             "Saarland",
             "Berlin",
             "Brandenburg",
             "Mecklenburg-Vorpommern",
             "Sachsen",
             "Sachsen-Anhalt",
             "Thüringen")

hospitals <- loadHospitalData()

# diviData <- readRDS("divi.rds")
# rkiData <- readRDS("rkiData/rki.rds")
# gemeindeNamen <- readRDS("gemeinden.rds")
# diviData <- fix.encoding(diviData)
# gemeindeNamen <- fix.encoding(gemeindeNamen)

diviData <- arrow::read_feather("divi.feather")
diviForecast <- arrow::read_feather("diviForecast.feather")
diviForecastAccuracy <- arrow::read_feather("diviForecastAccuracy.feather")
gemeindeNamen <- arrow::read_feather("gemeinden.feather")
rkiData <- arrow::read_feather("rkiData/rki.feather")


rki.mtime <- file.info("rkiData/rki.feather")$mtime
divi.mtime <- file.info("divi.feather")$mtime
diviForecast.mtime <- file.info("diviForecast.feather")$mtime
diviForecastAccuracy.mtime <- file.info("diviForecastAccuracy.feather")$mtime
choices <- setNames(gemeindeNamen$gemeinde,gemeindeNamen$name)

missing_county <- tibble(bundesland=c(7,9,9,9),
                  name=c("Rhein-Pfalz-Kreis","Landkreis neustadt an der Waldnaab", "Landkreis Coburg","Landkreis Fürth"),
                  gemeinde=c("07338","09374","09473","09573"),
                  anzahl_standorte=0,
                  anzahl_meldebereiche=0,
                  type="Kreis")

mapBoxToken <- paste(readLines("./mapBoxToken"), collapse="")

plotChoreo <- function() {
    today <- diviData %>% filter(date == max(date,na.rm = T))
    # geojson <- rjson::fromJSON(file="https://github.com/isellsoap/deutschlandGeoJSON/raw/master/4_kreise/2_hoch.geo.json")
    # geojson <- rjson::fromJSON(file="https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson")
    
    #geojson2 <- rjson::fromJSON(file="./rkiData/geojson.json")
    
    plot_ly() %>%
        add_trace(type="choroplethmapbox",
                  geojson=geojson,
                  name="",
                  locations = today$gemeinde,
                  featureidkey = "properties.RS",
                  z = today$auslastung,
                  colorscale = "Bluered",
                  hovertext = ~paste(
                      paste0("<b>",today$name,"</b>"),
                      paste0("Kliniken: ", today$anzahl_standorte),
                      paste0("Betten(frei): ",today$betten_frei),
                      paste0("Betten(belegt): ",today$betten_belegt),
                      paste0("Auslastung(%): ",today$auslastung),
                      paste0("Anteil COVID(%): ",today$pct_covid),
                      sep="<br />")) %>%
        layout(mapbox = list(style="carto-positron",
                             zoom=6,
                             center = list(lon=10.437657,lat=50.9384167))
        )
}

ui <- navbarPage(id = "page", theme=shinytheme("darkly"),
    # Application title
    title="COVID-19 Dashboard",
    tags$head(
        tags$meta(name="author",content="Bojan Hartmann"),
        tags$meta(name="keywords", content="COVID-19,DIVI,RKI,SARS-CoV-2,Dashboard"),
        tags$meta(name="description",content="Inoffizielles COVID-19 Dashboard basierend auf DIVI und RKI Daten"),
        tags$meta(property="og:title",content="COVID-19 Dashboard - shiny.bawki.de"),
        tags$meta(property="og:type", content="website"),
        tags$meta(property="og:url",content="https://shiny.bawki.de/"),
        tags$meta(property="og:locale",content="de_DE"),
        tags$meta(property="og:description",content="Inoffizielles COVID-19 Dashboard basierend auf DIVI und RKI Daten")
    ),
    tabPanel(title="Gemeinden",value="gemeinde",
        fluidRow(
            column(width = 4, uiOutput("filterUI"),
            selectInput("mapStatus",h3("Farbkodierung Karte wählen"),
                                          choices = c("Low Care"="maxBettenStatusEinschaetzungLowCare",
                                                      "High Care"="maxBettenStatusEinschaetzungHighCare",
                                                      "ECMO"="maxBettenStatusEinschaetzungEcmo"),
                                          selected = "maxBettenStatusEinschaetzungHighCare", selectize = TRUE),
            htmlOutput("stats")),
            column(width = 8, 
                   #tableOutput("plotlyClick")
                   uiOutput("hospitalDetailUI")
                   )
        ),
        # fluidRow(
        #     column(width = 4, ),
        #     
        # ),
        fluidRow(
            column(width = 8, plotlyOutput("map",height = "720px")),
            column(width = 4, uiOutput("info"))
        ),
        fluidRow(
            column(width=12,
                   plotlyOutput("rkiAgePlot"),
                   plotlyOutput("rkiPlot"),
                   plotlyOutput("diviAuslastung"),
                   #plotlyOutput("diviBetten"),
                   #plotlyOutput("diviPop")
            )
        ),
        fluidRow(
            column(width = 12,
            uiOutput("desc"))
        )
    ),
    tabPanel(title="Deutschland",value="deutschland",
             fluidRow(
                 column(width = 4,
                    selectInput("overallMapStatus",h3("Farbkodierung Karte wählen"),
                                  choices = c("Low Care"="maxBettenStatusEinschaetzungLowCare",
                                              "High Care"="maxBettenStatusEinschaetzungHighCare",
                                              "ECMO"="maxBettenStatusEinschaetzungEcmo"),
                                  selected = "maxBettenStatusEinschaetzungHighCare", selectize = TRUE),
                    htmlOutput("overallStats")
                 ),
                 column(width = 8, 
                        uiOutput("overallHospitalDetailUI")
                        )
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
    ),
    tabPanel(title="Map", value="map",
                        plotlyOutput("choropleth",height = "1080")
             ),
    tabPanel(title="Impressum",value="impressum",
             fluidRow(
                 column(width=12,
                 uiOutput("impressum")
                 )
             )
    )
)

server <- function(input, output, session) {
    gemeinde <- reactiveVal(value = "05334")
    tab <- reactiveVal(value = "gemeinde")
    geojson <- reactive({
                jsonlite::fromJSON("./rkiData/geojson.json",simplifyDataFrame = F)
                }) %>%
                shiny::bindCache(diviData %>% summarise(max(date,na.rm=T)))
    today <- reactive({
                diviData %>% filter(date == max(date,na.rm = T)) 
            }) %>%
            shiny::bindCache(diviData %>% summarise(max(date,na.rm=T)))
    
    mc <- reactive({ missing_county }) %>%
        shiny::bindCache(diviData %>% summarise(max(date,na.rm=T)))
    
    if(file.info("divi.feather")$mtime>divi.mtime) {
        print("divi.feather changed on disk, reloading")
        diviData <- arrow::read_feather("divi.feather")
        gemeindeNamen <- arrow::read_feather("gemeinden.feather")
        divi.mtime <- file.info("divi.feather")$mtime
    }
    
    if(file.info("diviForecast.feather")$mtime>diviForecast.mtime) {
        print("diviForecast.feather changed on disk, reloading")
        diviForecast <- arrow::read_feather("diviForecast.feather")
        diviForecast.mtime <- file.info("diviForecast.feather")$mtime
    }
    
    if(file.info("diviForecastAccuracy.feather")$mtime>diviForecastAccuracy.mtime) {
        print("diviForecastAccuracy.feather changed on disk, reloading")
        diviForecastAccuracy <- arrow::read_feather("diviForecastAccuracy.feather")
        diviForecastAccuracy.mtime <- file.info("diviForecastAccuracy.feather")$mtime
    }
    
    if(file.info("rkiData/rki.feather")$mtime>rki.mtime) {
        print("divi.feather changed on disk, reloading")
        rkiData <- arrow::read_feather("rkiData/rki.feather")
        rki.mtime <- file.info("rkiData/rki.feather")$mtime
    }

    if(checkFileCache("json_data/hospitals.json")) {
        hospitals <- loadHospitalData()
    }
    
    observeEvent(getQueryString(session), {
        qry <- getQueryString()
        if(!is.null(qry$tab)) {
            if(!is.na(qry$tab)) {
                tab(qry$tab)
            } else
                tab("gemeinde")
        } else
            tab("gemeinde")
        if(tab() == "gemeinde") {
            if(!is.null(qry$gemeinde)) {
                if(is.na(qry$gemeinde))
                    gemeinde("05334")
                else
                    gemeinde(qry$gemeinde)
            } else {
                gemeinde("05334")
            }
        } else {
            
        }
    },priority = 5)
    
    observeEvent(input$gemeinde, {
        gemeinde(input$gemeinde)
    })
    
    observeEvent(input$page, {
        if(input$page == "gemeinde")
            updateQueryString(paste0("?tab=",input$page,"&gemeinde=",gemeinde()),mode = "push",session = session)
        else if(input$page %in% c("deutschland","impressum","forecasts","map")) {
            updateQueryString(paste0("?tab=",input$page),mode="push",session=session)
        }
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
    
    output$choropleth <- renderPlotly({
        #withProgress(message = "Loading Map",value = 0,{
        p <- Progress$new()
        p$set(value = NULL, message = "Loading data...")
        
        td <- today()
        gj <- geojson()
        mic <- mc()
        
        tx <- paste(
            paste0("<b>",td$name,"</b>"),
            paste0("Kliniken: ", td$anzahl_standorte),
            paste0("Betten(frei): ",td$betten_frei),
            paste0("Betten(belegt): ",td$betten_belegt),
            paste0("Auslastung(%): ",td$auslastung),
            paste0("Anteil COVID(%): ",td$pct_covid),
            sep="<br />")
        
        tx2 <- paste(
            paste0("<b>",mic$name,"</b>"),
            paste0("Kliniken: ", mic$anzahl_standorte),
            sep="<br />")
        
        p$set(message = "Generating plot...")
        future_promise({
            plot_ly() %>%
            add_trace(type="choroplethmapbox",
                      geojson=gj,
                      name="Auslastung",
                      locations = td$gemeinde,
                      featureidkey = "properties.RS",
                      z = td$auslastung,
                      colorscale = "Bluered",
                      text = tx,
                      hovertemplate = "%{text}<extra></extra>"
                      ) %>%
                add_trace(type="choroplethmapbox",
                          geojson=gj,
                          name="Missing",
                          locations=mic$gemeinde,
                          featureidkey = "properties.RS",
                          color="darkgrey",
                          z=0,
                          showscale=F,
                          text=tx2,
                          hovertemplate= "%{text}<extra></extra>") %>%
            layout(mapbox = list(style="carto-positron",
                                 zoom=6,
                                 center = list(lon=10.437657,lat=50.9384167)
                                 )
            )
        })  %>%
            finally(~p$close())
            #incProgress(0.5,detail="Loading complete")
            #p
        #})
    })
    
    output$hospitalDetailUI <- renderUI({
        data <- event_data("plotly_click",source = "diviMap")
        
        if(!is.null(data)) {
            tableOutput("plotlyClick")
        } else {
            tags$text("Klicken Sie auf ein Krankenhaus auf der Karte für eine Detailansicht.")
        }
    })
    
    output$overallHospitalDetailUI <- renderUI({
        data <- event_data("plotly_click",source = "deutschlandMap")
        
        if(!is.null(data)) {
            tableOutput("overallPlotlyClick")
        } else {
            tags$text("Klicken Sie auf ein Krankenhaus auf der Karte für eine Detailansicht.")
        }
    })

    output$desc <- renderUI({
        tagList(
            tags$text("Quellen: "),
            tags$br(),
            tags$a(href="https://www.divi.de/divi-intensivregister-tagesreport-archiv-csv?layout=table","Deutsche Interdisziplinäre Vereinigung für Intensiv- und Notfallmedizin"),
            tags$br(),
            tags$a(href="https://www.rki.de/DE/Home/homepage_node.html","Robert Koch-Institut"),
            tags$br(),
            tags$a(href="https://www.bkg.bund.de/DE/Home/home.html","Bundesamt für Kartographie und Geodäsie")
        )
    })
    
    output$info <- renderUI({
        tagList(
            tags$text("Dies ist ein inoffizielles Dashboard das die Daten der DIVI und des RKI kombiniert anzeigt."),
            tags$br(),
            tags$ul(),
            tags$li("Die Darstellung der DIVI Daten basiert auf dem Meldedatum, die Fallzahlen werden jeden Tag gegen 13:30 Uhr aktualisiert, das Auslastungsbarometer stündlich."),
            tags$li("Im Gegensatz dazu wird bei der Darstellung der RKI Daten das Referenzdatum verwendet, dabei handelt es sich sofern bekannt um den Krankheitsbeginn, ansonsten das Meldedatum."),
            tags$li("Die Daten des RKI werden nachts gegen 03:30 Uhr aktualisiert."),
            tags$ul(),
            tags$text("Das Auslastungsbarometer ist eine subjektive Einschätzung der Meldenden Stationen, das anhand des Arbeitsaufkommens und Personalverfügbarkeit zwischen, verfügbar, begrenzt und nicht verfügbar unterscheidet")
        )
    })
    
    output$impressum <- renderUI({
        tagList(
            tags$h1("Impressum"),
            tags$br(),
            tags$h2("Inhaltlich verantwortlich"),
            tags$br(),
            tags$text("Bojan Hartmann"),
            tags$br(),
            tags$a(href="mailto:bogie+dashboard@bawki.de","Mail"),
            tags$br(),
            tags$a(href="https://github.com/bogie/divi-dashboard","Source code")
        )
    })
    
    output$hospitals <- renderTable({
        df <- hospitals %>% filter(community_code == gemeinde())
        hs <- lapply(df$id, function(id) {
            sections <- getReportingSections(id) %>% bind_rows()
            sections$id <- id
            return(sections)
        }) %>% bind_rows()
    })
    
    output$map <- renderPlotly({
        p <- Progress$new()
        p$set(value = NULL, message = "Loading data...")
        gm <- gemeinde()
        future_promise({
            subset(hospitals,community_code==gm)
        }) %...>%
            {
                p$set(message = " Rendering Map...")
                .
            } %...>%
            {
                df <- .
            center.lon <- (max(df$krankenhausStandort.position.longitude)+min(df$krankenhausStandort.position.longitude))/2
            center.lat <- (max(df$krankenhausStandort.position.latitude)+min(df$krankenhausStandort.position.latitude))/2
            
            
            zoom_lat <- abs(abs(max(df$krankenhausStandort.position.latitude)) - abs(min(df$krankenhausStandort.position.latitude)))
            zoom_long <- abs(abs(max(df$krankenhausStandort.position.longitude)) - abs(min(df$krankenhausStandort.position.longitude)))
            
            zoom_factor <- max(zoom_lat,zoom_long)
            auto_zoom <- -1.35 * log(zoom_factor) + 8
    
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
                        paste("Low Care:",maxBettenStatusEinschaetzungLowCare),
                        paste("High Care:",maxBettenStatusEinschaetzungHighCare),
                        paste("ECMO:",maxBettenStatusEinschaetzungEcmo),
                        sep="<br />")
                    )
            
            fig <- fig %>%
                layout(
                    mapbox = list(
                        style = 'dark',
                        zoom = auto_zoom,
                        center = list(lon = center.lon, lat = center.lat)
                        ),
                    legend = list(orientation = 'h'),
                    margin=list(l=30,r=30,t=30,b=30)
                    )
            fig <- fig %>%
                config(mapboxAccessToken = mapBoxToken, displayModeBar = FALSE)
            
            fig } %>% finally(~p$close())
    })
    
    output$deutschlandMap <- renderPlotly({
        center.lon <- median(hospitals$krankenhausStandort.position.longitude)
        center.lat <- median(hospitals$krankenhausStandort.position.latitude)

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
                source = "deutschlandMap",
                hovertext = ~paste(
                    paste0("<b>",krankenhausStandort.bezeichnung,"</b>"),
                    paste0("Address: ",krankenhausStandort.strasse," ",
                           krankenhausStandort.hausnummer,", ",
                           krankenhausStandort.plz," ",
                           krankenhausStandort.ort),
                    paste("Low Care:",maxBettenStatusEinschaetzungLowCare),
                    paste("High Care:",maxBettenStatusEinschaetzungHighCare),
                    paste("ECMO:",maxBettenStatusEinschaetzungEcmo),
                    sep="<br />")
            )
        fig <- fig %>%
            layout(
                mapbox = list(
                    style = 'dark',
                    zoom = 5,
                    center = list(lon = center.lon, lat = center.lat)),
                legend = list(orientation = 'h')
                ) 
        fig <- fig %>%
            config(mapboxAccessToken = mapBoxToken, displayModeBar = FALSE)
        
        fig
    })
    
    output$plotlyClick <- renderTable({
        data <- event_data("plotly_click",source = "diviMap")
        
        if(!is.null(data)) {
            id <- data$customdata
            json <- getReportingSections(id) %>%
                bind_rows() %>%
                dplyr::select(bezeichnung,)
            rbind(json) %>%
                rename(Bereich=bezeichnung)
        }
    })
    
    output$overallPlotlyClick <- renderTable({
        data <- event_data("plotly_click",source = "deutschlandMap")
        
        if(!is.null(data)) {
            id <- data$customdata
            json <- getReportingSections(id) %>%
                bind_rows() %>%
                dplyr::select(bezeichnung)
            rbind(json) %>%
                rename(Bereich=bezeichnung)
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
    
    output$rkiPlot <- renderPlotly({
        n <- 2
        withProgress(message = "RKI plot: generating data", value = 0, {
            df <- rkiData %>% filter(IdLandkreis==gemeinde()) %>%
                group_by(Refdatum) %>%
                summarise(cases=sum(cases),deaths=sum(deaths)) %>%
                ungroup() %>% arrange(Refdatum) %>%
                mutate(cumCases=cumsum(cases),cumDeaths=cumsum(deaths))
            
            incProgress(1/n, detail = paste("RKI plot: generating plot"))
            p <- df %>% plot_ly(type="scatter",mode="lines") %>%
                add_trace(x=~Refdatum,
                          y=~cumCases,
                          hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
                          name="Gesamte Fälle",
                          yaxis="y",
                          line=list(color=toRGB("black"))) %>%
                add_trace(x=~Refdatum,
                          y=~cumDeaths,
                          hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
                          name="Tote",
                          yaxis="y2",
                          line=list(color=toRGB("red"))) %>%
                add_trace(x=~date,
                          y=~faelle_covid_aktuell,
                          data=filter(diviData,gemeinde==gemeinde()),
                          name="COVID Patienten\n(auf Intensivstation)",
                          hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
                          line=list(color=toRGB("darkgreen")),
                          yaxis="y2") %>%
                add_trace(x=~date,
                          y=~faelle_covid_aktuell_beatmet,
                          data=filter(diviData,gemeinde==gemeinde()),
                          name="COVID Patienten\n(beatmet auf Intensiv)",
                          hovertemplate = paste0('Datum: %{x}','<br>Fälle: %{y}'),
                          line=list(color=toRGB("orange")),
                          yaxis="y2") %>%
                plotly::layout(
                    xaxis=list(title="Datum"),
                    yaxis=list(title="Fallzahl",side="left"),
                    yaxis2=list(title="Tote",overlaying="y",side="right"),
                    margin=list(l=40,r=40,t=0,b=0),
                    hovermode="x unified",
                    legend = list(orientation = 'h'))
            incProgress(2/n, detail = "RKI plot: rendering")
            p
        })
    })
    
    output$rkiAgePlot <- renderPlotly({
        rkiData %>% filter(IdLandkreis==gemeinde()) %>%
            plot_ly(type="scatter",mode="lines") %>%
            add_trace(x=~Refdatum,
                      y=~cumCases,
                      color=~Altersgruppe,
                      text=~Altersgruppe,
                      legendgroup="Fälle",
                      hovertemplate=paste(
                          'Datum: %{x}',
                          'Fälle: %{y}',
                          'Altersgruppe: %{text}',
                          sep = "<br>"
                      )) %>%
            add_trace(x=~Refdatum,
                      y=~cumDeaths,
                      color=~Altersgruppe,
                      text=~Altersgruppe,
                      legendgroup="Todesfälle",
                      line=list(dash="dash"),
                      yaxis="y2",
                      hovertemplate=paste('Datum: %{x}',
                                          'Todesfälle: %{y}',
                                          'Altersgruppe: %{text}',
                                          sep = "<br>")
                      ) %>%
            plotly::group_by(Altersgruppe) %>%
            plotly::layout(
                xaxis=list(title="Datum"),
                yaxis=list(title="Fallzahl",side="left"),
                yaxis2=list(title="Tote",overlaying="y",side="right"),
                margin=list(l=40,r=40,t=0,b=0),
                legend=list(x=0,
                            y=1,
                            margin=list(l=80,r=30,t=30,b=30)
                        )
           )
    })
    
    output$overallBetten <- renderPlotly({
        n <- 2
        withProgress(message = "DIVI: preparing data", value = 0, {
            dt <- diviData %>% group_by(date) %>% summarise(#sum_area = sum(area),
                                                            #sum_pop = sum(pop_all),
                                                            #sum_standorte = sum(anzahl_standorte),
                                                            #sum_free_beds = sum(betten_frei),
                                                            #sum_occup_beds = sum(betten_belegt),
                                                            sum_faelle_covid_aktuell = sum(faelle_covid_aktuell),
                                                            sum_faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet))
            incProgress(1/n, detail = "DIVI: preparing plot")
            p <- plot_ly(dt, type="scatter",mode="lines") %>%
                add_trace(x=~date, y=~sum_faelle_covid_aktuell, name="Fälle(ICU)") %>%
                add_trace(x=~date,y=~sum_faelle_covid_aktuell_beatmet, name="Fälle(beatmet)") %>%
                add_trace(data = diviForecast$total,
                          x=~.index,
                          y=~.conf_hi_sum_covid,
                          line = list(color = 'transparent'),
                          showlegend=FALSE,
                          name="Vorhersage: hoch") %>%
                add_trace(data = diviForecast$total,
                          x=~.index,
                          y=~.conf_lo_sum_covid,
                          fill="tonexty",
                          fillcolor='rgba(255,127,14,0.2)',
                          line = list(color = 'transparent'),
                          showlegend=FALSE,
                          name="Vorhersage: niedrig") %>%
                add_trace(data = diviForecast$total,
                          x=~.index,y=~.value_sum_covid,
                          name="Vorhersage: Fälle(ICU)",
                          line = list(color="#FF7F0E")) %>%
                add_trace(data = diviForecast$intub,
                          x=~.index,
                          y=~.conf_hi_sum_intub,
                          line = list(color = 'transparent'),
                          showlegend=FALSE,
                          name="Vorhersage: hoch") %>%
                add_trace(data = diviForecast$intub,
                          x=~.index,
                          y=~.conf_lo_sum_intub,
                          fill="tonexty",
                          fillcolor='rgba(40,160,40,0.2)',
                          line = list(color = 'transparent'),
                          showlegend=FALSE,
                          name="Vorhersage: niedrig") %>%
                add_trace(data = diviForecast$intub,
                          x=~.index,y=~.value_sum_intub,
                          name="Vorhersage: Fälle(beatmet)",
                          line = list(color="#2CA02C")) %>%
                plotly::layout(
                    xaxis=list(title="Datum"),
                    yaxis=list(title="Fälle"),
                    hovermode="x unified",
                    margin=list(l=40,r=40,t=0,b=0)
                    )
            incProgress(2/n, detail = "DIVI: rendering plot")
            p
        })
    })
    
    output$bundeslandBetten <- renderPlotly({
        dt <- diviData %>% filter(!is.na(bundesland)) %>%
            group_by(date,bundesland) %>% summarise(#sum_area = sum(area),
                                                        #sum_pop = sum(pop_all),
                                                        #sum_standorte = sum(anzahl_standorte),
                                                        #sum_free_beds = sum(betten_frei),
                                                        #sum_occup_beds = sum(betten_belegt),
                                                        sum_faelle_covid_aktuell = sum(faelle_covid_aktuell)
                                                        #sum_faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_beatmet)
                                                        )
        plot_ly(dt,
                x=~date,
                y=~sum_faelle_covid_aktuell,
                color=~as.factor(bundesland),
                name=~blNames[bundesland]) %>%
            plotly::group_by(bundesland) %>%
            add_lines() %>%
            plotly::layout(
                xaxis=list(title="Datum"),
                yaxis=list(title="Fälle"),
                margin=list(l=40,r=40,t=0,b=0)
            )
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
            plotly::layout(xaxis=list(title="Datum"),
                           yaxis=ylbl,
                           hovermode="x unified",
                           margin=list(l=40,r=40,t=0,b=0),
                           legend = list(orientation = 'h'))
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
                                            '<br>Prozent: %{y}'),
                      yaxis="y") %>%
            add_trace(x=~date,y=~auslastung,name="Auslastung",
                      hovertemplate = paste('Datum: %{x}',
                                            '<br>Prozent: %{y}'),
                      yaxis="y") %>%
            add_trace(x=~date,
                      y=~betten_frei,
                      data=filter(diviData,gemeinde==gemeinde()),
                      name="Intensivbetten(frei)",
                      hovertemplate = paste0('Datum: %{x}','<br>Betten: %{y}'),
                      line=list(color=toRGB("lightblue")),
                      yaxis="y2"
            ) %>%
            add_trace(x=~date,
                      y=~betten_belegt,
                      data=filter(diviData,gemeinde==gemeinde()),
                      name="Intensivbetten(belegt)",
                      hovertemplate = paste0('Datum: %{x}','<br>Betten: %{y}'),
                      line=list(color=toRGB("yellowgreen")),
                      yaxis="y2"
            ) %>%
        plotly::layout(
            xaxis=list(title="Datum"),
            yaxis=list(title="Prozent",side="left"),
            yaxis2=list(title="Betten",overlaying="y",side="right"),
            margin=list(l=40,r=40,t=0,b=0),
            hovermode="x unified",
            legend = list(orientation = 'h'))
    })
    
    output$diviBetten <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde())
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date, y=~faelle_covid_aktuell, name="Aktuelle COVID Fälle") %>%
            add_trace(x=~date,y=~faelle_covid_aktuell_beatmet, name="Aktuelle COVID Fälle(beatmet)") %>%
            plotly::layout(
                xaxis=list(title="Datum"),
                yaxis=list(title="Fälle"),
                margin=list(l=40,r=40,t=0,b=0),
                hovermode="x unified",
                legend = list(orientation = 'h')
            )
    })
    
    output$diviPop <- renderPlotly({
        dt <- diviData %>% dplyr::filter(gemeinde==gemeinde())
        plot_ly(dt, type="scatter",mode="lines") %>%
            add_trace(x=~date, y=~covid_per_100k, name="COVID Fälle/100k Einwohner") %>%
            add_trace(x=~date, y=~covid_per_100k_intubated, name="COVID Fälle/100k Einwohner(beatmet)") %>%
            plotly::layout(
                xaxis=list(title="Datum"),
                yaxis=list(title="Fälle/100k Einwohner"),
                margin=list(l=40,r=40,t=0,b=0),
                hovermode="x unified",
                legend = list(orientation = 'h')
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
