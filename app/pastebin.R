output$predictICU <- renderPlotly({
    forecast_length <- 10
    germany <- diviData %>%
        group_by(date) %>%
        summarise(sum_cov = sum(faelle_covid_aktuell), sum_intub = sum(faelle_covid_aktuell_beatmet)) %>%
        ungroup()
    fcVal <- tsclean(germany$sum_intub) %>% auto.arima(.) %>% forecast(.,forecast_length)
    cov_forecast <- tsclean(germany$sum_cov) %>% auto.arima(.) %>% forecast(.,forecast_length)
    
    fore.dates <- seq(as.POSIXct(germany$date[length(germany$date)], origin='1970-01-01'),
                      by=germany$date[length(germany$date)] - germany$date[length(germany$date)-1], len=forecast_length)
    
    
    plot_ly(mode="lines") %>%
        add_lines(x=as.POSIXct(germany$date, origin='1970-01-01'),
                  y= germany$sum_intub,
                  name="Aktuelle COVID Fälle(beatmet)",
                  color=I("green"),
                  marker=list(mode="lines")) %>%
        add_lines(x=fore.dates,
                  y=fcVal$mean,
                  color=I("blue"),
                  name="Aktuelle COVID Fälle(beatmet) Vorhersage") %>%
        add_ribbons(x=fore.dates,
                    ymin=fcVal$lower[,2],
                    ymax=fcVal$upper[,2],
                    name="95% confidence",
                    color = I("gray95")) %>%
        add_lines(x=as.POSIXct(germany$date, origin='1970-01-01'),
                  y= germany$sum_cov,
                  name="Aktuelle COVID Fälle",
                  color=I("orange"),
                  marker=list(mode="lines")) %>%
        add_lines(x=fore.dates,
                  y=cov_forecast$mean,
                  color=I("yellow"),
                  name="Aktuelle COVID Fälle Vorhersage") %>%
        add_ribbons(x=fore.dates,
                    ymin=cov_forecast$lower[,2],
                    ymax=cov_forecast$upper[,2],
                    name="Aktuelle COVID Fälle 95% confidence",
                    color = I("gray95"))
})