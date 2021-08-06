download.file("https://opendata.arcgis.com/datasets/6d78eb3b86ad4466a8e264aa2e32a2e4_0.csv",
              destfile = "rkiData/rkiHistory.csv")

library(vroom)
library(dplyr)
library(lubridate)

raw_rki <- vroom("rkiData/rkiHistory.csv",col_types = "ccccnnnnn")

rki <- raw_rki %>%
    mutate(Datum = ymd_hms(Datum))
