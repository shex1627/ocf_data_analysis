library(tidyr)
library(dplyr)
library(lubridate)

toner <- read.csv("~/Downloads/printer_toner_public (1).csv")

toner_sp18 = toner %>%
  filter(date(date) >= "2018-01-16")
  
papercut = toner_sp18 %>%
  filter(printer == "papercut")

plot(papercut$value)
