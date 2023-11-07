# Objective: quickly visualize Hector results, this might be helpful to do sometimes... 

# 0. SET UP --------------------------------------------------------------------
library(hector)
library(ggplot2)
library(magrittr)

INPUT_DIR <- here::here("inst", "input")

theme_set(theme_bw())


# 1. PI CONTROL ----------------------------------------------------------------
ini <- file.path(INPUT_DIR, "picontrol_concentration.ini")
hc <- newcore(ini)
run(hc)
out <- fetchvars(hc, 1900:2300, vars = c(GLOBAL_TAS(), RF_TOTAL()))

# this shoudl be a pretty boring plot, just 0s... 
ggplot(data = out) +
  geom_line(aes(year, value, color = variable)) + 
  facet_wrap("variable")
