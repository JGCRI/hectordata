# 
# 
# library(magrittr)
# library(data.table)
# 
# 
# 
# 
# 
# # TODO this part of the function will be replaced with however we end up pull the MINTED  data 
# # fromt he PNNL data hub.
# protocol_version <- "4-0-0"
# conc_data <- file.path(
#   "data-raw",
#   sprintf("rcmip-concentrations-annual-means-v%s.csv", protocol_version)
# ) %>%
#   readr::read_csv() %>% 
#   as.data.table()
#   
# # Repeate the transfomration of a wide to long data table for the emissions data. 
# emiss_data <- file.path(
#   "data-raw",
#   sprintf("rcmip-emissions-annual-means-v%s.csv", protocol_version)
# ) %>%  
#   readr::read_csv() %>% 
#   as.data.table()
# 
# 
# # Determine the columns that contain identifier information, such as the model, scneairo, region, variable, 
# # unit, ect. These columns will be used to transform the data from being wide to long so that each row 
# # corresponds to concenration for a specific year. 
# id_vars <- which(!grepl(pattern = "[[:digit:]]{4}", x = names(conc_data)))
# conc_long <- data.table::melt.data.table(data = conc_data, id.vars = id_vars, variable.name = "year", value.name = "value")
# 
# 
# id_vars <- which(!grepl(pattern = "[[:digit:]]{4}", x = names(emiss_data)))
# emiss_long <- data.table::melt.data.table(data = emiss_data, id.vars = id_vars, variable.name = "year", value.name = "value")
# 
# # Concatenate the long emissions and concetnration data tables together. 
# raw_inputs <- rbind(emiss_long, conc_long)
# 
# 
# 
# 
#   
# 
