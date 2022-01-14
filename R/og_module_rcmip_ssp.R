#' #' Convert the the rcmip inputs to the Hector inputs for the ssp scenarios
#' #'
#' #' @param scenario a string vector that defines the scenarios to process, the default is set to
#' #' NULL and will process all of the ssps scenarios.
#' #' @param years a vector of the years to convert from rcmip to hector intpus, default set to sequence from 1750 to 2100
#' #' @return a data frame of Hector inputs
#' #' @import data.table
#' #' @importFrom assertthat assert_that
#' #' @importFrom magrittr %>% 
#' #' @noRd
#' convert_ssp_hector <- function(scenario = NULL, years = 1750:2100){
#' 
#'   # Silence package checks 
#'   scenarios <- Scenario <- Region <- hector_variable <- hector_unit <- NULL
#'     
#'   # Make sure that strings are not read in as factors. 
#'   options(stringsAsFactors=FALSE)
#'   
#'   # If there is no specified scenario then process all of the CMIP6 scenarios.
#'   if(is.null(scenario)){
#'     scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")
#'   }
#'   
#'   # Download the Minted data from the zenodo repository, this is not working at the 
#'   # moment will need to problem solve. 
#'   link <- "https://zenodo.org/record/3779281/files/rcmip-emissions-annual-means-v4-0-0.csv?download=1"
#'   emiss_data <- data.table::as.data.table(readr::read_csv(url(link)))
#'     
#'   link <- "https://zenodo.org/record/3779281/files/rcmip-concentrations-annual-means-v4-0-0.csv?download=1"
#'   conc_data <- data.table::as.data.table(readr::read_csv(url(link)))
#' 
#' 
#'   # Check inputs 
#'   assert_that(is.integer(years))
#'   
#'   # Make sure data exists for the scenario(s) selected to process. 
#'   data_scns <- unique(emiss_data$Scenario, conc_data$Scenario)
#'   available <- scenario %in% data_scns
#'   assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))
#' 
#'   # Concatenate the long emissions and concentration data tables together and subset so that
#'   # only the scenarios of interest will be converted.
#'   raw_inputs <- rbind(conc_data, emiss_data, fill = TRUE)[Scenario %in% scenario  & Region == "World"]
#'   
#'   # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
#'   # unit, ect. These columns will be used to transform the data from being wide to long so that each row
#'   # corresponds to concentration for a specific year.
#'   id_vars <- which(!grepl(pattern = "^[[:digit:]]{4}", x = names(raw_inputs)))
#'   long_inputs <- data.table::melt.data.table(data = raw_inputs, id.vars = id_vars,
#'                                            variable.name = "year", value.name = "value",
#'                                            variable.factor = FALSE)
#'   # Convert the year to an integer. 
#'   long_inputs <- long_inputs[ , year :=  as.integer(year)]
#' 
#'   # Add the conversion data table information to the raw data with an inner join so that only variables that
#'   # have conversion information will be converted. The raw inputs include values for variables that Hector
#'   # does not have that are required by other classes of simple climate models.
#'   conversion_table <- hectordata::rcmipCMIP6_conversion
#'   input_conversion_table <- stats::na.omit(long_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
#' 
#'   # Convert the value column from RCMIP units to Hector units.
#'   # This step may take a while depending on the number of scenarios being
#'   # processed.
#'   mapply(ud_convert2,
#'          x = input_conversion_table$value,
#'          from = input_conversion_table$rcmip_udunits,
#'          to = input_conversion_table$hector_udunits,
#'          SIMPLIFY = FALSE) %>%
#'     unlist ->
#'     new_values
#' 
#'   # Create the data table of the inputs that have the Hector relvant variable, units, and values by selecting
#'   # and renaming the columns from the input conversion table. Then add the converted values.
#'   converted_cmip6 <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
#'   names(converted_cmip6) <- c('scenario', 'year', 'variable', 'units')
#'   converted_cmip6[['value']] <- new_values
#'   
#'   return(converted_cmip6)
#' }
#' 
#' 
#' #' Get the ssp rf values 
#' #'
#' #' @param scenario a string vector that defines the scenarios to process, the default is set to
#' #' NULL and will process all of the ssp scenarios.
#' #' @param years a vector of the years to convert from rcmip to hector inputs, default set to sequence from 1750 to 2100.
#' #' @return data.table of radiative forcing values that can be read into Hector.
#' #' @import data.table
#' #' @noRd
#' prepare_ssp_rf <- function(scenario = NULL, years = 1750:2100){
#'   
#'   # If there is no specified scenario then process all of the CMIP6 scenarios.
#'   if(is.null(scenario)){
#'     scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")
#'   }
#'   
#'   ar6_files <- c("https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp119_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp126_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp245_1750-2500.csv",
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp334_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp370_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp434_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp460_1750-2500.csv",
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp534-over_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/SSPs/ERF_ssp585_1750-2500.csv")
#'   
#'   to_import   <- ar6_files[grepl(pattern = paste0(scenario, collapse = "|"), x = ar6_files)]
#'   ssp_rf_data_list <- lapply(to_import, function(f){
#'     
#'     # Read in remote data
#'     link <- url(f)
#'     data <- as.data.table(read.csv(f))
#'     close(link)
#'     
#'     # Make a mapping file to translate the ar6 variable to the hector variable name. 
#'     mapping_table <- data.table("ar6_variable" = c("volcanic", "land_use", "total"), 
#'                                 "variable" = c("SV", "Ftalbedo", "Ftot_constrain"))
#'     
#'     # Determine the scenario name from the file name. 
#'     scenario <- gsub(pattern = "ERF_|_1750-2500.csv", replacement = "", x = basename(f))
#'     data$scenario <- scenario
#'     
#'     # Select the columns, reshape, and rename the variables. 
#'     cols <- c("scenario", "year", "volcanic", "land_use", "total")
#'     data[, ..cols] %>% 
#'       melt(., id.vars = c("scenario", "year"), variable.name = "ar6_variable", value.name = "value") %>% 
#'       merge(mapping_table, on = c("scenario", "year")) %>% 
#'       .[, c("scenario", "year", "variable", "value")] -> 
#'       out 
#'     
#'     return(out)
#'   })
#'   
#'   ssp_rf_data <- data.table(rbindlist(ssp_rf_data_list))
#'   ssp_rf_data$units <- "W/m2"
#'   return(ssp_rf_data)
#' 
#' }
#' 
#' 
#' #' Convert the the rcmip inputs to the Hector inputs for the rcp scenarios
#' #'
#' #' @param scenario a string vector that defines the scenarios to process,  the default is set to
#' #' NULL and will process all of the rcp scenarios.
#' #' @param years a vector of the years to convert from rcmip to hector intpus, default set to sequence from 1750 to 2100
#' #' @return a data frame of Hector inputs
#' #' @import data.table 
#' #' @importFrom assertthat assert_that
#' #' @importFrom magrittr %>% 
#' #' @noRd
#' convert_rcp_hector <- function(scenario = NULL, years = 1750:2100){
#'   
#'   # Silence package checks 
#'   scenarios <- Scenario <- Region <- hector_variable <- hector_unit <- NULL
#'   
#'   # Make sure that strings are not read in as factors. 
#'   options(stringsAsFactors=FALSE)
#'   
#'   # If there is no specified scenario then process all of the CMIP6 scenarios.
#'   if(is.null(scenario)){
#'     scenario <- c("rcp26", "rcp45", "rcp60", "rcp85")
#'   }
#'   
#'   # Download the Minted data from the zenodo repository, this is not working at the 
#'   # moment will need to problem solve. 
#'   link <- "https://zenodo.org/record/3779281/files/rcmip-emissions-annual-means-v4-0-0.csv?download=1"
#'   emiss_data <- data.table::as.data.table(readr::read_csv(url(link)))
#'   
#'   link <- "https://zenodo.org/record/3779281/files/rcmip-concentrations-annual-means-v4-0-0.csv?download=1"
#'   conc_data <- data.table::as.data.table(readr::read_csv(url(link)))
#'   
#'   # Make sure data exists for the scenario(s) selected to process. 
#'   data_scns <- unique(emiss_data$Scenario, conc_data$Scenario)
#'   available <- scenario %in% data_scns
#'   assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))
#'   
#'   # Concatenate the long emissions and concentration data tables together and subset so that
#'   # only the scenarios of interest will be converted.
#'   raw_inputs <- rbind(conc_data, emiss_data, fill = TRUE)[Scenario %in% scenario  & Region == "World"]
#'   
#'   # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
#'   # unit, ect. These columns will be used to transform the data from being wide to long so that each row
#'   # corresponds to concentration for a specific year.
#'   id_vars <- which(!grepl(pattern = "^[[:digit:]]{4}", x = names(raw_inputs)))
#'   long_inputs <- data.table::melt.data.table(data = raw_inputs, id.vars = id_vars,
#'                                              variable.name = "year", value.name = "value",
#'                                              variable.factor = FALSE)
#'   # Convert the year to an integer. 
#'   long_inputs <- long_inputs[ , year :=  as.integer(year)]
#'   
#'   # Add the conversion data table information to the raw data with an inner join so that only variables that
#'   # have conversion information will be converted. The raw inputs include values for variables that Hector
#'   # does not have that are required by other classes of simple climate models.
#'   conversion_table <- hectordata::rcmipCMIP6_conversion
#'   input_conversion_table <- stats::na.omit(long_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
#'   
#'   # Convert the value column from RCMIP units to Hector units.
#'   # This step may take a while depending on the number of scenarios being
#'   # processed.
#'   mapply(ud_convert2,
#'          x = input_conversion_table$value,
#'          from = input_conversion_table$rcmip_udunits,
#'          to = input_conversion_table$hector_udunits,
#'          SIMPLIFY = FALSE) %>%
#'     unlist ->
#'     new_values
#'   
#'   # Create the data table of the inputs that have the Hector relvant variable, units, and values by selecting
#'   # and renaming the columns from the input conversion table. Then add the converted values.
#'   converted_cmip6 <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
#'   names(converted_cmip6) <- c('scenario', 'year', 'variable', 'units')
#'   converted_cmip6[['value']] <- new_values
#'   
#'   return(converted_cmip6)
#' }
#' 
#' 
#' #' Get the rcp rf values 
#' #'
#' #' @param scenario a string vector that defines the scenarios to process, the default is set to NULL and will process all of the CMIP6 phase specific scenarios.
#' #' @param years a vector of the years to convert from rcmip to hector intpus, default set to sequence from 1750 to 2100
#' #' @return data.table of radiative forcing values that can be read into Hector.
#' #' @import data.table 
#' #' @noRd
#' prepare_rcp_rf <- function(scenario = NULL, years = 1750:2100){
#'   
#'   # If there is no specified scenario then process all of the CMIP6 scenarios.
#'   if(is.null(scenario)){
#'     scenario <- c("rcp26", "rcp45", "rcp60", "rcp85")
#'   }
#'   
#'   rcp_files <- c("https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/RCPs/ERF_rcp26_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/RCPs/ERF_rcp45_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/RCPs/ERF_rcp60_1750-2500.csv", 
#'                  "https://raw.githubusercontent.com/chrisroadmap/ar6/main/data_output/RCPs/ERF_rcp85_1750-2500.csv")
#'   to_import   <- rcp_files[grepl(pattern = paste0(scenario, collapse = "|"), x = rcp_files)]
#'   
#' 
#'   rcp_rf_data_list <- lapply(to_import, function(f){
#'     
#'     # Read in remote data
#'     link <- url(f)
#'     data <- as.data.table(read.csv(f))
#'     close(link)
#'     
#'     # Make a mapping file to translate the ar6 variable to the hector variable name. 
#'     mapping_table <- data.table("rcp_variable" = c("volcanic", "land_use", "total"), 
#'                                 "variable" = c("SV", "Ftalbedo", "Ftot_constrain"))
#'     
#'     # Determine the scenario name from the file name. 
#'     scenario <- gsub(pattern = "ERF_|_1750-2500.csv", replacement = "", x = basename(f))
#'     data$scenario <- scenario
#'     
#'     # Select the columns, reshape, and rename the variables. 
#'     cols <- c("scenario", "year", "volcanic", "land_use", "total")
#'     data[, ..cols] %>% 
#'       melt(., id.vars = c("scenario", "year"), variable.name = "rcp_variable", value.name = "value") %>% 
#'       merge(mapping_table, on = c("scenario", "year")) %>% 
#'       .[, c("scenario", "year", "variable", "value")] -> 
#'       out 
#'     
#'     return(out)
#'   })
#'   rcp_rf_data <- data.table(rbindlist(rcp_rf_data_list))
#'   rcp_rf_data$units <- "W/m2"
#'   return(rcp_rf_data)
#' }
