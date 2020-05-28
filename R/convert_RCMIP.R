## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

#' Convert the the cmip rcmip inputs to the Hector inputs
#'
#' @param scenario a string vector that defines the scenarios to process, the default is set to
#' NULL and will process all of the CMIP6 phase specific scenarios.
#' @return a data frame of Hector inputs
#' @noRd
convert_rcmipCMIP6_hector <- function(scenario = NULL){

  # If there is no specified scenario then process all of the CMIP6 scenarios.
  if(is.null(scenario)){
    scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126",
                  "ssp245", "ssp534-over", "ssp585")
  }

  # Download the Minted data from the zenodo repository. 
  conc_data <- data.table::as.data.table(rpackageutils::remote_read("https://zenodo.org/record/3779281/files/rcmip-concentrations-annual-means-v4-0-0.csv?download=1"))
  emiss_data <- data.table::as.data.table( rpackageutils::remote_read("https://zenodo.org/record/3779281/files/rcmip-emissions-annual-means-v4-0-0.csv?download=1"))

  # Make sure data exists for the scenario(s) selected to process. 
  data_scns <- unique(emiss_data$Scenario, conc_data$Scenario)
  missing   <- !scenario %in% data_scns
  assertthat::assert_that(sum(missing) == 0,
                          msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[missing], collapse = ', ')))


  # Determine the columns that contain identifier information, such as the model, scneairo, region, variable,
  # unit, ect. These columns will be used to transform the data from being wide to long so that each row
  # corresponds to concenration for a specific year.
  id_vars <- which(!grepl(pattern = "[[:digit:]]{4}", x = names(conc_data)))
  conc_long <- data.table::melt.data.table(data = conc_data, id.vars = id_vars,
                                           variable.name = "year", value.name = "value",
                                           variable.factor = FALSE)
  


  id_vars <- which(!grepl(pattern = "[[:digit:]]{4}", x = names(emiss_data)))
  emiss_long <- data.table::melt.data.table(data = emiss_data, id.vars = id_vars,
                                            variable.name = "year", value.name = "value",
                                            variable.factor = FALSE)

  # Concatenate the long emissions and concetnration data tables together and subset so that
  # only the scenarios of intrest will be converted. Remove the NA entries that arose when converted from
  # the wide to long format.
  raw_inputs <- stats::na.omit(rbind(emiss_long, conc_long))
  raw_inputs <- raw_inputs[Scenario %in% scenario  & Region == "World"][ , year :=  as.integer(year)]
  raw_inputs <- remove_spaces(df = raw_inputs, cols = c("Variable", "Mip_Era", "Model", "Scenario", "Region"))
  
  
  
  # Add the conversion data table information to the raw data with an inner join so that only variables that
  # have conversion information will be converted. The raw inputs include values for variables that Hector
  # does not have that are required by other classes of simple climate models.
  conversion_table <- remove_spaces(hectordata::rcmipCMIP6_conversion, cols = c("hector_variable", "rcmip_variable", "rcmip_udunits", "hector_udunits"))
  input_conversion_table <- stats::na.omit(raw_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])

  # Convert to the value column from RCMIP units to Hector units.
  # This step may take a while depending on the number of scenarios being
  # processed.
  mapply(ud_convert2,
         x = input_conversion_table$value,
         from = input_conversion_table$rcmip_udunits,
         to = input_conversion_table$hector_udunits,
         SIMPLIFY = FALSE) %>%
    unlist ->
    new_values

  # Create the data table of the inputs that have the Hector relvant variable, units, and values by selecting
  # and renaming the columns from the input conversion table. Then add the converted values.
  converted_cmip6 <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
  names(converted_cmip6) <- c('scenario', 'year', 'variable', 'units')
  
  converted_cmip6[['value']] <- new_values

  # Fill in the missing years.
  completed_data <- complete_missing_years(data = converted_cmip6, expected_years = 1700:2500)
  return(completed_data)
}
