## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

#' Convert the the cmip rcmip inputs to the Hector inputs
#'
#' @param scenario a string vector that defines the scenarios to process, the default is set to
#' NULL and will process all of the CMIP6 phase specific scenarios.
#' @return a data frame of Hector inputs
#' @importFrom assertthat assert_that
#' @importFrom data.table melt.data.table
#' @noRd
convert_rcmipCMIP6_hector <- function(scenario = NULL){

  # Make sure that strings are not read in as factors. 
  options(stringsAsFactors=FALSE)
  
  # If there is no specified scenario then process all of the CMIP6 scenarios.
  if(is.null(scenario)){
    scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126",
                  "ssp245", "ssp534-over", "ssp585")
  }

  # Download the Minted data from the zenodo repository. 
  conc_data <- data.table::as.data.table(rpackageutils::remote_read("https://zenodo.org/record/3779281/files/rcmip-concentrations-annual-means-v4-0-0.csv?download=1"))
  emiss_data <- data.table::as.data.table(rpackageutils::remote_read("https://zenodo.org/record/3779281/files/rcmip-emissions-annual-means-v4-0-0.csv?download=1"))

  # Check inputs 
  assert_that(is.integer(years))
  
  # Make sure data exists for the scenario(s) selected to process. 
  data_scns <- unique(emiss_data$Scenario, conc_data$Scenario)
  available <- scenario %in% data_scns
  assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))

  # Concatenate the long emissions and concentration data tables together and subset so that
  # only the scenarios of interest will be converted.
  raw_inputs <- rbind(conc_data, emiss_data, fill = TRUE)[Scenario %in% scenario  & Region == "World"]
  
  # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
  # unit, ect. These columns will be used to transform the data from being wide to long so that each row
  # corresponds to concentration for a specific year.
  id_vars <- which(!grepl(pattern = "^X[[:digit:]]{4}", x = names(raw_inputs)))
  raw_inputs <- melt.data.table(data = raw_inputs, id.vars = id_vars,
                                           variable.name = "year", value.name = "value",
                                           variable.factor = FALSE)
  
  # Remove NAs and convert the year to an integer. 
  raw_inputs <- stats::na.omit(raw_inputs[ , year :=  as.integer(substr(year, 2, 5))])

  # Add the conversion data table information to the raw data with an inner join so that only variables that
  # have conversion information will be converted. The raw inputs include values for variables that Hector
  # does not have that are required by other classes of simple climate models.
  conversion_table <- hectordata::rcmipCMIP6_conversion
  input_conversion_table <- stats::na.omit(raw_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])

  # Convert the value column from RCMIP units to Hector units.
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
  
  return(converted_cmip6)
}
