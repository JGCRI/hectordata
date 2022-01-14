

#' Generate the ssp ini & csv files from the rcmip source
#'
#' @return str of the ini files 
#' @export
#' @importFrom assertthat assert_that
module_rcmip_ssp <- function(){

  ds_inputs <- c("rcmip-emissions-annual-means-v5-1-0.csv",
                 "rcmip-concentrations-annual-means-v5-1-0.csv",
                 "rcmip-radiative-forcing-annual-means-v5-1-0.csv")

  # Load of the data files. 
  data <- load_data(ds_inputs)
  
  # The scenarios that are being processed in this module. 
  scenario <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", "ssp460", "ssp534-over", "ssp585")

  # Make sure data exists for the scenario(s) selected to process.
  data_scns <- unique(c(data$`rcmip-emissions-annual-means-v5-1-0.csv`$Scenario, 
                        data$`rcmip-concentrations-annual-means-v5-1-0.csv`$Scenario, 
                        data$`rcmip-radiative-forcing-annual-means-v5-1-0.csv`$Scenario))
  available <- scenario %in% data_scns
  assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))
  
  # Concatenate the long emissions and concentration data tables together and subset so that
  # only the scenarios of interest will be converted.
  raw_inputs <- rbind(data$`rcmip-emissions-annual-means-v5-1-0.csv`,
                      data$`rcmip-concentrations-annual-means-v5-1-0.csv`, 
                      data$`rcmip-radiative-forcing-annual-means-v5-1-0.csv`, fill = TRUE)[Scenario %in% scenario  & Region == "World"]

  # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
  # unit, etc. These columns will be used to transform the data from being wide to long so that each row
  # corresponds to concentration for a specific year.
  id_vars <- which(!grepl(pattern = "^X[[:digit:]]{4}", x = names(raw_inputs)))
  long_inputs <- data.table::melt.data.table(data = raw_inputs, id.vars = id_vars,
                                             variable.name = "year", value.name = "value",
                                             variable.factor = FALSE)
  
  # Convert the year to an integer.
  long_inputs <- long_inputs[ , year :=  as.integer(gsub(pattern = "X", replacement = "", x = year))]
  
  # Add the conversion data table information to the raw data with an inner join so that only variables that
  # have conversion information will be converted. The raw inputs include values for variables that Hector
  # does not have that are required by other classes of simple climate models.
  conversion_table <- hectordata::rcmipCMIP6_conversion
  input_conversion_table <- stats::na.omit(long_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
  
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
  
  # Create the data table of the inputs that have the Hector relevant variable, units, and values by selecting
  # and renaming the columns from the input conversion table. Then add the converted values.
  converted_cmip6 <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
  names(converted_cmip6) <- c('scenario', 'year', 'variable', 'units')
  converted_cmip6[['value']] <- new_values
  
  # Interpolate the data over the missing years.
  complete_data <- complete_missing_years(converted_cmip6, expected_years = YEARS)
  
  # Format hector inputs so that negative carbon emissions are properly
  # categorized into daccs and land uptake.
  final_data <- process_carbon_cycle_emissions(complete_data)
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file.
  split(final_data, final_data$scenario) %>%
    sapply(write_hector_csv, write_to = TABLES_DIR, USE.NAMES = FALSE, source = "rcmip") ->
    files
  
  # Generate the ini files corresponding to the new csv files.
  inis <- make_new_ini(files)
  return(inis)
  

    }

