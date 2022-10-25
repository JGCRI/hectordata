#' Process the inputs materials from RCMIP into the appropriate units for Hector
#'
#' @param scenarios_to_process string vector of the scenario names to process
#' @import data.table
#' @return data frame of Hector inputs for multiple scenarios 
process_rcmip_data <- function(scenarios_to_process){
  
  rcmip_dir <- find_input_dir(dir = ZENODO_DIR, "rcmip")

  # Import the data files and subset to include only the data to process.
  files <- list.files(rcmip_dir, full.names = TRUE, pattern = "means")
  all_data <- as.data.table(dplyr::bind_rows(lapply(files, read.csv)))
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("rcp60", "ssp370", "ssp434", "ssp460", "rcp26", "ssp119", "ssp126", "rcp85", 
                              "ssp245", "rcp45", "ssp534-over", "ssp585")  
  } else {
    available <- scenarios_to_process %in% unique(all_data$Scenario)
    assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios_to_process[!available], collapse = ', ')))
  }
  raw_inputs <- all_data[Scenario %in% scenarios_to_process  & Region == "World"]
  
  # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
  # unit, etc. These columns will be used to transform the data from being wide to long so that each row
  # corresponds to concentration for a specific year.
  id_vars <- which(!grepl(pattern = "^X[[:digit:]]{4}", x = names(raw_inputs)))
  long_inputs <- melt.data.table(data = raw_inputs, id.vars = id_vars,
                                 variable.name = "year", value.name = "value",
                                 variable.factor = FALSE)
  
  # Convert the year to an integer.
  long_inputs <- long_inputs[ , year :=  as.integer(gsub(pattern = "X", replacement = "", x = year))]
  
  # Add the conversion data table information to the raw data with an inner join so that only variables that
  # have conversion information will be converted. The raw inputs include values for variables that Hector
  # does not have that are required by other classes of simple climate models.
  input_conversion_table <- stats::na.omit(long_inputs[conversion_table$rcmip, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
  
  # Convert the value column from RCMIP units to Hector units.
  # This step may take a while depending on the number of scenarios being
  # processed.
  new_values <- unlist(mapply(ud_convert2,
                              x = input_conversion_table$value,
                              from = input_conversion_table$rcmip_udunits,
                              to = input_conversion_table$hector_udunits,
                              SIMPLIFY = FALSE))
  
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
  
  return(final_data)
  
}

generate_rcmip_ssps <- function(){
  
  scns <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", 
            "ssp460", "ssp534-over", "ssp585")
  data <- process_rcmip_data(scns)
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file. 
  data_list <- split(data, data$scenario)
  for(dat in data_list){
    ofile <- write_hector_csv(dat, 
                              required = c(REQUIRED_EMISSIONS, REQUIRED_RF), 
                              info_source = "rcmip", 
                              write_to = TABLES_DIR, 
                              end_tag = "_emiss-constraints_rf")
   inis <- make_new_ini(ofile)
    
  }
  
}

generate_rcmip_rcps <- function(){
  
  scns <- c("rcp26", "rcp45", "rcp60", "rcp85")
  data <- process_rcmip_data(scns)
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file. 
  data_list <- split(data, data$scenario)
  for(dat in data_list){
    ofile <- write_hector_csv(dat, 
                              required = c(REQUIRED_EMISSIONS, REQUIRED_RF), 
                              info_source = "rcmip", 
                              write_to = TABLES_DIR, 
                              end_tag = "_emiss-constraints_rf")
    inis <- make_new_ini(ofile)
    
  }
  
}





