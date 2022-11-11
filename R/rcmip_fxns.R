#' Process the inputs materials from RCMIP into the appropriate units for Hector
#'
#' @param scenarios_to_process string vector of the scenario names to process
#' @import data.table
#' @return data frame of Hector inputs for multiple scenarios 
#' @export
process_rcmip_data <- function(scenarios_to_process=NULL){
  
  rcmip_dir <- find_input_dir(dir = ZENODO_DIR, "rcmip")
  assertthat::assert_that(dir.exists(rcmip_dir), msg = "data dir not found")

  # Import the concentrations, emissions, and radiative forcing 
  # files and subset to include only the data to process.
  files <- list.files(rcmip_dir, full.names = TRUE, pattern = "means")
  all_data <- as.data.table(do.call(lapply(files, read.csv), what = "rbind"))

  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("rcp60", "ssp370", "ssp370-lowNTCF-aerchemmip", "ssp370-lowNTCF-gidden",    
                              "ssp434", "ssp460", "rcp26", "ssp119",                   
                              "ssp126", "rcp85", "ssp245" , "rcp45" ,                   
                              "ssp534-over", "ssp585", "1pctCO2", "1pctCO2-4xext",           
                              "abrupt-0p5xCO2", "abrupt-2xCO2", "abrupt-4xCO2", "historical",               
                              "historical-cmip5", "piControl", "esm-bell-1000PgC", "esm-bell-2000PgC",         
                              "esm-bell-750PgC", "esm-pi-CO2pulse", "esm-pi-cdr-pulse", "esm-piControl")
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
  
  # Save intermediate data.
  ofile <- file.path(INTERMEDIATE_DIR, "rcmip_data.csv")
  write.csv(x = final_data, file = ofile, row.names = FALSE)
  return(final_data)
  
}


#' Generate the Hector input tables and ini files that were used in RCMIP phases 1 & 2
#' 
#' See https://doi.org/10.5194/gmd-13-5175-2020 for more details
#' 
#' TODO needs better way to determine dependencies 
#' @param scenarios_to_process string vector of the scenario names to process
#' @param depends_on string vector of the required intermeidate data tables
#' @return nothing writes out the csv and ini files
generate_rcmip_submission_files <- function(scenarios_to_process=NULL, depends_on = c("rcmip_data.csv")){
  # Check to make sure the data exists. 
  data_files <- file.path(INTERMEDIATE_DIR, depends_on)
  assertthat::assert_that(all(file.exists(data_files)), msg = "some element of depends_on does not exist")
  
  # Load the hector input data. 
  hinput_data <- as.data.table(read.csv(data_files))
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", 
                              "ssp460", "ssp534-over", "ssp585", "rcp26", "rcp45", "rcp60", "rcp85")
  }
  
  
  data <- hinput_data[scenario %in% scenarios_to_process, ]
  
  
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
  
  