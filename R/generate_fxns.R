#' Generate emission and concentration constraint csv input tables for Hector.
#'
#' @param scenarios a character vector of scenario names.
#' @param write_to a character of the full path to the directory location for
#'  the user to write the generated csv files to.
#' @param years a vector of the expected output years, default is a sequence from 1750 to 2100. 
#' @return the location of the generated csv files.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>% 
generate_inputs <- function(scenarios, write_to, years=1750:2100){
  
  # Silence package checks  
  emiss_conc <- variable <- na.omit <- scenario <- value <- NULL
  
  # Check to make sure that the output directory exstits.
  assert_that(dir.exists(write_to))
  assert_that(is.numeric(years))
  
  # Create an empty hector_inputs data table.
  hector_inputs <- data.table::data.table()
  
  # A list of scenarios in each data source.
  # TODO this section may expand as the ability to generate hector inputs from different sources expands. 
  # (i.e. idealized scnearios, CMIP5 scenarios, GCAM outputs ect.)
  rcmipCMIP6_scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")
  
  # Assert that the scenarios to process are categorized scenarios.
  assert_that(any(scenarios %in% c(rcmipCMIP6_scenario)), msg = 'unrecognized scenarios')
  
  # Convert Inputs ------------------------------------------------------------------
  # The scenario inputs are provided by different sources IIASA, RCMIP and so on. So 
  # each set of scenarios must be processed with different rules. This series of if else 
  # statements processes the scenario inputs to match Hector inputs based on their source.
  if(any(scenarios %in% rcmipCMIP6_scenario)){
    # Convert the CMIP phase 6 specific scenarios, subset the rcmip CMIP6 scenarios
    # from the scenarios argument to convert. 
    to_process <- intersect(rcmipCMIP6_scenario, scenarios)
    processed  <- convert_rcmipCMIP6_hector(to_process, years = years)
    hector_inputs <- rbind(hector_inputs, processed)
    
  } else {
    
    stop('The ability to process non rcmip cmip6 scenarios has not been added yet. \n
         See https://github.com/JGCRI/hectordata/issues/8')
    
  }
  
  # Interpolate the data over the missing years. 
  hector_inputs <- complete_missing_years(hector_inputs, expected_years = years)
  
  # Format and save the emissions and concentration constraints in the csv files 
  # in the proper Hector table input file. 
  split(hector_inputs, hector_inputs$scenario) %>%  
    sapply(write_hector_csv, write_to = write_to, USE.NAMES = FALSE) -> 
    files 
  
  # Copy over the volcanic RF to create the full 
  emissions_dir <- unique(dirname(files))
  link <- "https://raw.githubusercontent.com/JGCRI/hector/master/inst/input/emissions/volcanic_RF.csv"
  rf_data <- readLines(url(link))
  writeLines(rf_data, file.path(emissions_dir, "volcanic_RF.csv"))

  # Generate the ini files corresponding to the new csv files. 
  inis <- make_new_ini(files)
  
  return(inis)
  
}
