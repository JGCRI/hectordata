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
  
  # A list of scenarios that to be processed, categorized by the source of the raw data. 
  ssp_scenarios_rcmip    <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", "ssp460", "ssp534-over", "ssp585")
  rcp_scenarios          <- c("rcp26", "rcp45", "rcp60", "rcp85")
  complete_scenario_list <- c(ssp_scenarios_rcmip, rcp_scenarios)
  
  # Assert that the scenarios to process are categorized scenarios.
  assert_that(any(scenarios %in% complete_scenario_list), msg = 'unrecognized scenarios')
  
  # Convert Inputs ------------------------------------------------------------------
  # The scenario inputs are provided by different sources IIASA, RCMIP and so on. So 
  # each set of scenarios must be processed with different rules. This series of if else 
  # statements processes the scenario inputs to match Hector inputs based on their source.
  if(any(scenarios %in% ssp_scenarios_rcmip)){
    
    # Subset the scenarios to process in this section & generate the ssp 
    # input data tables. 
    to_process     <- intersect(ssp_scenarios_rcmip, scenarios)
    ssp_emiss_conc <- convert_ssp_hector(scenario = to_process, years = years)
    ssp_rf         <- prepare_ssp_rf(scenario = to_process, years = years)
    
    # Combine the ssp emissions, concentrations, and radiative forcing.  
    hector_inputs <- rbind(hector_inputs, rbind(ssp_emiss_conc, ssp_rf))
    
  } else if(any(scenarios %in% rcp_scenarios)){
    
    # Subset the scenarios to process in this section & generate the rcp 
    # input data tables.  
    to_process     <- intersect(rcp_scenarios, scenarios)
    rcp_emiss_conc <- convert_rcp_hector(scenario = to_process, years = years)
    rcp_rf         <- prepare_rcp_rf(scenario = to_process, years = years)
    
    # Combine the rcp emissions, concentrations, and radiative forcing.  
    hector_inputs <- rbind(hector_inputs, rbind(rcp_emiss_conc, rcp_rf))
    
  } else {
    
    stop('The ability to process non rcmip cmip6 scenarios has not been added yet. \n
         See https://github.com/JGCRI/hectordata/issues/8')
    
  }
  
  # Interpolate the data over the missing years. 
  complete_data <- complete_missing_years(hector_inputs, expected_years = years)
  
  # Format hector inputs so that negative carbon emissions are properly 
  # categorized into daccs and land uptake. 
  final_data <- process_carbon_cycle_emissions(complete_data) 
    
  # Format and save the emissions and concentration constraints in the csv files 
  # in the proper Hector table input file. 
  split(final_data, final_data$scenario) %>%  
    sapply(write_hector_csv, write_to = write_to, USE.NAMES = FALSE) -> 
    files 
  
  # Copy over the volcanic RF to create the full 
  emissions_dir <- unique(dirname(files))
  
  # Generate the ini files corresponding to the new csv files. 
  inis <- make_new_ini(files)
  
  return(inis)
  
}
