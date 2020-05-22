## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

#' Generate emission and concentration constraint input tables for Hector. 
#'
#' @param scenarios a character vector of scenario names. 
#' @param output_dir a character of the full path to the directory location for the user to write the generated csv files to. 
#' @return the location of the generated csv files.  
#' @export
generate_input_tables <- function(scenarios, output_dir){
  
  # Check to make sure that the output directory exstits. 
  assertthat::assert_that(dir.exists(output_dir))
  
  # Create an empty hector_inputs data table. 
  hector_inputs <- data.table::data.table()
  
  # A list of scenarios in each data source. 
  rcmipCMIP6_scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")
  
  # Assert that the scenarios to process are categorized scenarios. 
  assertthat::assert_that(any(scenarios %in% c(rcmipCMIP6_scenario)), msg = 'unrecognized scenarios')
  
  # Convert the CMIP phase 6 specfic scenarios.
  if(any(scenarios %in% rcmipCMIP6_scenario)){
    to_process    <- rcmipCMIP6_scenario[rcmipCMIP6_scenario %in% scenarios]
    hector_inputs <- rbind(hector_inputs, convert_rcmipCMIP6_hector(to_process))
  }
  
  
  # Split up the scenario emissions and concentration constraints. 
  vars <- unique(hector_inputs$variable)
  emiss_data <- hector_inputs[variable %in% vars[grepl(pattern = 'emissions', x = vars)], ]
  const_data <- hector_inputs[variable %in% vars[grepl(pattern = 'constrain', x = vars)], ]
  
  # Set up the directory to save the 
  input_dir <- file.path(output_dir, 'inputs')
  dir.create(input_dir, showWarnings = FALSE)
  
  # Convert emission and concentration files and save output. 
  files <- unlist(c(split(emiss_data, emiss_data$scenario) %>% 
                      lapply(function(x){
                        scn   <- unique(x[['scenario']])
                        fname <- file.path(output_dir, paste0('emission_', scn, '.csv'))
                        save_output(x, fname)
                        fname}), 
                    split(const_data, const_data$scenario) %>% 
                      lapply(function(x){
                        scn   <- unique(x[['scenario']])
                        fname <- file.path(output_dir, paste0('constraint_', scn, '.csv'))
                        save_output(x, fname)
                        fname}), use.names = FALSE))
  
  invisible(files)
  
}

