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
  
  # Convert Inputs ------------------------------------------------------------------
  if(any(scenarios %in% rcmipCMIP6_scenario)){
    # Convert the CMIP phase 6 specfic scenarios.
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
  
  # TODO how robust is this? if there are no constraints or emissions will this throw an error?
  # Split up the emissions and concentrations by scenario and save output. 
  emiss_files <-  lapply(split(emiss_data, emiss_data$scenario), 
                         function(x){
                           scn   <- unique(x[['scenario']])
                           fname <- file.path(output_dir, paste0('emission_', scn, '.csv'))
                           save_output(x, fname)
                           fname})
  
  const_files <- lapply(split(const_data, const_data$scenario), 
                        function(x){
                          scn   <- unique(x[['scenario']])
                          fname <- file.path(output_dir, paste0('constraint_', scn, '.csv'))
                          save_output(x, fname)
                          fname})
    
 files <- c(unlist(emiss_files), unlist(const_files), use.names = FALSE)
 return(files)
  
}


#' Generate the volcanic input from the Hector repository 
#' 
#' During the historical period Hector perscribed radiative forcing for volcanic eruptions. 
#' Use this function to copy and format the volcanic radiative forcing inputs from the Hector 
#' repository into the inputs directory. 
#' 
#'
#' @param scenarios a character vector of scenario names. 
#' @param output_dir a character of the full path to the directory location for the user to write the generated csv files to. 
#' @return the location of the generated csv files.  
#' @export
generate_volanic_inputs <- function(output_dir){
  
  assertthat::assert_that(dir.exists(output_dir))
  
  # Copy over the volcanic rf input from the hector repository since they are required for all scenarios during 
  # the historical period. 
  # TODO 
  # 1. is this function necessary? 
  # 2. Would it be better to inlucde it in something like generate_input_tables which generates 
  # all of the input tables? Or should this be fexlible so that users can decide what volcanic RF is being used? 
  # 3. Do we want to read this directly from the hector repo or should it pull from zenodo?  
  vol_rf <- readLines(url('https://raw.githubusercontent.com/JGCRI/hector/master/inst/input/emissions/volcanic_RF.csv'))
  f1 <- file.path(output_dir, 'volcanic_RF.csv')
  writeLines(new_lines, f1)
  
  return(f1)
  
}