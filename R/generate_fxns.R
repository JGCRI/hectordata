## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files


#' Generate emission and concentration constraint input tables for Hector.
#'
#' @param scenarios a character vector of scenario names.
#' @param output_dir a character of the full path to the directory location for
#'  the user to write the generated csv files to.
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
    
    to_process <- intersect(rcmipCMIP6_scenario, scenarios)
    processed  <- convert_rcmipCMIP6_hector(to_process)
    hector_inputs <- rbind(hector_inputs, processed)
    
  } # TODO expand to process other scenarios here such as the DECK and CMIP5 scenarios.


  # Split up the scenario emissions and concentration constraints.
  vars <- unique(hector_inputs$variable)
  emiss_data <- hector_inputs[variable %in% vars[grepl(pattern = 'emissions', x = vars)], ]
  const_data <- hector_inputs[variable %in% vars[grepl(pattern = 'constrain', x = vars)], ]

  # Set up the directory to save the
  input_dir <- file.path(output_dir, 'inputs')
  dir.create(input_dir, showWarnings = FALSE)

  # TODO 1. how robust is this? if there are no constraints or emissions will this throw an error?
  # 2. Should the user be able to define where these files are saved to or should they save
  # to emissions and constraint sub directories like they were hector repository?
  # Split up the emissions and concentrations by scenario and save output.
  emiss_files <-  lapply(split(emiss_data, emiss_data$scenario),
                         function(x){
                           scn   <- unique(x[['scenario']])
                           fname <- file.path(output_dir, paste0('emission_', scn, '.csv'))
                           save_hector_table(x, fname)
                           fname})

  const_files <- lapply(split(const_data, const_data$scenario),
                        function(x){
                          scn   <- unique(x[['scenario']])
                          fname <- file.path(output_dir, paste0('constraint_', scn, '.csv'))
                          save_hector_table(x, fname)
                          fname})

  files <- c(unlist(emiss_files), unlist(const_files), use.names = FALSE)
  return(files)

}
