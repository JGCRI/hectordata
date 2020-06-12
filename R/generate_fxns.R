## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files


#' Generate emission and concentration constraint input tables for Hector.
#'
#' @param scenarios a character vector of scenario names.
#' @param output_dir a character of the full path to the directory location for
#'  the user to write the generated csv files to.
#' @param years a vector of the expected output years. 
#' @return the location of the generated csv files.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>% 
generate_input_tables <- function(scenarios, output_dir, years){

  # Silence package checks  
  emiss_conc <- variable <- na.omit <- scenario <- value <- NULL
  
  # Check to make sure that the output directory exstits.
  assert_that(dir.exists(output_dir))
  assert_that(is.numeric(years))

  # Create an empty hector_inputs data table.
  hector_inputs <- data.table::data.table()

  # A list of scenarios in each data source.
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
  
  # Add identifier information about if a variable is an emission or a constraint. 
  hector_inputs[ , emiss_conc := ifelse(grepl(pattern = 'emission', x = variable), 'emission', NA_character_)]
  hector_inputs[ , emiss_conc := ifelse(grepl(pattern = 'constrain', x = variable), 'constraint', emiss_conc)]
  hector_inputs <- na.omit(hector_inputs)
  
  
  
  # Format and Save ------------------------------------------------------------------
  # Save the Hector input tables. 
  # Set up the directory to save them.
  input_dir <- file.path(output_dir, 'inputs')
  dir.create(input_dir, showWarnings = FALSE)

  # Save the the tables in the proper Hector table format. 
  split(hector_inputs, interaction(hector_inputs$emiss_conc, hector_inputs$scenario, drop = TRUE)) %>%  
          purrr::map(.f = function(x){
            
            # Save the file name 
            scn   <- unique(x[['scenario']])
            id     <- unique(x[['emiss_conc']])
            fname <- file.path(output_dir, paste0(id, '_', scn, '.csv'))
            
            # Format and save the output table. 
            save_hector_table(x[ , list(scenario, year, variable, units, value)], fname)
            return(fname)

          }) %>%  
    unlist(use.names = FALSE) -> 
    files 

  return(files)

}
