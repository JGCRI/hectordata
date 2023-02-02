#' Process the inputs materials from RCMIP into the appropriate units for Hector
#'
#' @param scenarios_to_process string vector of the scenario names to process
#' @import data.table
#' @return data frame of Hector inputs for multiple scenarios 
#' @export
process_rcmip_data <- function(scenarios_to_process=NULL){
  
  Scenario <- Region <- hector_variable <- hector_unit <- NULL
  
  rcmip_dir <- find_input_dir(dir = ZENODO_DIR, "rcmip")
  assertthat::assert_that(dir.exists(rcmip_dir), msg = "data dir not found")
  
  # Import the concentrations, emissions, and radiative forcing 
  # files and subset to include only the data to process.
  data_list <- lapply(X = list.files(rcmip_dir, full.names = TRUE, pattern = "means"),
                      FUN = utils::read.csv)
  all_data <- rbindlist(data_list, fill = TRUE)
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434",
                              "ssp460", "ssp534-over", "ssp585", "rcp26", "rcp45", "rcp60", "rcp85") 
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
  input_conversion_table <- stats::na.omit(long_inputs[hectordata::conversion_table$rcmip, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
  
  # Convert the value column from RCMIP units to Hector units.
  # This step may take a while depending on the number of scenarios being
  # processed.
  new_values <- unlist(mapply(ud_convert2,
                              x = hectordata::input_conversion_table$value,
                              from = hectordata::input_conversion_table$rcmip_udunits,
                              to = hectordata::input_conversion_table$hector_udunits,
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
  utils::write.csv(x = final_data, file = ofile, row.names = FALSE)
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
  
  # Silence global variable
  scenario <- NULL 
  
  hinput_data <- read_intermediate_data(depends_on)
  
  # TODO expand on the types of scenario file that can be generated as needed
  multi_forcing_scns <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434",
                          "ssp460", "ssp534-over", "ssp585", "rcp26", "rcp45", "rcp60", "rcp85")  
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- hinput_data$scenario
  } else {
    missing_code <- setdiff(scenarios_to_process, multi_forcing_scns)
    assertthat::assert_that(length(missing_code) == 0, msg = paste("missing code for: ", paste0(missing_code, collapse = ", ")))
    }
  
  # Check to see if missing data 
  existing_scns <- unique(hinput_data$scenario)
  missing <- setdiff(scenarios_to_process, existing_scns)
  assertthat::assert_that(length(missing) == 0, msg = paste("missing data for: ", paste0(missing, collapse = ", ")))

  
  # Process the future multi-forcing driven scenarios, use these to 
  # construct both emission and concentration driven runs. 
  multi_forcing_scn <- intersect(scenarios_to_process, multi_forcing_scns)
  if(length(multi_forcing_scn) >= 1){
    data <- hinput_data[scenario %in% multi_forcing_scn, ]
    
    # Format and save the emissions and concentration constraints in the csv files
    # in the proper Hector table input file. 
    data_list <- split(data, data$scenario)
    for(dat in data_list){
      info_source <- "rcmip"
      end_tag <- "_emiss-constraints_rf"
      
      ofile <- write_hector_csv(dat, 
                                required = c(REQUIRED_EMISSIONS, REQUIRED_RF, WM_GHG_CONSTRAINTS), 
                                info_source = info_source, 
                                write_to = TABLES_DIR, 
                                end_tag = end_tag)
      # Make emission driven ini files the RCMIP phase one submission 
      # uses esm- as a prefix. 
      inis <- make_new_ini(ofile, iniprefix_ = "esm-")
      
      # Construct the concentration driven ini file, here we will need to activate all of the 
      # well mixed ghg concentration constraints 
      scn <- unique(dat$scenario)
      new_path <- file.path('tables', basename(ofile))
      new_ini <- replace_csv_string(hectordata::template_ini, replacement_path = new_path, run_name = scn)
      new_ini <- activate_input_variables(lines = new_ini, vars = WM_GHG_CONSTRAINTS)
      
      write_to <- gsub(pattern = "/tables", x = dirname(ofile), replacement = "")
      name <- paste0(info_source, "_", scn)
      ini_path <- file.path(write_to, paste0(name, ".ini"))
      writeLines(new_ini, ini_path)
      
    }
    
  }
  
}

